{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Turning a (agent, provider, route) choice into an actual child process.
--
-- The planning half is pure and total, so the TUI can render exactly what is
-- about to happen -- command line, environment, endpoint, warnings -- before
-- anything is spawned, and so the same computation can be unit-tested.
module ProxyTool.Launch
  ( -- * Planning
    LaunchPlan (..)
  , ResolvedLaunch (..)
  , PreviewLine (..)
  , planLaunch
  , resolveLaunch
  , previewEndpoint
  , templateVars
  , renderArgList
  , describeLaunch
  , routeSummary
    -- * Secrets
  , resolveSecret
  , newSessionToken
    -- * Running
  , CommandResolution (..)
  , resolveCommand
  , executeAgent
  ) where

import Control.Monad (replicateM)
import Data.Char (toLower, toUpper)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isPathSeparator)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode)
import System.Process
  ( CreateProcess (delegate_ctlc, env)
  , createProcess
  , proc
  , waitForProcess
  )
import System.Random (randomRIO)

import ProxyTool.I18n
import ProxyTool.Types

-- ---------------------------------------------------------------------------
-- Planning
-- ---------------------------------------------------------------------------

-- | The decisions that do not depend on runtime facts (the actual port, the
-- actual key).
data LaunchPlan = LaunchPlan
  { lpAgent     :: Agent
  , lpProvider  :: Provider
  , lpRoute     :: RouteMode
    -- ^ The route as requested.
  , lpUseProxy  :: Bool
    -- ^ The route as decided.
  , lpTranslate :: Bool
    -- ^ Whether the proxy has to change dialect rather than just rewrite.
  , lpWarnings  :: [Msg]
  } deriving (Eq, Show)

-- | Everything needed to spawn the child, with the endpoint and key filled in.
data ResolvedLaunch = ResolvedLaunch
  { rlCommand  :: Text
  , rlArgs     :: [Text]
  , rlSetEnv   :: [(Text, Text)]
  , rlClearEnv :: [Text]
    -- ^ Variables the binding owns.  They are removed from the inherited
    -- environment before 'rlSetEnv' is applied, so the child can never pick
    -- up a stale value for a variable this tool is responsible for.  Without
    -- this, an @ANTHROPIC_API_KEY@ left over in the shell would be sent to
    -- whichever third-party endpoint the user just selected.
  , rlEndpoint :: Text
  } deriving (Eq, Show)

-- | Decide how to run this pairing, or explain why it cannot be run.
--
-- 'RouteAuto' proxies exactly when the agent and the provider speak different
-- dialects; when they agree, the agent's own environment variables are enough
-- and an extra hop would only add latency.
planLaunch :: Agent -> Provider -> RouteMode -> Either Msg LaunchPlan
planLaunch agent provider route
  | T.null (T.strip (provBaseUrl provider)) =
      Left (MsgProviderNoBaseUrl (provId provider))
  | useProxy && translate && agWire agent /= WireAnthropic =
      Left (MsgCannotBridge (wireLabel (agWire agent)) (agId agent)
                            (wireLabel (provWire provider)) (provId provider))
  | route == RouteDirect && wiresDiffer =
      Left (MsgDirectNeedsMatch (agId agent) (wireLabel (agWire agent))
                                (provId provider) (wireLabel (provWire provider)))
  | otherwise = Right LaunchPlan
      { lpAgent     = agent
      , lpProvider  = provider
      , lpRoute     = route
      , lpUseProxy  = useProxy
      , lpTranslate = translate
      , lpWarnings  = warnings
      }
  where
    wiresDiffer = agWire agent /= provWire provider
    useProxy = case route of
      RouteDirect -> False
      RouteProxy  -> True
      RouteAuto   -> wiresDiffer
    translate = useProxy && wiresDiffer

    warnings = catMaybes
      [ if T.null (T.strip (provModel provider))
          then Just MsgWarnNoModel
          else Nothing
      , case provKey provider of
          SecretNone | not (isLoopback (provBaseUrl provider)) ->
            Just MsgWarnUnauthenticated
          _ -> Nothing
      , case provEffort provider of
          Just e | e > EffortXHigh, provWire provider == WireOpenAIChat ->
            Just (MsgWarnEffortClamped (effortSlug e))
          _ -> Nothing
      , if not (T.null (agNotes agent)) && "UNVERIFIED" `T.isPrefixOf` agNotes agent
          then Just (MsgWarnUnverifiedBinding (agNotes agent))
          else Nothing
      ]

isLoopback :: Text -> Bool
isLoopback u =
  let l = T.toLower u
  in any (`T.isInfixOf` l) ["//127.0.0.1", "//localhost", "//[::1]", "//0.0.0.0"]

-- | The endpoint string to show before the proxy exists.
previewEndpoint :: LaunchPlan -> Maybe Int -> Text
previewEndpoint plan mport
  | lpUseProxy plan = "http://127.0.0.1:" <> maybe "<auto>" (T.pack . show) mport
  | otherwise       = stripTrailingSlash (provBaseUrl (lpProvider plan))

stripTrailingSlash :: Text -> Text
stripTrailingSlash = T.dropWhileEnd (== '/')

-- | The substitutions available to every binding template.
templateVars :: LaunchPlan -> Text -> Text -> [(Text, Text)]
templateVars plan endpoint key =
  [ ("base_url",        endpoint)
  , ("key",             key)
  , ("model",           provModel provider)
  , ("fast_model",      fromMaybe "" (provFastModel provider))
  , ("effort",          maybe "" effortSlug (provEffort provider))
  , ("effort_openai",   maybe "" effortOpenAIWire (provEffort provider))
  , ("thinking_budget", maybe "" (T.pack . show)
                          (provEffort provider >>= effortThinkingBudget))
  , ("provider",        provId provider)
  , ("provider_label",  provLabel provider)
  ]
  where provider = lpProvider plan

-- | Fill a plan in with the endpoint and key that are only known at run time.
resolveLaunch :: LaunchPlan -> Text -> Text -> ResolvedLaunch
resolveLaunch plan endpoint key = ResolvedLaunch
  { rlCommand  = agCommand agent
    -- Both argument lists are templates.  Treating only the binding's as
    -- expandable made "{effort}" in an agent's fixed arguments arrive at the
    -- process verbatim, which is a difference no field label explains.
  , rlArgs     = renderArgList vars (agArgs agent <> bindArgs binding)
  , rlSetEnv   = [(k, v) | (k, v) <- envPairs, not (T.null v)]
  , rlClearEnv = ownedNames
  , rlEndpoint = endpoint
  }
  where
    agent   = lpAgent plan
    binding = agBinding agent
    vars    = templateVars plan endpoint key

    envPairs = catMaybes
      [ (,) <$> bindBaseUrlEnv binding   <*> Just endpoint
      , (,) <$> bindKeyEnv binding       <*> Just key
      , (,) <$> bindModelEnv binding     <*> Just (provModel (lpProvider plan))
      , (,) <$> bindFastModelEnv binding
            <*> Just (fromMaybe "" (provFastModel (lpProvider plan)))
      ] <> [(k, renderTemplate vars v) | (k, v) <- bindEnv binding]

    ownedNames = catMaybes
      [ bindBaseUrlEnv binding
      , bindKeyEnv binding
      , bindModelEnv binding
      , bindFastModelEnv binding
      ] <> map fst (bindEnv binding)

-- | Render templated arguments, dropping the ones that came out empty.
--
-- The rule, in one sentence: an argument whose template referenced a
-- placeholder that resolved to the empty string is dropped, and so is the
-- bare flag immediately before it.  That is what turns
-- @["-c", "model_reasoning_effort={effort}"]@ into nothing at all when no
-- effort is configured, instead of into a syntactically valid setting with a
-- meaningless empty value.
renderArgList :: [(Text, Text)] -> [Text] -> [Text]
renderArgList vars = foldl step [] . map classify
  where
    classify tmpl
      | referencedEmpty = Nothing
      | otherwise       = Just (renderTemplate vars tmpl)
      where
        referencedEmpty = any
          (\(k, v) -> T.null v && ("{" <> k <> "}") `T.isInfixOf` tmpl)
          vars

    step acc (Just a) = acc <> [a]
    step acc Nothing  = dropTrailingFlag acc

    dropTrailingFlag acc = case reverse acc of
      (lastArg : rest) | isBareFlag lastArg -> reverse rest
      _                                     -> acc

    isBareFlag t = "-" `T.isPrefixOf` t && not ("=" `T.isInfixOf` t)

-- | One line of a launch preview.
--
-- The warning flag travels with the line rather than being recovered by
-- matching a prefix: once the label is translated, \"does this line start
-- with the word warning\" stops being a question with an answer.
data PreviewLine = PreviewLine
  { plWarning :: Bool
  , plText    :: Text
  } deriving (Eq, Show)

-- | What the resolved route actually does, as a message rather than a
-- sentence.  Callers that want only this one fact -- @doctor@ does -- ask for
-- it directly instead of fishing the line back out of a rendered preview,
-- which stopped being possible once the labels were translated.
routeSummary :: LaunchPlan -> Msg
routeSummary plan
  | not (lpUseProxy plan) = MsgRouteDirect
  | lpTranslate plan      = MsgRouteProxyTranslate (wireLabel (agWire agent))
                                                   (wireLabel (provWire prov))
  | otherwise             = MsgRouteProxyRewrite
  where
    agent = lpAgent plan
    prov  = lpProvider plan

-- | A human-readable summary, safe to print: the key never appears.
describeLaunch :: Lang -> LaunchPlan -> ResolvedLaunch -> [PreviewLine]
describeLaunch lang plan r =
  map plain
    [ field MsgLblAgent $ agLabel agent <> "  (" <> agId agent <> ", "
        <> say (MsgSpeaks (wireLabel (agWire agent))) <> ")"
    , field MsgLblProvider $ provLabel prov <> "  (" <> provId prov <> ", "
        <> say (MsgSpeaks (wireLabel (provWire prov))) <> ")"
    , field MsgLblRoute $ routeLabel (lpRoute plan) <> " -> " <> routeDesc
    , field MsgLblEndpoint  $ rlEndpoint r
    , field MsgLblUpstream  $ stripTrailingSlash (provBaseUrl prov)
    , field MsgLblModel     $ orDash (provModel prov)
    , field MsgLblFastModel $ orDash (fromMaybe "" (provFastModel prov))
    , field MsgLblEffort    $ maybe "-" effortSlug (provEffort prov)
    , field MsgLblKey       $ secretDisplay (provKey prov)
    , field MsgLblCommand   $ T.unwords (rlCommand r : map quoteArg (rlArgs r))
    ]
  <> [ plain (field MsgLblEnvUnset (T.intercalate ", " (rlClearEnv r)))
     | not (null (rlClearEnv r)) ]
  <> [ plain (field MsgLblEnvSet (k <> "=" <> redact k v)) | (k, v) <- rlSetEnv r ]
  <> [ PreviewLine True (field MsgLblWarning (say w)) | w <- lpWarnings plan ]
  where
    agent = lpAgent plan
    prov  = lpProvider plan
    say   = render lang
    plain = PreviewLine False

    -- Labels are padded by display width, so the colons still line up in a
    -- language whose characters are two columns wide.
    field m v = padWidth labelWidth (say m) <> " : " <> v
    labelWidth = maximum (map (textWidth . say) allLabels)
    allLabels =
      [ MsgLblAgent, MsgLblProvider, MsgLblRoute, MsgLblEndpoint, MsgLblUpstream
      , MsgLblModel, MsgLblFastModel, MsgLblEffort, MsgLblKey, MsgLblCommand
      , MsgLblEnvUnset, MsgLblEnvSet, MsgLblWarning ]

    routeDesc = say (routeSummary plan)

    orDash t = if T.null (T.strip t) then "-" else t
    quoteArg a = if T.any (== ' ') a then "\"" <> a <> "\"" else a
    -- Any variable whose name looks credential-shaped is shown masked, so a
    -- preview can be pasted into a bug report.
    redact k v
      | any (`T.isInfixOf` T.toUpper k) ["KEY", "TOKEN", "SECRET", "PASSWORD"] =
          secretDisplay (SecretLiteral v)
      | otherwise = v

-- ---------------------------------------------------------------------------
-- Secrets
-- ---------------------------------------------------------------------------

-- | Resolve a key reference to its value, or explain why it is unavailable.
resolveSecret :: SecretRef -> IO (Either Msg Text)
resolveSecret = \case
  SecretNone      -> pure (Right "")
  SecretLiteral v -> pure (Right v)
  SecretEnv name  -> lookupEnv (T.unpack name) >>= \case
    Just v | not (null v) -> pure (Right (T.pack v))
    _ -> pure (Left (MsgEnvVarNotSet name))

-- | A per-session bearer token for the local proxy.
--
-- The proxy listens on loopback only, but that still leaves it usable by
-- every other process on the machine, and it forwards with the user's real
-- credentials attached.  The agent is handed this token and the proxy
-- rejects anything else.  It is not a long-lived secret: it exists for the
-- lifetime of one launch.
newSessionToken :: IO Text
newSessionToken = T.pack <$> replicateM 40 pick
  where
    alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    pick = do
      i <- randomRIO (0, length alphabet - 1)
      pure (alphabet !! i)

-- ---------------------------------------------------------------------------
-- Running
-- ---------------------------------------------------------------------------

-- | How a command name was resolved to something 'createProcess' can run.
data CommandResolution = CommandResolution
  { crExecutable :: FilePath
  , crPrefixArgs :: [String]
    -- ^ Arguments that must precede the agent's own.
  , crVia        :: Text
    -- ^ How it was found, for the error and preview text.
  } deriving (Eq, Show)

-- | Find the executable for a command name.
--
-- This is more work than it looks, and all of it is Windows.  Two separate
-- gaps have to be closed, and most CLI agents fall into both because they
-- ship as npm packages:
--
--  1. @System.Directory.findExecutable@ appends only @.exe@, so a @.cmd@
--     shim is invisible to it -- verified here: @findExecutable \"codex\"@
--     returns 'Nothing' while @findExecutable \"codex.cmd\"@ finds it.  So
--     @PATHEXT@ is walked explicitly.
--
--  2. @CreateProcess@ cannot execute a batch file at all, so one found that
--     way has to be run through @cmd.exe \/c@.
--
-- On a Unix host @PATHEXT@ is unset, the extension list is empty and this
-- reduces to the plain 'findExecutable' it started as.
--
-- A command containing a path separator is taken as a path and checked
-- directly, since 'findExecutable' only ever searches @PATH@.
resolveCommand :: Text -> IO (Either Msg CommandResolution)
resolveCommand name = do
  exts <- pathExtensions
  let base       = T.unpack name
      candidates = base : [base <> e | e <- exts]
      look       = if any isPathSeparator base then existingFile else findExecutable
  found <- firstJustM look candidates
  pure $ case found of
    Nothing -> Left (MsgCommandNotFound name)
    Just path
      | isBatch path -> Right CommandResolution
          { crExecutable = "cmd.exe"
          , crPrefixArgs = ["/c", path]
          , crVia        = "cmd.exe /c " <> T.pack path
          }
      | otherwise -> Right CommandResolution
          { crExecutable = path
          , crPrefixArgs = []
          , crVia        = T.pack path
          }
  where
    isBatch p = any (`isSuffixOf` map toLower p) [".cmd", ".bat"]

    existingFile p = do
      ok <- doesFileExist p
      pure (if ok then Just p else Nothing)

-- | Executable suffixes to try, from @PATHEXT@.  Empty off Windows.
pathExtensions :: IO [String]
pathExtensions = do
  raw <- lookupEnv "PATHEXT"
  pure $ case raw of
    Nothing -> []
    Just v  -> [ T.unpack e
               | e <- T.splitOn ";" (T.pack v)
               , not (T.null (T.strip e)) ]

firstJustM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
firstJustM _ []       = pure Nothing
firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)

-- | Spawn the agent with the computed environment and wait for it.
--
-- stdin, stdout and stderr are inherited, so the agent owns the terminal and
-- behaves exactly as if it had been started by hand.  @delegate_ctlc@ hands
-- Ctrl-C to the child rather than killing this process out from under it.
executeAgent :: ResolvedLaunch -> IO (Either Msg ExitCode)
executeAgent r = resolveCommand (rlCommand r) >>= \case
  Left err -> pure (Left err)
  Right res -> do
    parent <- getEnvironment
    let cleared = [ (k, v) | (k, v) <- parent, not (owned k) ]
        childEnv = cleared
                <> [ (T.unpack k, T.unpack v) | (k, v) <- rlSetEnv r ]
        args = crPrefixArgs res <> map T.unpack (rlArgs r)
    (_, _, _, ph) <- createProcess (proc (crExecutable res) args)
      { env = Just childEnv
      , delegate_ctlc = True
      }
    Right <$> waitForProcess ph
  where
    -- Windows environment variable names are case-insensitive, so the
    -- comparison is too; binding names are uppercase by convention anyway.
    ownedUpper = map (T.unpack . T.toUpper) (rlClearEnv r)
    owned k = map toUpper k `elem` ownedUpper
