{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration: bring up the proxy if the pairing needs one, run the
-- agent attached to it, and tear everything down again.
--
-- This is the only place where "ProxyTool.Launch" and "ProxyTool.Proxy" meet.
-- Keeping it separate means the TUI and the command line share exactly one
-- implementation of what it means to start a session.
module ProxyTool.Session
  ( SessionOutcome (..)
  , runSession
  , previewSession
  , translateOptsFor
    -- * Standing proxy
  , serveSession
  , tokenFromEnv
  , stopServe
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Clock (getMonotonicTime)
import qualified Network.HTTP.Client as H
import Network.HTTP.Types (statusCode)
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.Process (getCurrentPid)

import ProxyTool.Config
import ProxyTool.Launch
import ProxyTool.Proxy
import ProxyTool.I18n
import ProxyTool.ServeRoutes (ServeInfo (..))
import ProxyTool.Translate (TranslateOpts (..))
import ProxyTool.Types

data SessionOutcome = SessionOutcome
  { soPlan     :: LaunchPlan
  , soResolved :: ResolvedLaunch
  , soPort     :: Maybe Int
  , soExit     :: ExitCode
  }

-- | What the proxy should impose, derived from the provider.
--
-- An empty model means \"leave whatever the agent asked for alone\", which is
-- different from forcing the empty string.
translateOptsFor :: Provider -> TranslateOpts
translateOptsFor p = TranslateOpts
  { optModel     = nonEmpty (provModel p)
  , optFastModel = provFastModel p >>= nonEmpty
  , optEffort    = provEffort p
  , optExtraBody = provExtraBody p
  }
  where
    nonEmpty t = if T.null (T.strip t) then Nothing else Just t

-- | Render what a launch would do, without doing any of it.
previewSession :: Config -> Agent -> Provider -> Either Msg [PreviewLine]
previewSession cfg agent provider = do
  plan <- planLaunch agent provider (cfgRoute cfg)
  let endpoint = previewEndpoint plan (cfgPort cfg)
      -- The preview must never show a real credential, so it is rendered with
      -- a placeholder in place of the session token or provider key.
      resolved = resolveLaunch plan endpoint "<session-token>"
  pure (describeLaunch (cfgLang cfg) plan resolved)

-- | Run a full session and wait for the agent to exit.
runSession
  :: (Text -> IO ())
  -> Config
  -> Agent
  -> Provider
  -> IO (Either Msg SessionOutcome)
runSession logLine cfg agent provider =
  case planLaunch agent provider (cfgRoute cfg) of
    Left err -> pure (Left err)
    Right plan -> resolveSecret (provKey provider) >>= \case
      Left err -> pure (Left err)
      Right key
        | lpUseProxy plan -> withProxy plan key
        | otherwise       -> run plan Nothing (stripSlash (provBaseUrl provider)) key
  where
    stripSlash = T.dropWhileEnd (== '/')

    withProxy plan key = do
      token <- newSessionToken
      let pc = ProxyConfig
            { pcUpstreamBase = stripSlash (provBaseUrl provider)
            , pcUpstreamWire = provWire provider
            , pcClientWire   = agWire agent
            , pcApiKey       = key
            , pcHeaders      = provHeaders provider
            , pcOpts         = translateOptsFor provider
            , pcToken        = token
            , pcLog          = logLine
            , pcLogBodies    = cfgLogBodies cfg
            , pcLang         = cfgLang cfg
            , pcServe        = Nothing
            , pcRequestStop  = pure ()
            }
      startProxy pc (cfgPort cfg) >>= \case
        Left err -> pure (Left err)
        Right h -> bracket (pure h) stopProxy $ \handle -> do
          let endpoint = "http://127.0.0.1:" <> T.pack (show (phPort handle))
          let say = render (cfgLang cfg)
              mode = if lpTranslate plan
                       then say (MsgProxyModeTranslate (wireLabel (agWire agent))
                                                       (wireLabel (provWire provider)))
                       else say MsgProxyModeRewrite
          logLine $ say (MsgProxyListening endpoint
                                           (stripSlash (provBaseUrl provider)))
                 <> " (" <> mode <> ")"
          -- The agent is handed the session token, not the provider key: the
          -- credential stays inside this process.
          run plan (Just (phPort handle)) endpoint token

    run plan mport endpoint agentKey = do
      let resolved = resolveLaunch plan endpoint agentKey
      mapM_ (\w -> logLine (render (cfgLang cfg) (MsgLblWarning)
                            <> ": " <> render (cfgLang cfg) w)) (lpWarnings plan)
      executeAgent resolved >>= \case
        Left err   -> pure (Left err)
        Right code -> pure (Right (SessionOutcome plan resolved mport code))

-- ---------------------------------------------------------------------------
-- Standing proxy
-- ---------------------------------------------------------------------------

-- | The proxy token a standing proxy demands, read from the named
-- environment variable.
--
-- Unset and empty are both refusals: a standing proxy without a token would
-- be an open relay to the user's paid provider for every process on the
-- machine.  The value is never rendered -- the message names the variable.
tokenFromEnv :: Text -> IO (Either Msg Text)
tokenFromEnv name = lookupEnv (T.unpack name) >>= \case
  Just v | not (null v) -> pure (Right (T.pack v))
  _                     -> pure (Left (MsgEnvVarNotSet name))

-- | Run the proxy for one provider with no agent attached, until
-- @waitForStop@ returns (@proxy-tool serve@).
--
-- The client dialect is Anthropic Messages -- the standing proxy exists so a
-- Claude Code that somebody else launches can reach an OpenAI-compatible
-- provider.  The provider's own credential stays inside this process, as in
-- a launched session; the client presents @token@.  @port@ 0 asks the OS for
-- a free port (the test suite does); @onReady@ learns the port actually
-- bound before @waitForStop@ is entered.  @requestStop@ is what an
-- authorised @POST \/admin\/shutdown@ runs: it must make @waitForStop@
-- return (the command line fills the same MVar its signal handlers do).
serveSession
  :: (Text -> IO ())
  -> Config
  -> Provider
  -> Int
  -> Text
  -> (Int -> IO ())
  -> IO ()
  -> IO ()
  -> IO (Either Msg ())
serveSession logLine cfg provider port token onReady requestStop waitForStop
  | T.null (T.strip (provBaseUrl provider)) =
      pure (Left (MsgProviderNoBaseUrl (provId provider)))
  | otherwise = resolveSecret (provKey provider) >>= \case
      Left err -> pure (Left err)
      Right key -> do
        started <- getMonotonicTime
        startedAt <- T.pack . iso8601Show <$> getCurrentTime
        pid <- fromIntegral <$> getCurrentPid
        let upstreamBase = T.dropWhileEnd (== '/') (provBaseUrl provider)
            opts = translateOptsFor provider
            identity boundPort = ServeInfo
              { siProvider  = provId provider
              , siPort      = boundPort
              , siStarted   = started
              , siModel     = optModel opts
              , siEffort    = effortSlug <$> optEffort opts
              , siBaseUrl   = upstreamBase
              , siStartedAt = startedAt
              , siPid       = pid
              }
            pc boundPort = ProxyConfig
              { pcUpstreamBase = upstreamBase
              , pcUpstreamWire = provWire provider
              , pcClientWire   = WireAnthropic
              , pcApiKey       = key
              , pcHeaders      = provHeaders provider
              , pcOpts         = opts
              , pcToken        = token
              , pcLog          = logLine
              , pcLogBodies    = cfgLogBodies cfg
              , pcLang         = cfgLang cfg
              , pcServe        = Just (identity boundPort)
              , pcRequestStop  = requestStop
              }
        -- The health answer reports the port actually bound, which for a
        -- requested port of 0 exists only once the socket does.
        startProxyWith pc (Just port) >>= \case
          Left err -> pure (Left err)
          Right h -> bracket (pure h) stopProxy $ \handle -> do
            let say = render (cfgLang cfg)
                endpoint = "http://127.0.0.1:" <> T.pack (show (phPort handle))
                mode = if provWire provider /= WireAnthropic
                         then say (MsgProxyModeTranslate (wireLabel WireAnthropic)
                                                         (wireLabel (provWire provider)))
                         else say MsgProxyModeRewrite
            logLine $ say (MsgProxyListening endpoint upstreamBase)
                   <> " (" <> mode <> ")"
            onReady (phPort handle)
            waitForStop
            logLine (say MsgServeStopped)
            pure (Right ())

-- | Stop the standing proxy on @port@ the way a supervisor without a console
-- can (@proxy-tool serve-stop@): an authorised @POST \/admin\/shutdown@, then
-- wait until the port no longer answers, so \"stopped\" is a fact and not a
-- hope.  Refusals are told apart: nothing listening, the token refused, any
-- other status, accepted but still answering after ten seconds.
stopServe :: Int -> Text -> IO (Either Msg ())
stopServe port token = do
  mgr <- H.newManager H.defaultManagerSettings
  base <- H.parseRequest ("http://127.0.0.1:" <> show port)
  -- Connection: close on every request, so no pooled keep-alive connection
  -- to the old process can answer the probes after its listener is gone.
  let close' = ("connection", "close")
      stopReq = base
        { H.method          = "POST"
        , H.path            = "/admin/shutdown"
        , H.requestHeaders  = [("authorization", "Bearer " <> TE.encodeUtf8 token), close']
        , H.responseTimeout = H.responseTimeoutMicro 5000000
        }
      probe = base
        { H.path            = "/healthz"
        , H.requestHeaders  = [close']
        , H.responseTimeout = H.responseTimeoutMicro 1000000
        }
      waitGone :: Int -> IO (Either Msg ())
      waitGone n = (try (H.httpNoBody probe mgr) :: IO (Either SomeException (H.Response ()))) >>= \case
        Left _ -> pure (Right ())
        Right _
          | n <= 0    -> pure (Left (MsgServeStopStillUp port))
          -- A poll interval, not a race guard: the exit is observable only
          -- from outside, as the port going quiet.
          | otherwise -> threadDelay 200000 >> waitGone (n - 1)
  (try (H.httpNoBody stopReq mgr) :: IO (Either SomeException (H.Response ()))) >>= \case
    Left _ -> pure (Left (MsgServeStopUnreachable port))
    Right res -> case statusCode (H.responseStatus res) of
      200 -> waitGone 50
      401 -> pure (Left (MsgServeStopRefused port))
      s   -> pure (Left (MsgServeStopFailed port s))
