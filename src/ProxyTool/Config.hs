{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | On-disk configuration: where it lives, what ships by default, and how it
-- is read and written.
--
-- The defaults are data, not code paths.  Every agent binding below is
-- editable in the TUI, which matters because CLI agents change their flags
-- between releases: when that happens the fix is an edit, not a rebuild.
module ProxyTool.Config
  ( Config (..)
  , defaultConfig
  , defaultAgents
  , defaultProviders
    -- * Paths
  , configFilePath
    -- * IO
  , loadConfig
  , loadConfigFrom
  , saveConfig
  , encodeConfig
    -- * Lookups and edits
  , findAgent
  , findProvider
  , upsertProvider
  , removeProvider
  , freshProviderId
  ) where

import Control.Exception (IOException, try)
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find)

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( XdgDirectory (XdgConfig)
  , createDirectoryIfMissing
  , doesFileExist
  , emptyPermissions
  , getXdgDirectory
  , setOwnerReadable
  , setOwnerWritable
  , setPermissions
  )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

import ProxyTool.I18n
import ProxyTool.Types

-- ---------------------------------------------------------------------------
-- The config record
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgAgents       :: [Agent]
  , cfgProviders    :: [Provider]
  , cfgRoute        :: RouteMode
  , cfgPort         :: Maybe Int
    -- ^ Fixed proxy port.  'Nothing' asks the OS for a free one, which is the
    -- better default: several sessions can then run side by side.
  , cfgLastAgent    :: Maybe Text
  , cfgLastProvider :: Maybe Text
  , cfgLogBodies    :: Bool
    -- ^ Log request/response bodies to stderr.  Off by default: bodies carry
    -- source code and prompts.
  , cfgLang         :: Lang
    -- ^ Persisted interface language.  English unless changed, and only
    -- changed by an explicit choice -- see "ProxyTool.I18n".
  } deriving (Eq, Show)

instance ToJSON Config where
  toJSON c = object
    [ "agents"        .= cfgAgents c
    , "providers"     .= cfgProviders c
    , "route"         .= cfgRoute c
    , "port"          .= cfgPort c
    , "last_agent"    .= cfgLastAgent c
    , "last_provider" .= cfgLastProvider c
    , "log_bodies"    .= cfgLogBodies c
    , "lang"          .= cfgLang c
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .:? "agents"        .!= defaultAgents
    <*> o .:? "providers"     .!= defaultProviders
    <*> o .:? "route"         .!= RouteAuto
    <*> o .:? "port"
    <*> o .:? "last_agent"
    <*> o .:? "last_provider"
    <*> o .:? "log_bodies"    .!= False
    <*> o .:? "lang"          .!= LangEn

defaultConfig :: Config
defaultConfig = Config
  { cfgAgents       = defaultAgents
  , cfgProviders    = defaultProviders
  , cfgRoute        = RouteAuto
  , cfgPort         = Nothing
  , cfgLastAgent    = Nothing
  , cfgLastProvider = Nothing
  , cfgLogBodies    = False
  , cfgLang         = LangEn
  }

-- ---------------------------------------------------------------------------
-- Shipped agents
-- ---------------------------------------------------------------------------

-- | The agents the tool knows about out of the box.
--
-- Provenance of the bindings, so that a future reader can tell fact from
-- guess:
--
--  * Claude Code: @ANTHROPIC_BASE_URL@, @ANTHROPIC_AUTH_TOKEN@,
--    @ANTHROPIC_API_KEY@, @ANTHROPIC_MODEL@, @ANTHROPIC_SMALL_FAST_MODEL@ and
--    @ANTHROPIC_DEFAULT_HAIKU_MODEL@ were all read out of the shipped
--    @claude@ binary.
--
--  * Codex: the flags @-c@ / @-m@ come from @codex --help@ (v0.153.3); the
--    config keys @model_provider@, @model_providers@, @base_url@, @env_key@,
--    @wire_api@ and @model_reasoning_effort@ were read out of the shipped
--    @codex@ binary.
--
--  * Kimi Code: not installed here, so its binding could not be checked
--    first-hand.  It is shipped pointing at the Anthropic-compatible shape
--    that Moonshot documents, and its notes say so.  Edit it in the TUI if
--    your build differs -- that is what the binding editor is for.
defaultAgents :: [Agent]
defaultAgents =
  [ Agent
      { agId      = "claude"
      , agLabel   = "Claude Code"
      , agCommand = "claude"
      , agArgs    = []
      , agWire    = WireAnthropic
      , agBinding = emptyBinding
          { bindBaseUrlEnv   = Just "ANTHROPIC_BASE_URL"
          , bindKeyEnv       = Just "ANTHROPIC_AUTH_TOKEN"
          , bindModelEnv     = Just "ANTHROPIC_MODEL"
          , bindFastModelEnv = Just "ANTHROPIC_SMALL_FAST_MODEL"
          , bindEnv =
              [ ("ANTHROPIC_API_KEY", "{key}")
              , ("ANTHROPIC_DEFAULT_HAIKU_MODEL", "{fast_model}")
              ]
          }
      , agNotes =
          "Auth token and API key are both bound, because which one an \
          \endpoint wants differs: ANTHROPIC_AUTH_TOKEN becomes a Bearer \
          \header, ANTHROPIC_API_KEY an x-api-key header. Every variable \
          \named here is cleared before launch, so a stale key in your shell \
          \is never inherited."
      }
  , Agent
      { agId      = "codex"
      , agLabel   = "Codex CLI"
      , agCommand = "codex"
      , agArgs    = []
      , agWire    = WireOpenAIChat
      , agBinding = emptyBinding
          { bindKeyEnv = Just "OPENAI_API_KEY"
          , bindArgs =
              [ "-c", "model_provider=proxytool"
              , "-c", "model_providers.proxytool.name=proxy-tool"
              , "-c", "model_providers.proxytool.base_url={base_url}"
              , "-c", "model_providers.proxytool.wire_api=chat"
              , "-c", "model_providers.proxytool.env_key=OPENAI_API_KEY"
              , "-m", "{model}"
              , "-c", "model_reasoning_effort={effort}"
              ]
          }
      , agNotes =
          "Codex prefers the OpenAI Responses API; this binding pins the \
          \generated provider to wire_api=chat, which is the dialect this \
          \proxy translates. The values are deliberately unquoted: codex \
          \parses a -c value as TOML and falls back to a raw string literal, \
          \so avoiding quotes keeps the arguments intact when a Windows .cmd \
          \shim has to be run through cmd.exe. An argument whose placeholder \
          \resolves to empty is dropped along with the flag in front of it, \
          \so leaving effort unset simply omits -c model_reasoning_effort."
      }
  , Agent
      { agId      = "kimi"
      , agLabel   = "Kimi Code"
      , agCommand = "kimi"
      , agArgs    = []
      , agWire    = WireAnthropic
      , agBinding = emptyBinding
          { bindBaseUrlEnv = Just "ANTHROPIC_BASE_URL"
          , bindKeyEnv     = Just "ANTHROPIC_AUTH_TOKEN"
          , bindModelEnv   = Just "ANTHROPIC_MODEL"
          }
      , agNotes =
          "UNVERIFIED: no Kimi CLI was present on the machine this config was \
          \generated on, so this binding is a starting point rather than a \
          \checked fact. If your build reads different variables, edit them \
          \here. Running Claude Code against the Moonshot provider is the \
          \path that was actually checked."
      }
  ]

-- ---------------------------------------------------------------------------
-- Shipped providers
-- ---------------------------------------------------------------------------

-- | Example endpoints.  No credentials: each names an environment variable.
--
-- Base URL convention, which the probes above confirmed: Anthropic-dialect
-- clients append @\/v1\/messages@ themselves, so their base URL stops before
-- the @\/v1@; OpenAI-dialect clients append @\/chat\/completions@, so theirs
-- includes it.
defaultProviders :: [Provider]
defaultProviders =
  [ emptyProvider
      { provId      = "anthropic"
      , provLabel   = "Anthropic (official)"
      , provWire    = WireAnthropic
      , provBaseUrl = "https://api.anthropic.com"
      , provKey     = SecretEnv "ANTHROPIC_API_KEY"
      , provModel   = "claude-opus-4-5"
      }
  , emptyProvider
      { provId      = "openai"
      , provLabel   = "OpenAI (official)"
      , provWire    = WireOpenAIChat
      , provBaseUrl = "https://api.openai.com/v1"
      , provKey     = SecretEnv "OPENAI_API_KEY"
      , provModel   = "gpt-5.1"
      }
  , emptyProvider
      { provId      = "moonshot-anthropic"
      , provLabel   = "Moonshot / Kimi (Anthropic-compatible)"
      , provWire    = WireAnthropic
      , provBaseUrl = "https://api.moonshot.ai/anthropic"
      , provKey     = SecretEnv "MOONSHOT_API_KEY"
      , provModel   = "kimi-k2-turbo-preview"
      }
  , emptyProvider
      { provId      = "moonshot-openai"
      , provLabel   = "Moonshot / Kimi (OpenAI-compatible)"
      , provWire    = WireOpenAIChat
      , provBaseUrl = "https://api.moonshot.ai/v1"
      , provKey     = SecretEnv "MOONSHOT_API_KEY"
      , provModel   = "kimi-k2-turbo-preview"
      }
  , emptyProvider
      { provId      = "local"
      , provLabel   = "Local OpenAI-compatible server"
      , provWire    = WireOpenAIChat
      , provBaseUrl = "http://127.0.0.1:11434/v1"
      , provKey     = SecretNone
      , provModel   = ""
      }
  ]

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | Where the config lives.
--
-- @PROXY_TOOL_CONFIG@ overrides the whole path, which is what the test suite
-- and any throwaway profile use.  Otherwise it is the XDG config directory
-- for this platform, so nothing machine-specific is compiled in.
configFilePath :: IO FilePath
configFilePath = lookupEnv "PROXY_TOOL_CONFIG" >>= \case
  Just p | not (null p) -> pure p
  _ -> do
    dir <- getXdgDirectory XdgConfig "proxy-tool"
    pure (dir </> "config.json")

-- ---------------------------------------------------------------------------
-- Reading and writing
-- ---------------------------------------------------------------------------

-- | Read the config from its usual location.
--
-- A missing file is not an error: the defaults are returned and nothing is
-- written, so a read-only run never creates state as a side effect.
loadConfig :: IO (Either Msg (FilePath, Config))
loadConfig = do
  path <- configFilePath
  fmap ((,) path) <$> loadConfigFrom path

loadConfigFrom :: FilePath -> IO (Either Msg Config)
loadConfigFrom path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right defaultConfig)
    else do
      raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      pure $ case raw of
        Left e -> Left (MsgCouldNotRead (T.pack path) (T.pack (show e)))
        Right bs -> case eitherDecodeStrict bs of
          Left err -> Left (MsgInvalidConfig (T.pack path) (T.pack err))
          Right c  -> Right c

-- | Serialise, with stable key order and two-space indentation, so that the
-- file stays reviewable in a diff.
encodeConfig :: Config -> BL.ByteString
encodeConfig = AP.encodePretty' AP.defConfig
  { AP.confIndent  = AP.Spaces 2
  , AP.confCompare = mempty
  , AP.confTrailingNewline = True
  }

-- | Write the config, creating the directory if needed.
--
-- The file may hold pasted API keys, so it is created owner-read/write only.
-- On POSIX that is a real 0600; on Windows the @directory@ package can only
-- express the read-only attribute, and the NTFS ACL inherited from the
-- profile directory is what actually restricts access there.
saveConfig :: FilePath -> Config -> IO (Either Msg ())
saveConfig path cfg = do
  r <- try act :: IO (Either IOException ())
  pure $ case r of
    Left e   -> Left (MsgCouldNotWrite (T.pack path) (T.pack (show e)))
    Right () -> Right ()
  where
    act = do
      createDirectoryIfMissing True (takeDirectory path)
      BL.writeFile path (encodeConfig cfg)
      setPermissions path
        (setOwnerReadable True (setOwnerWritable True emptyPermissions))

-- ---------------------------------------------------------------------------
-- Lookups and edits
-- ---------------------------------------------------------------------------

findAgent :: Text -> Config -> Maybe Agent
findAgent aid c = find ((== aid) . agId) (cfgAgents c)

findProvider :: Text -> Config -> Maybe Provider
findProvider pid c = find ((== pid) . provId) (cfgProviders c)

-- | Insert or replace a provider, keeping list position stable so the TUI
-- selection does not jump after an edit.
upsertProvider :: Provider -> Config -> Config
upsertProvider p c
  | any ((== provId p) . provId) (cfgProviders c) =
      c { cfgProviders = map swap (cfgProviders c) }
  | otherwise = c { cfgProviders = cfgProviders c <> [p] }
  where
    swap q | provId q == provId p = p
           | otherwise            = q

removeProvider :: Text -> Config -> Config
removeProvider pid c =
  c { cfgProviders = filter ((/= pid) . provId) (cfgProviders c) }

-- | An id not already taken, derived from a label.
freshProviderId :: Config -> Text -> Text
freshProviderId c base0 = pick candidates
  where
    slug = T.map dashify (T.toLower (T.strip base0))
    dashify ch
      | ch `elem` (" \t/\\:" :: String) = '-'
      | otherwise = ch
    base = if T.null slug then "provider" else slug
    candidates = base : [base <> "-" <> T.pack (show n) | n <- [(2 :: Int) ..]]
    taken cand = any ((== cand) . provId) (cfgProviders c)
    -- The candidate list is infinite and 'taken' is finite, so this always
    -- terminates; the fallback arm exists only to keep the match total.
    pick (cand : rest) = if taken cand then pick rest else cand
    pick []            = base
