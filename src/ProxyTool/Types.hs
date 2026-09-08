{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core vocabulary shared by the config layer, the launcher, the proxy and
-- the TUI.
--
-- Everything the user can steer -- provider, base URL, model, effort, and the
-- environment/argument binding used to point an agent at an endpoint -- is
-- plain data here, so that it can be edited in the TUI and round-tripped
-- through the on-disk config.  No vendor knowledge is baked into control flow.
module ProxyTool.Types
  ( -- * Wire protocols
    Wire (..)
  , wireLabel
  , wireSlug
  , parseWire
    -- * Reasoning effort
  , Effort (..)
  , effortSlug
  , parseEffort
  , allEfforts
  , effortOpenAIWire
  , effortThinkingBudget
    -- * Secrets
  , SecretRef (..)
  , secretDisplay
    -- * Providers
  , Provider (..)
  , emptyProvider
    -- * Agents
  , Binding (..)
  , emptyBinding
  , Agent (..)
    -- * Routing
  , RouteMode (..)
  , routeLabel
  , routeSlug
  , parseRoute
    -- * Templates
  , renderTemplate
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Wire protocols
-- ---------------------------------------------------------------------------

-- | The HTTP dialect a participant speaks.
--
-- Only the two dialects the translator actually implements are represented.
-- Codex natively prefers the OpenAI /Responses/ API, but it accepts a
-- per-provider @wire_api = "chat"@ (key verified present in the shipped codex
-- binary), so routing it through this proxy means putting it on
-- 'WireOpenAIChat'.
data Wire
  = WireAnthropic   -- ^ Anthropic @\/v1\/messages@.
  | WireOpenAIChat  -- ^ OpenAI @\/v1\/chat\/completions@.
  deriving (Eq, Show, Enum, Bounded)

wireLabel :: Wire -> Text
wireLabel = \case
  WireAnthropic  -> "Anthropic Messages"
  WireOpenAIChat -> "OpenAI Chat Completions"

wireSlug :: Wire -> Text
wireSlug = \case
  WireAnthropic  -> "anthropic"
  WireOpenAIChat -> "openai-chat"

parseWire :: Text -> Maybe Wire
parseWire t = case T.toLower (T.strip t) of
  "anthropic"          -> Just WireAnthropic
  "anthropic-messages" -> Just WireAnthropic
  "openai-chat"        -> Just WireOpenAIChat
  "openai"             -> Just WireOpenAIChat
  "chat"               -> Just WireOpenAIChat
  _                    -> Nothing

instance ToJSON Wire where
  toJSON = String . wireSlug

instance FromJSON Wire where
  parseJSON = withText "Wire" $ \t ->
    maybe (fail $ "unknown wire protocol: " <> T.unpack t) pure (parseWire t)

-- ---------------------------------------------------------------------------
-- Effort
-- ---------------------------------------------------------------------------

-- | Reasoning effort.
--
-- The set is the union of what the two ecosystems accept.  OpenAI's public
-- @reasoning_effort@ enum is @none | minimal | low | medium | high | xhigh@
-- (verified against openai-python 2.30.0,
-- @types\/shared\/reasoning_effort.py@); the Codex CLI additionally offers
-- @max@ and @ultra@ in its embedded model catalogue.  'effortOpenAIWire'
-- clamps when a value has to go out on the OpenAI wire.
data Effort
  = EffortNone
  | EffortMinimal
  | EffortLow
  | EffortMedium
  | EffortHigh
  | EffortXHigh
  | EffortMax
  | EffortUltra
  deriving (Eq, Ord, Show, Enum, Bounded)

allEfforts :: [Effort]
allEfforts = [minBound .. maxBound]

effortSlug :: Effort -> Text
effortSlug = \case
  EffortNone    -> "none"
  EffortMinimal -> "minimal"
  EffortLow     -> "low"
  EffortMedium  -> "medium"
  EffortHigh    -> "high"
  EffortXHigh   -> "xhigh"
  EffortMax     -> "max"
  EffortUltra   -> "ultra"

parseEffort :: Text -> Maybe Effort
parseEffort t =
  lookup (T.toLower (T.strip t)) [(effortSlug e, e) | e <- allEfforts]

-- | The value to place in an OpenAI @reasoning_effort@ field.
--
-- @max@ and @ultra@ are Codex-only levels; the documented OpenAI enum tops
-- out at @xhigh@, so they are clamped rather than sent through to be
-- rejected.  The clamp is surfaced in the launch preview.
effortOpenAIWire :: Effort -> Text
effortOpenAIWire e
  | e >= EffortXHigh = "xhigh"
  | otherwise        = effortSlug e

-- | Effort expressed as an Anthropic extended-thinking budget, in tokens.
--
-- There is no vendor-defined correspondence between the two notions, so this
-- table is a deliberate, documented choice rather than a discovered fact.
-- 'Nothing' means \"do not enable extended thinking at all\".  The Anthropic
-- API requires @budget_tokens >= 1024@ and @budget_tokens < max_tokens@; the
-- @max_tokens@ half of that is enforced in "ProxyTool.Translate".
effortThinkingBudget :: Effort -> Maybe Int
effortThinkingBudget = \case
  EffortNone    -> Nothing
  EffortMinimal -> Just 1024
  EffortLow     -> Just 4096
  EffortMedium  -> Just 12288
  EffortHigh    -> Just 24576
  EffortXHigh   -> Just 32768
  EffortMax     -> Just 48000
  EffortUltra   -> Just 63000

instance ToJSON Effort where
  toJSON = String . effortSlug

instance FromJSON Effort where
  parseJSON = withText "Effort" $ \t ->
    maybe (fail $ "unknown effort: " <> T.unpack t) pure (parseEffort t)

-- ---------------------------------------------------------------------------
-- Secrets
-- ---------------------------------------------------------------------------

-- | How to obtain the upstream API key.
--
-- 'SecretEnv' is the recommended form: the config file then names a variable
-- instead of carrying the credential.  'SecretLiteral' exists because people
-- do paste keys into a TUI; it lands in the config file, which is created
-- with owner-only permissions, and is never rendered unmasked.
data SecretRef
  = SecretNone
  | SecretEnv Text
  | SecretLiteral Text
  deriving (Eq, Show)

-- | Human-readable, non-leaking rendering of a 'SecretRef'.
secretDisplay :: SecretRef -> Text
secretDisplay = \case
  -- A dash rather than a word: this string is rendered inside launch previews
  -- in every interface language, and it names an absence, not a sentence.
  SecretNone      -> "-"
  SecretEnv n     -> "env:" <> n
  SecretLiteral v -> "literal:" <> mask v
  where
    mask v
      | T.length v <= 4 = T.replicate (T.length v) "*"
      | otherwise       = T.replicate (T.length v - 4) "*" <> T.takeEnd 4 v

instance ToJSON SecretRef where
  toJSON = \case
    SecretNone      -> object ["kind" .= ("none" :: Text)]
    SecretEnv n     -> object ["kind" .= ("env" :: Text), "name" .= n]
    SecretLiteral v -> object ["kind" .= ("literal" :: Text), "value" .= v]

instance FromJSON SecretRef where
  parseJSON = withObject "SecretRef" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "none"    -> pure SecretNone
      "env"     -> SecretEnv <$> o .: "name"
      "literal" -> SecretLiteral <$> o .: "value"
      _         -> fail $ "unknown secret kind: " <> T.unpack kind

-- ---------------------------------------------------------------------------
-- Providers
-- ---------------------------------------------------------------------------

-- | A model endpoint: everything needed to talk to somebody's API.
data Provider = Provider
  { provId        :: Text
    -- ^ Stable identifier, unique within the config.
  , provLabel     :: Text
    -- ^ Display name.
  , provWire      :: Wire
    -- ^ Dialect the endpoint speaks.
  , provBaseUrl   :: Text
    -- ^ Base URL, e.g. @https:\/\/api.moonshot.ai\/anthropic@.  Trailing
    -- slashes are tolerated; see "ProxyTool.Proxy".
  , provKey       :: SecretRef
  , provModel     :: Text
    -- ^ Model that the agent's main requests are rewritten to.
  , provFastModel :: Maybe Text
    -- ^ Optional cheap/fast model for agents that use a second tier.
  , provEffort    :: Maybe Effort
  , provHeaders   :: [(Text, Text)]
    -- ^ Extra headers added to every upstream request.
  , provExtraBody :: Maybe Object
    -- ^ Raw JSON merged into every upstream request body.  The escape hatch
    -- for provider knobs this tool does not model.
  } deriving (Eq, Show)

emptyProvider :: Provider
emptyProvider = Provider
  { provId        = ""
  , provLabel     = ""
  , provWire      = WireOpenAIChat
  , provBaseUrl   = ""
  , provKey       = SecretNone
  , provModel     = ""
  , provFastModel = Nothing
  , provEffort    = Nothing
  , provHeaders   = []
  , provExtraBody = Nothing
  }

instance ToJSON Provider where
  toJSON p = object
    [ "id"         .= provId p
    , "label"      .= provLabel p
    , "wire"       .= provWire p
    , "base_url"   .= provBaseUrl p
    , "key"        .= provKey p
    , "model"      .= provModel p
    , "fast_model" .= provFastModel p
    , "effort"     .= provEffort p
    , "headers"    .= map namedPair (provHeaders p)
    , "extra_body" .= provExtraBody p
    ]

instance FromJSON Provider where
  parseJSON = withObject "Provider" $ \o -> Provider
    <$> o .:  "id"
    <*> o .:? "label"    .!= ""
    <*> o .:? "wire"     .!= WireOpenAIChat
    <*> o .:? "base_url" .!= ""
    <*> o .:? "key"      .!= SecretNone
    <*> o .:? "model"    .!= ""
    <*> o .:? "fast_model"
    <*> o .:? "effort"
    <*> (map unNamedPair <$> o .:? "headers" .!= [])
    <*> o .:? "extra_body"

-- ---------------------------------------------------------------------------
-- Agents
-- ---------------------------------------------------------------------------

-- | How to point one particular CLI agent at an endpoint.
--
-- Every field is a template evaluated by 'renderTemplate' with the keys
-- @{base_url}@, @{key}@, @{model}@, @{fast_model}@, @{effort}@,
-- @{effort_openai}@ and @{provider}@.  Keeping this as data -- rather than a
-- case split inside the launcher -- is what lets the TUI adapt the tool to an
-- agent whose flags have changed, without a rebuild.
data Binding = Binding
  { bindBaseUrlEnv   :: Maybe Text
  , bindKeyEnv       :: Maybe Text
  , bindModelEnv     :: Maybe Text
  , bindFastModelEnv :: Maybe Text
  , bindEnv          :: [(Text, Text)]
    -- ^ Additional @NAME=template@ environment entries.
  , bindArgs         :: [Text]
    -- ^ Additional templated command-line arguments.
  } deriving (Eq, Show)

emptyBinding :: Binding
emptyBinding = Binding Nothing Nothing Nothing Nothing [] []

instance ToJSON Binding where
  toJSON b = object
    [ "base_url_env"   .= bindBaseUrlEnv b
    , "key_env"        .= bindKeyEnv b
    , "model_env"      .= bindModelEnv b
    , "fast_model_env" .= bindFastModelEnv b
    , "env"            .= map namedPair (bindEnv b)
    , "args"           .= bindArgs b
    ]

instance FromJSON Binding where
  parseJSON = withObject "Binding" $ \o -> Binding
    <$> o .:? "base_url_env"
    <*> o .:? "key_env"
    <*> o .:? "model_env"
    <*> o .:? "fast_model_env"
    <*> (map unNamedPair <$> o .:? "env" .!= [])
    <*> o .:? "args" .!= []

-- | A CLI coding agent this tool knows how to launch.
data Agent = Agent
  { agId      :: Text
  , agLabel   :: Text
  , agCommand :: Text
    -- ^ Executable name, resolved on @PATH@ at launch time.
  , agArgs    :: [Text]
    -- ^ Fixed leading arguments.
  , agWire    :: Wire
    -- ^ Dialect the agent itself speaks.  Decides whether the proxy has to
    -- translate or can stream bytes through untouched.
  , agBinding :: Binding
  , agNotes   :: Text
  } deriving (Eq, Show)

instance ToJSON Agent where
  toJSON a = object
    [ "id"      .= agId a
    , "label"   .= agLabel a
    , "command" .= agCommand a
    , "args"    .= agArgs a
    , "wire"    .= agWire a
    , "binding" .= agBinding a
    , "notes"   .= agNotes a
    ]

instance FromJSON Agent where
  parseJSON = withObject "Agent" $ \o -> Agent
    <$> o .:  "id"
    <*> o .:? "label"   .!= ""
    <*> o .:  "command"
    <*> o .:? "args"    .!= []
    <*> o .:? "wire"    .!= WireAnthropic
    <*> o .:? "binding" .!= emptyBinding
    <*> o .:? "notes"   .!= ""

-- ---------------------------------------------------------------------------
-- Routing
-- ---------------------------------------------------------------------------

-- | Whether to interpose the local proxy.
data RouteMode
  = RouteAuto    -- ^ Proxy only when it buys something; see "ProxyTool.Launch".
  | RouteDirect  -- ^ Never proxy; point the agent straight at the provider.
  | RouteProxy   -- ^ Always proxy.
  deriving (Eq, Show, Enum, Bounded)

routeLabel :: RouteMode -> Text
routeLabel = \case
  RouteAuto   -> "auto"
  RouteDirect -> "direct"
  RouteProxy  -> "proxy"

routeSlug :: RouteMode -> Text
routeSlug = routeLabel

parseRoute :: Text -> Maybe RouteMode
parseRoute t = lookup (T.toLower (T.strip t))
  [(routeSlug r, r) | r <- [minBound .. maxBound]]

instance ToJSON RouteMode where
  toJSON = String . routeSlug

instance FromJSON RouteMode where
  parseJSON = withText "RouteMode" $ \t ->
    maybe (fail $ "unknown route mode: " <> T.unpack t) pure (parseRoute t)

-- ---------------------------------------------------------------------------
-- Shared JSON helpers
-- ---------------------------------------------------------------------------

-- | Ordered @name@/@value@ pairs, encoded as a JSON array of objects rather
-- than as an object, because header and environment order is meaningful and
-- JSON object key order is not.
namedPair :: (Text, Text) -> Value
namedPair (k, v) = object ["name" .= k, "value" .= v]

unNamedPair :: Object -> (Text, Text)
unNamedPair o = (textAt "name", textAt "value")
  where
    textAt k = case KM.lookup k o of
      Just (String s) -> s
      _               -> ""

-- ---------------------------------------------------------------------------
-- Templates
-- ---------------------------------------------------------------------------

-- | Substitute @{name}@ placeholders.
--
-- An unknown placeholder is left verbatim, so a template mentioning a key
-- this build does not supply degrades to a visible @{foo}@ in the launch
-- preview instead of silently becoming the empty string.
renderTemplate :: [(Text, Text)] -> Text -> Text
renderTemplate vars = go
  where
    go t =
      let (before, rest) = T.breakOn "{" t
      in if T.null rest
           then before
           else
             let body = T.drop 1 rest
                 (name, close) = T.breakOn "}" body
             in if T.null close
                  then before <> rest
                  else
                    let after = T.drop 1 close
                    in case lookup name vars of
                         Just v  -> before <> v <> go after
                         Nothing -> before <> "{" <> name <> "}" <> go after
