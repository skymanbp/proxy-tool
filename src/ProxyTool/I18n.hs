{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | User-facing text, in every language the tool speaks.
--
-- The library returns 'Msg' values rather than sentences.  Rendering happens
-- once, at the edge, in whichever language is configured.  That keeps the
-- decision of /what happened/ separate from the decision of /how to say it/,
-- and it means a message can carry its parameters instead of being
-- pre-formatted somewhere that does not know the language yet.
--
-- 'renderEn' and 'renderZh' are both total over 'Msg' with no catch-all, so
-- adding a message fails to compile until both languages have been written.
-- A half-translated build is not representable.
--
-- What is deliberately /not/ translated:
--
--  * Protocol names (@Anthropic Messages@, @OpenAI Chat Completions@), model
--    names, effort levels and route names -- they are identifiers the user
--    types back in, not prose.
--  * JSON error bodies the proxy returns to an agent: those are API payloads
--    read by a program, not by a person.
module ProxyTool.I18n
  ( Lang (..)
  , langSlug
  , langLabel
  , parseLang
  , allLangs
  , Msg (..)
  , render
  , helpLines
  , usageText
    -- * Layout
  , textWidth
  , padWidth
  ) where

import Data.Aeson
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- | Width of a string in terminal columns.
--
-- A label column that lines up in English falls apart the moment the text is
-- Chinese, because those characters occupy two cells each while
-- 'Data.Text.length' counts them as one.  Only the wide ranges that actually
-- appear in this tool's own strings are covered -- CJK, kana, Hangul and
-- fullwidth forms -- which is enough to align its own output and honest about
-- not being a general wcwidth.
textWidth :: Text -> Int
textWidth = T.foldl' (\acc c -> acc + charWidth c) 0
  where
    charWidth c
      | isWide (ord c) = 2
      | otherwise      = 1
    isWide n =
      (n >= 0x1100 && n <= 0x115F)      -- Hangul Jamo
        || (n >= 0x2E80 && n <= 0xA4CF) -- CJK radicals through Yi
        || (n >= 0xAC00 && n <= 0xD7A3) -- Hangul syllables
        || (n >= 0xF900 && n <= 0xFAFF) -- CJK compatibility ideographs
        || (n >= 0xFE30 && n <= 0xFE6F) -- CJK compatibility forms
        || (n >= 0xFF00 && n <= 0xFF60) -- fullwidth forms
        || (n >= 0xFFE0 && n <= 0xFFE6)
        || (n >= 0x20000 && n <= 0x3FFFD)

-- | Pad on the right to a given number of terminal columns.
padWidth :: Int -> Text -> Text
padWidth n t = t <> T.replicate (max 0 (n - textWidth t)) " "

-- ---------------------------------------------------------------------------
-- Languages
-- ---------------------------------------------------------------------------

-- | English is first, and therefore the 'minBound' default.
data Lang = LangEn | LangZh
  deriving (Eq, Show, Enum, Bounded)

allLangs :: [Lang]
allLangs = [minBound .. maxBound]

langSlug :: Lang -> Text
langSlug = \case
  LangEn -> "en"
  LangZh -> "zh"

-- | Each language names itself in itself, which is what a language picker
-- should show.
langLabel :: Lang -> Text
langLabel = \case
  LangEn -> "English"
  LangZh -> "\20013\25991"

parseLang :: Text -> Maybe Lang
parseLang t = case T.toLower (T.strip t) of
  "en"      -> Just LangEn
  "eng"     -> Just LangEn
  "english" -> Just LangEn
  "zh"      -> Just LangZh
  "cn"      -> Just LangZh
  "zh-cn"   -> Just LangZh
  "zh_cn"   -> Just LangZh
  "chinese" -> Just LangZh
  _         -> Nothing

instance ToJSON Lang where
  toJSON = String . langSlug

instance FromJSON Lang where
  parseJSON = withText "Lang" $ \t ->
    maybe (fail ("unknown language: " <> T.unpack t)) pure (parseLang t)

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

-- | Everything the tool says to a person.
data Msg
  -- Launch planning -----------------------------------------------------
  = MsgProviderNoBaseUrl Text
  | MsgCannotBridge Text Text Text Text
    -- ^ agent wire, agent id, provider wire, provider id
  | MsgDirectNeedsMatch Text Text Text Text
    -- ^ agent id, agent wire, provider id, provider wire
  | MsgWarnNoModel
  | MsgWarnUnauthenticated
  | MsgWarnEffortClamped Text
  | MsgWarnUnverifiedBinding Text
  -- Secrets and commands ------------------------------------------------
  | MsgEnvVarNotSet Text
  | MsgCommandNotFound Text
  -- Launch description --------------------------------------------------
  | MsgLblAgent | MsgLblProvider | MsgLblRoute | MsgLblEndpoint | MsgLblUpstream
  | MsgLblModel | MsgLblFastModel | MsgLblEffort | MsgLblKey | MsgLblCommand
  | MsgLblEnvUnset | MsgLblEnvSet | MsgLblWarning
  | MsgSpeaks Text
  | MsgRouteDirect
  | MsgRouteProxyRewrite
  | MsgRouteProxyTranslate Text Text
  -- Config IO -----------------------------------------------------------
  | MsgCouldNotRead Text Text
  | MsgInvalidConfig Text Text
  | MsgCouldNotWrite Text Text
  | MsgWroteFile Text
  -- Proxy ---------------------------------------------------------------
  | MsgProxyBindFailed Text
  | MsgProxyManagerFailed Text
  | MsgProxyListening Text Text
  | MsgProxyModeRewrite
  | MsgProxyModeTranslate Text Text
  | MsgUpstreamFailed Text
  -- TUI chrome ----------------------------------------------------------
  | MsgPaneAgents | MsgPaneProviders | MsgLaunchPreview
  | MsgNoAgentSelected | MsgNoProviderSelected | MsgCannotLaunch Text
  | MsgListEmpty | MsgUnsavedFlag | MsgSavedFlag
  | MsgKeyPane | MsgKeyLaunch | MsgKeyNew | MsgKeyEdit | MsgKeyDelete
  | MsgKeyRoute | MsgKeyBinding | MsgKeySave | MsgKeyHelp | MsgKeyQuit
  | MsgKeyLang | MsgKeyField | MsgKeyChoice | MsgKeyCancel | MsgAnyKeyReturns
  -- Provider form -------------------------------------------------------
  | MsgNewProvider | MsgEditProvider Text
  | MsgFldLabel | MsgFldBaseUrl | MsgFldModel | MsgFldFastModel | MsgFldKey
  | MsgFldHeaders | MsgFldExtraBody | MsgFldDialect | MsgFldEffort
  | MsgHintLabel | MsgHintBaseUrl | MsgHintModel | MsgHintFastModel | MsgHintKey
  | MsgHintHeaders | MsgHintExtraBody | MsgHintDialect | MsgHintEffort
  | MsgValueUnset
  -- Agent form ----------------------------------------------------------
  | MsgEditAgent Text
  | MsgFldAgLabel | MsgFldAgCommand | MsgFldAgArgs | MsgFldAgBaseUrlEnv
  | MsgFldAgKeyEnv | MsgFldAgModelEnv | MsgFldAgFastModelEnv | MsgFldAgExtraEnv
  | MsgHintAgLabel | MsgHintAgCommand | MsgHintAgArgs | MsgHintAgBaseUrlEnv
  | MsgHintAgKeyEnv | MsgHintAgModelEnv | MsgHintAgFastModelEnv | MsgHintAgExtraEnv
  | MsgHintAgDialect
  -- Validation ----------------------------------------------------------
  | MsgErrBaseUrlRequired | MsgErrBaseUrlScheme | MsgErrExtraBodyObject
  | MsgErrExtraBodyJson | MsgErrCommandRequired
  | MsgErrorPrefix Text
  -- Statuses ------------------------------------------------------------
  | MsgStatusConfig Text | MsgStatusSaved Text | MsgStatusSaveFailed Text
  | MsgStatusProviderUpdated Text | MsgStatusDeleted Text
  | MsgStatusAgentUpdated Text | MsgStatusLanguage Text
  | MsgStatusNoProviderToEdit | MsgStatusNoAgentToEdit
  | MsgStatusNothingToDelete | MsgStatusSelectBoth
  | MsgPressEnterReturn | MsgExitedWith Text Text | MsgLaunchRefused Text
  | MsgExitStatus Int
  -- Command line --------------------------------------------------------
  | MsgCliAgents | MsgCliProviders | MsgCliRoute Text
  | MsgNoSuchAgent Text | MsgNoSuchProvider Text
  | MsgUnrecognisedArgs Text | MsgExpectedPair | MsgUnknownRoute Text
  | MsgRouteNeedsValue | MsgUnexpectedArg Text | MsgUnknownLang Text
  | MsgLangNeedsValue
  -- doctor ---------------------------------------------------------------
  | MsgDocConfig | MsgDocRoute | MsgDocProxyPort | MsgDocAutoPort
  | MsgDocLanguage
  | MsgDocAgents | MsgDocProviders | MsgDocPairings
  | MsgDocCredAvailable Text | MsgDocNoCred
  | MsgDocAllOk | MsgDocNeedAttention Int
  deriving (Eq, Show)

render :: Lang -> Msg -> Text
render = \case
  LangEn -> renderEn
  LangZh -> renderZh

-- ---------------------------------------------------------------------------
-- English
-- ---------------------------------------------------------------------------

renderEn :: Msg -> Text
renderEn = \case
  MsgProviderNoBaseUrl p -> "provider \"" <> p <> "\" has no base URL"
  MsgCannotBridge aw ai pw pi_ ->
    "cannot bridge " <> aw <> " (agent \"" <> ai <> "\") to " <> pw
      <> " (provider \"" <> pi_ <> "\"): only the Anthropic-to-OpenAI direction "
      <> "is implemented. Pick a provider that speaks " <> aw
      <> ", or an agent that speaks " <> pw <> "."
  MsgDirectNeedsMatch ai aw pi_ pw ->
    "direct routing needs matching dialects, but agent \"" <> ai <> "\" speaks "
      <> aw <> " and provider \"" <> pi_ <> "\" speaks " <> pw
      <> ". Use route=proxy."
  MsgWarnNoModel ->
    "no model set: the agent's own default will be sent through unchanged"
  MsgWarnUnauthenticated ->
    "no API key configured for a remote endpoint; requests will go out unauthenticated"
  MsgWarnEffortClamped e ->
    "effort " <> e <> " is above the documented OpenAI range and will be sent as xhigh"
  MsgWarnUnverifiedBinding n -> "agent binding is unverified: " <> n
  MsgEnvVarNotSet n -> "environment variable " <> n <> " is not set"
  MsgCommandNotFound c -> "command \"" <> c <> "\" was not found on PATH"

  MsgLblAgent     -> "agent"
  MsgLblProvider  -> "provider"
  MsgLblRoute     -> "route"
  MsgLblEndpoint  -> "endpoint"
  MsgLblUpstream  -> "upstream"
  MsgLblModel     -> "model"
  MsgLblFastModel -> "fast model"
  MsgLblEffort    -> "effort"
  MsgLblKey       -> "key"
  MsgLblCommand   -> "command"
  MsgLblEnvUnset  -> "env unset"
  MsgLblEnvSet    -> "env set"
  MsgLblWarning   -> "warning"
  MsgSpeaks w     -> "speaks " <> w
  MsgRouteDirect  -> "direct"
  MsgRouteProxyRewrite -> "local proxy, rewriting only"
  MsgRouteProxyTranslate a b ->
    "local proxy, translating " <> a <> " -> " <> b

  MsgCouldNotRead p e  -> "could not read " <> p <> ": " <> e
  MsgInvalidConfig p e -> "invalid config at " <> p <> ": " <> e
  MsgCouldNotWrite p e -> "could not write " <> p <> ": " <> e
  MsgWroteFile p       -> "wrote " <> p

  MsgProxyBindFailed e    -> "could not listen on 127.0.0.1: " <> e
  MsgProxyManagerFailed e -> "could not create HTTP manager: " <> e
  MsgProxyListening a b   -> "proxy listening on " <> a <> " -> " <> b
  MsgProxyModeRewrite     -> "rewriting only"
  MsgProxyModeTranslate a b -> "translating " <> a <> " -> " <> b
  MsgUpstreamFailed e     -> "upstream request failed: " <> e

  MsgPaneAgents        -> "Agents"
  MsgPaneProviders     -> "Providers"
  MsgLaunchPreview     -> "Launch preview"
  MsgNoAgentSelected   -> "no agent selected"
  MsgNoProviderSelected -> "no provider selected"
  MsgCannotLaunch e    -> "cannot launch: " <> e
  MsgListEmpty         -> "(none)"
  MsgUnsavedFlag       -> "* unsaved"
  MsgSavedFlag         -> "  saved  "
  MsgKeyPane    -> "pane"
  MsgKeyLaunch  -> "launch"
  MsgKeyNew     -> "new"
  MsgKeyEdit    -> "edit"
  MsgKeyDelete  -> "delete"
  MsgKeyRoute   -> "route"
  MsgKeyBinding -> "binding"
  MsgKeySave    -> "save"
  MsgKeyHelp    -> "help"
  MsgKeyQuit    -> "quit"
  MsgKeyLang    -> "language"
  MsgKeyField   -> "field"
  MsgKeyChoice  -> "choice"
  MsgKeyCancel  -> "cancel"
  MsgAnyKeyReturns -> "any key returns"

  MsgNewProvider    -> "New provider"
  MsgEditProvider p -> "Edit provider: " <> p
  MsgFldLabel     -> "Label"
  MsgFldBaseUrl   -> "Base URL"
  MsgFldModel     -> "Model"
  MsgFldFastModel -> "Fast model"
  MsgFldKey       -> "API key"
  MsgFldHeaders   -> "Headers"
  MsgFldExtraBody -> "Extra body"
  MsgFldDialect   -> "Dialect"
  MsgFldEffort    -> "Effort"
  MsgHintLabel     -> "shown in the provider list"
  MsgHintBaseUrl   -> "Anthropic: stop before /v1.  OpenAI: include /v1"
  MsgHintModel     -> "forced on every request"
  MsgHintFastModel -> "used when the agent asks for a haiku/mini/flash-class model"
  MsgHintKey       -> "env:NAME to read a variable (preferred), or paste the key"
  MsgHintHeaders   -> "Name: value; Name: value"
  MsgHintExtraBody -> "JSON object merged into every request body"
  MsgHintDialect   -> "what the endpoint speaks, not what the agent speaks"
  MsgHintEffort    -> "OpenAI: reasoning_effort.  Anthropic: thinking budget"
  MsgValueUnset    -> "(unset)"

  MsgEditAgent a -> "Edit agent: " <> a
  MsgFldAgLabel        -> "Label"
  MsgFldAgCommand      -> "Command"
  MsgFldAgArgs         -> "Extra args"
  MsgFldAgBaseUrlEnv   -> "Base URL var"
  MsgFldAgKeyEnv       -> "API key var"
  MsgFldAgModelEnv     -> "Model var"
  MsgFldAgFastModelEnv -> "Fast model var"
  MsgFldAgExtraEnv     -> "Extra env"
  MsgHintAgLabel        -> "shown in the agent list"
  MsgHintAgCommand      -> "executable name, resolved on PATH"
  MsgHintAgArgs         ->
    "space separated, so no argument may contain a space; {model} {effort} {base_url} {key} expand"
  MsgHintAgBaseUrlEnv   -> "variable that carries the endpoint"
  MsgHintAgKeyEnv       -> "variable that carries the credential"
  MsgHintAgModelEnv     -> "variable that carries the model name"
  MsgHintAgFastModelEnv -> "variable for the cheap tier, if the agent has one"
  MsgHintAgExtraEnv     -> "NAME={model}; NAME2={effort}"
  MsgHintAgDialect      -> "what this agent speaks on the wire"

  MsgErrBaseUrlRequired -> "base URL is required"
  MsgErrBaseUrlScheme   -> "base URL must start with http:// or https://"
  MsgErrExtraBodyObject -> "extra body must be a JSON object"
  MsgErrExtraBodyJson   -> "extra body is not valid JSON"
  MsgErrCommandRequired -> "command is required"
  MsgErrorPrefix e      -> "error: " <> e

  MsgStatusConfig p           -> "config: " <> p
  MsgStatusSaved p            -> "saved " <> p
  MsgStatusSaveFailed e       -> "save failed: " <> e
  MsgStatusProviderUpdated p  -> "provider " <> p <> " updated (unsaved)"
  MsgStatusDeleted p          -> "deleted " <> p <> " (unsaved)"
  MsgStatusAgentUpdated a     -> "agent " <> a <> " updated (unsaved)"
  MsgStatusLanguage l         -> "language: " <> l <> " (unsaved)"
  MsgStatusNoProviderToEdit   -> "no provider to edit"
  MsgStatusNoAgentToEdit      -> "no agent to edit"
  MsgStatusNothingToDelete    -> "nothing to delete"
  MsgStatusSelectBoth         -> "select both an agent and a provider"
  MsgPressEnterReturn         -> "press Enter to return to the menu "
  MsgExitedWith a s           -> a <> " exited with " <> s
  MsgLaunchRefused e          -> "launch refused: " <> e
  MsgExitStatus n             -> "status " <> T.pack (show n)

  MsgCliAgents      -> "agents:"
  MsgCliProviders   -> "providers:"
  MsgCliRoute r     -> "route: " <> r
  MsgNoSuchAgent a  -> "no such agent: " <> a <> " (try: proxy-tool list)"
  MsgNoSuchProvider p -> "no such provider: " <> p <> " (try: proxy-tool list)"
  MsgUnrecognisedArgs a -> "unrecognised arguments: " <> a
  MsgExpectedPair   -> "expected: <agent> <provider> [--route auto|direct|proxy] [--lang en|zh]"
  MsgUnknownRoute r -> "unknown route: " <> r <> " (expected auto, direct or proxy)"
  MsgRouteNeedsValue -> "--route needs a value"
  MsgUnexpectedArg a -> "unexpected argument: " <> a
  MsgUnknownLang l   -> "unknown language: " <> l <> " (expected en or zh)"
  MsgLangNeedsValue  -> "--lang needs a value"

  MsgDocConfig    -> "config      "
  MsgDocRoute     -> "route       "
  MsgDocProxyPort -> "proxy port  "
  MsgDocLanguage  -> "language    "
  MsgDocAutoPort  -> "auto (ephemeral)"
  MsgDocAgents    -> "agents:"
  MsgDocProviders -> "providers:"
  MsgDocPairings  -> "pairings:"
  MsgDocCredAvailable s -> "credential available (" <> s <> ")"
  MsgDocNoCred    -> "no credential configured"
  MsgDocAllOk     -> "all agents runnable and all credentials resolvable"
  MsgDocNeedAttention n ->
    T.pack (show n) <> " item(s) need attention above"

-- ---------------------------------------------------------------------------
-- Chinese
-- ---------------------------------------------------------------------------
--
-- Written with escapes rather than literal characters so the file stays pure
-- ASCII: this project is built on a machine whose ANSI code page is 936, and
-- a source file that is UTF-8 without a BOM is decoded with the code page by
-- some Windows toolchains.  Escapes cannot be corrupted that way.

renderZh :: Msg -> Text
renderZh = \case
  MsgProviderNoBaseUrl p ->
    "\25552\20379\21830 \"" <> p <> "\" \27809\26377\35774\32622 Base URL"
  MsgCannotBridge aw ai pw pi_ ->
    "\26080\27861\26725\25509：" <> aw <> "（agent \"" <> ai
      <> "\"）\8594 " <> pw <> "（\25552\20379\21830 \"" <> pi_
      <> "\"）\12290\30446\21069\20165\23454\29616 Anthropic \8594 OpenAI "
      <> "\26041\21521\12290\35831\25913\29992\35828 " <> aw
      <> " \30340\25552\20379\21830，\25110\25913\29992\35828 " <> pw
      <> " \30340 agent\12290"
  MsgDirectNeedsMatch ai aw pi_ pw ->
    "\30452\36830\35201\27714\21327\35758\19968\33268，\20294 agent \""
      <> ai <> "\" \35828 " <> aw <> "，\25552\20379\21830 \"" <> pi_
      <> "\" \35828 " <> pw <> "\12290\35831\25913\29992 route=proxy\12290"
  MsgWarnNoModel ->
    "\26410\35774\32622\27169\22411：agent \33258\24102\30340\40664\35748\20540\23558\21407\26679\36716\21457"
  MsgWarnUnauthenticated ->
    "\36828\31471\26410\37197\32622 API \23494\38053：\35831\27714\23558\20197\26410\35748\35777\26041\24335\21457\20986"
  MsgWarnEffortClamped e ->
    "effort " <> e
      <> " \36229\20986 OpenAI \25991\26723\33539\22260，\23558\25353 xhigh \21457\36865"
  MsgWarnUnverifiedBinding n ->
    "agent \32465\23450\26410\32463\39564\35777：" <> n
  MsgEnvVarNotSet n ->
    "\29615\22659\21464\37327 " <> n <> " \26410\35774\32622"
  MsgCommandNotFound c ->
    "\22312 PATH \19978\26410\25214\21040\21629\20196 \"" <> c <> "\""

  MsgLblAgent     -> "agent"
  MsgLblProvider  -> "\25552\20379\21830"
  MsgLblRoute     -> "\36335\30001"
  MsgLblEndpoint  -> "\31471\28857"
  MsgLblUpstream  -> "\19978\28216"
  MsgLblModel     -> "\27169\22411"
  MsgLblFastModel -> "\24555\36895\27169\22411"
  MsgLblEffort    -> "\24605\32771\24378\24230"
  MsgLblKey       -> "\23494\38053"
  MsgLblCommand   -> "\21629\20196"
  MsgLblEnvUnset  -> "\28165\38500\29615\22659\21464\37327"
  MsgLblEnvSet    -> "\35774\32622\29615\22659\21464\37327"
  MsgLblWarning   -> "\35686\21578"
  MsgSpeaks w     -> "\35828 " <> w
  MsgRouteDirect  -> "\30452\36830"
  MsgRouteProxyRewrite -> "\26412\22320\20195\29702，\20165\25913\20889"
  MsgRouteProxyTranslate a b ->
    "\26412\22320\20195\29702，\32763\35793 " <> a <> " \8594 " <> b

  MsgCouldNotRead p e  -> "\26080\27861\35835\21462 " <> p <> "：" <> e
  MsgInvalidConfig p e -> "\37197\32622\26080\25928 " <> p <> "：" <> e
  MsgCouldNotWrite p e -> "\26080\27861\20889\20837 " <> p <> "：" <> e
  MsgWroteFile p       -> "\24050\20889\20837 " <> p

  MsgProxyBindFailed e    ->
    "\26080\27861\30417\21548 127.0.0.1：" <> e
  MsgProxyManagerFailed e ->
    "\26080\27861\21019\24314 HTTP \36830\25509\27744：" <> e
  MsgProxyListening a b   ->
    "\20195\29702\30417\21548\20110 " <> a <> " \8594 " <> b
  MsgProxyModeRewrite     -> "\20165\25913\20889"
  MsgProxyModeTranslate a b -> "\32763\35793 " <> a <> " \8594 " <> b
  MsgUpstreamFailed e     -> "\19978\28216\35831\27714\22833\36133：" <> e

  MsgPaneAgents        -> "Agent \21015\34920"
  MsgPaneProviders     -> "\25552\20379\21830"
  MsgLaunchPreview     -> "\21551\21160\39044\35272"
  MsgNoAgentSelected   -> "\26410\36873\25321 agent"
  MsgNoProviderSelected -> "\26410\36873\25321\25552\20379\21830"
  MsgCannotLaunch e    -> "\26080\27861\21551\21160：" <> e
  MsgListEmpty         -> "（\26080）"
  MsgUnsavedFlag       -> "* \26410\20445\23384"
  MsgSavedFlag         -> "  \24050\20445\23384"
  MsgKeyPane    -> "\20999\25442"
  MsgKeyLaunch  -> "\21551\21160"
  MsgKeyNew     -> "\26032\24314"
  MsgKeyEdit    -> "\32534\36753"
  MsgKeyDelete  -> "\21024\38500"
  MsgKeyRoute   -> "\36335\30001"
  MsgKeyBinding -> "\32465\23450"
  MsgKeySave    -> "\20445\23384"
  MsgKeyHelp    -> "\24110\21161"
  MsgKeyQuit    -> "\36864\20986"
  MsgKeyLang    -> "\35821\35328"
  MsgKeyField   -> "\23383\27573"
  MsgKeyChoice  -> "\36873\39033"
  MsgKeyCancel  -> "\21462\28040"
  MsgAnyKeyReturns -> "\25353\20219\24847\38190\36820\22238"

  MsgNewProvider    -> "\26032\24314\25552\20379\21830"
  MsgEditProvider p -> "\32534\36753\25552\20379\21830：" <> p
  MsgFldLabel     -> "\21517\31216"
  MsgFldBaseUrl   -> "Base URL"
  MsgFldModel     -> "\27169\22411"
  MsgFldFastModel -> "\24555\36895\27169\22411"
  MsgFldKey       -> "API \23494\38053"
  MsgFldHeaders   -> "\35831\27714\22836"
  MsgFldExtraBody -> "\39069\22806\35831\27714\20307"
  MsgFldDialect   -> "\21327\35758"
  MsgFldEffort    -> "\24605\32771\24378\24230"
  MsgHintLabel     -> "\26174\31034\22312\25552\20379\21830\21015\34920\20013"
  MsgHintBaseUrl   ->
    "Anthropic：\19981\21253\21547 /v1；OpenAI：\38656\21253\21547 /v1"
  MsgHintModel     -> "\24378\21046\29992\20110\27599\19968\27425\35831\27714"
  MsgHintFastModel ->
    "\24403 agent \35831\27714 haiku/mini/flash \31561\32423\27169\22411\26102\26367\25442"
  MsgHintKey       ->
    "\29992 env:NAME \35835\29615\22659\21464\37327（\25512\33616），\25110\30452\25509\31896\36148\23494\38053"
  MsgHintHeaders   -> "Name: value; Name: value"
  MsgHintExtraBody ->
    "\21512\24182\21040\27599\19968\27425\35831\27714\20307\30340 JSON \23545\35937"
  MsgHintDialect   ->
    "\25351\31471\28857\35828\30340\21327\35758，\19981\26159 agent \35828\30340"
  MsgHintEffort    ->
    "OpenAI：reasoning_effort；Anthropic：thinking \39044\31639"
  MsgValueUnset    -> "（\26410\35774\32622）"

  MsgEditAgent a -> "\32534\36753 agent：" <> a
  MsgFldAgLabel        -> "\21517\31216"
  MsgFldAgCommand      -> "\21629\20196"
  MsgFldAgArgs         -> "\39069\22806\21442\25968"
  MsgFldAgBaseUrlEnv   -> "Base URL \21464\37327"
  MsgFldAgKeyEnv       -> "\23494\38053\21464\37327"
  MsgFldAgModelEnv     -> "\27169\22411\21464\37327"
  MsgFldAgFastModelEnv -> "\24555\36895\27169\22411\21464\37327"
  MsgFldAgExtraEnv     -> "\39069\22806\29615\22659\21464\37327"
  MsgHintAgLabel        -> "\26174\31034\22312 agent \21015\34920\20013"
  MsgHintAgCommand      -> "\21487\25191\34892\25991\20214\21517，\22312 PATH \19978\26597\25214"
  MsgHintAgArgs         ->
    "\20197\31354\26684\20998\38548，\22240\27492\21442\25968\20869\19981\33021\24102\31354\26684；{model} {effort} {base_url} {key} \20250\23637\24320"
  MsgHintAgBaseUrlEnv   -> "\25215\36733\31471\28857\30340\29615\22659\21464\37327"
  MsgHintAgKeyEnv       -> "\25215\36733\23494\38053\30340\29615\22659\21464\37327"
  MsgHintAgModelEnv     -> "\25215\36733\27169\22411\21517\30340\29615\22659\21464\37327"
  MsgHintAgFastModelEnv ->
    "\24265\20215\26723\20301\30340\29615\22659\21464\37327（\22914\26524 agent \25903\25345）"
  MsgHintAgExtraEnv     -> "NAME={model}; NAME2={effort}"
  MsgHintAgDialect      -> "\27492 agent \22312\32447\19978\35828\30340\21327\35758"

  MsgErrBaseUrlRequired -> "Base URL \24517\22635"
  MsgErrBaseUrlScheme   -> "Base URL \24517\39035\20197 http:// \25110 https:// \24320\22836"
  MsgErrExtraBodyObject -> "\39069\22806\35831\27714\20307\24517\39035\26159 JSON \23545\35937"
  MsgErrExtraBodyJson   -> "\39069\22806\35831\27714\20307\19981\26159\21512\27861\30340 JSON"
  MsgErrCommandRequired -> "\21629\20196\24517\22635"
  MsgErrorPrefix e      -> "\38169\35823：" <> e

  MsgStatusConfig p          -> "\37197\32622：" <> p
  MsgStatusSaved p           -> "\24050\20445\23384 " <> p
  MsgStatusSaveFailed e      -> "\20445\23384\22833\36133：" <> e
  MsgStatusProviderUpdated p ->
    "\25552\20379\21830 " <> p <> " \24050\26356\26032（\26410\20445\23384）"
  MsgStatusDeleted p ->
    "\24050\21024\38500 " <> p <> "（\26410\20445\23384）"
  MsgStatusAgentUpdated a ->
    "agent " <> a <> " \24050\26356\26032（\26410\20445\23384）"
  MsgStatusLanguage l ->
    "\35821\35328：" <> l <> "（\26410\20445\23384）"
  MsgStatusNoProviderToEdit -> "\27809\26377\21487\32534\36753\30340\25552\20379\21830"
  MsgStatusNoAgentToEdit    -> "\27809\26377\21487\32534\36753\30340 agent"
  MsgStatusNothingToDelete  -> "\27809\26377\21487\21024\38500\30340\39033"
  MsgStatusSelectBoth       -> "\35831\21516\26102\36873\25321 agent \21644\25552\20379\21830"
  MsgPressEnterReturn       -> "\25353 Enter \36820\22238\33756\21333 "
  MsgExitedWith a s         -> a <> " \24050\36864\20986，" <> s
  MsgLaunchRefused e        -> "\21551\21160\34987\25298\32477：" <> e
  MsgExitStatus n           -> "\36864\20986\30721 " <> T.pack (show n)

  MsgCliAgents      -> "agent："
  MsgCliProviders   -> "\25552\20379\21830："
  MsgCliRoute r     -> "\36335\30001：" <> r
  MsgNoSuchAgent a  ->
    "\26080\27492 agent：" <> a <> "（\35797\35797：proxy-tool list）"
  MsgNoSuchProvider p ->
    "\26080\27492\25552\20379\21830：" <> p <> "（\35797\35797：proxy-tool list）"
  MsgUnrecognisedArgs a -> "\26080\27861\35782\21035\30340\21442\25968：" <> a
  MsgExpectedPair   ->
    "\29992\27861：<agent> <provider> [--route auto|direct|proxy] [--lang en|zh]"
  MsgUnknownRoute r ->
    "\26410\30693\36335\30001：" <> r <> "（\24212\20026 auto\12289direct \25110 proxy）"
  MsgRouteNeedsValue -> "--route \32570\23569\21442\25968\20540"
  MsgUnexpectedArg a -> "\24847\22806\30340\21442\25968：" <> a
  MsgUnknownLang l   ->
    "\26410\30693\35821\35328：" <> l <> "（\24212\20026 en \25110 zh）"
  MsgLangNeedsValue  -> "--lang \32570\23569\21442\25968\20540"

  MsgDocConfig    -> "\37197\32622\25991\20214  "
  MsgDocRoute     -> "\36335\30001      "
  MsgDocProxyPort -> "\20195\29702\31471\21475  "
  MsgDocLanguage  -> "\30028\38754\35821\35328  "
  MsgDocAutoPort  -> "\33258\21160（\20020\26102\31471\21475）"
  MsgDocAgents    -> "agent："
  MsgDocProviders -> "\25552\20379\21830："
  MsgDocPairings  -> "\32452\21512："
  MsgDocCredAvailable s -> "\23494\38053\21487\29992（" <> s <> "）"
  MsgDocNoCred    -> "\26410\37197\32622\23494\38053"
  MsgDocAllOk     ->
    "\25152\26377 agent \22343\21487\21551\21160，\25152\26377\23494\38053\22343\21487\35835\21462"
  MsgDocNeedAttention n ->
    "\26377 " <> T.pack (show n) <> " \39033\38656\35201\22788\29702（\35265\19978）"

-- ---------------------------------------------------------------------------
-- Multi-line texts
-- ---------------------------------------------------------------------------

helpLines :: Lang -> [Text]
helpLines LangEn =
  [ ""
  , "  Pick an agent on the left and a provider on the right, then press Enter."
  , "  The agent takes over the terminal; when it exits you come back here."
  , ""
  , "  route=direct  the agent talks straight to the provider"
  , "  route=proxy   a loopback proxy rewrites model and effort on the way"
  , "  route=auto    proxy only when the two speak different dialects"
  , ""
  , "  A proxy is required to run an Anthropic-speaking agent against an"
  , "  OpenAI-compatible endpoint; that translation is implemented. The"
  , "  reverse direction is not, and is refused before anything is started."
  , ""
  , "  Keys: Tab pane   Up/Down select   Enter launch   n new provider"
  , "        e edit provider   d delete   b edit agent binding"
  , "        r cycle route   L language   s save   q quit"
  , ""
  ]
helpLines LangZh =
  [ ""
  , "  \22312\24038\20391\36873 agent，\21491\20391\36873\25552\20379\21830，\28982\21518\25353 Enter\12290"
  , "  agent \20250\25509\31649\32456\31471；\23427\36864\20986\21518\20250\22238\21040\36825\37324\12290"
  , ""
  , "  route=direct  agent \30452\25509\35775\38382\25552\20379\21830"
  , "  route=proxy   \32463\26412\22320\20195\29702，\36884\20013\25913\20889\27169\22411\19982\24605\32771\24378\24230"
  , "  route=auto    \20165\22312\21327\35758\19981\21516\26102\25165\36208\20195\29702"
  , ""
  , "  \35753\35828 Anthropic \30340 agent \36208 OpenAI \20860\23481\31471\28857\24517\39035\32463\20195\29702，"
  , "  \35813\26041\21521\24050\23454\29616\12290\21453\26041\21521\26410\23454\29616，\19988\20250\22312\21551\21160\21069\30452\25509\25298\32477\12290"
  , ""
  , "  \25353\38190：Tab \20999\25442   \19978/\19979 \36873\25321   Enter \21551\21160   n \26032\24314\25552\20379\21830"
  , "        e \32534\36753\25552\20379\21830   d \21024\38500   b \32534\36753 agent \32465\23450"
  , "        r \20999\25442\36335\30001   L \20999\25442\35821\35328   s \20445\23384   q \36864\20986"
  , ""
  ]

usageText :: Lang -> Text
usageText LangEn = T.unlines
  [ "proxy-tool -- run a CLI coding agent against any model provider"
  , ""
  , "  proxy-tool                     open the terminal UI"
  , "  proxy-tool list                list configured agents and providers"
  , "  proxy-tool plan <agent> <provider> [--route auto|direct|proxy]"
  , "                                 show what a launch would do, and stop"
  , "  proxy-tool run  <agent> <provider> [--route auto|direct|proxy]"
  , "                                 launch the agent"
  , "  proxy-tool doctor              check config, commands and credentials"
  , "  proxy-tool config path|show|init"
  , ""
  , "  --lang en|zh                   interface language for this run"
  , ""
  , "Config path: PROXY_TOOL_CONFIG.  Language: PROXY_TOOL_LANG, or the"
  , "\"lang\" field in the config file.  The default is English."
  ]
usageText LangZh = T.unlines
  [ "proxy-tool -- \35753 CLI \32534\30721 agent \36816\34892\22312\20219\24847\27169\22411\25552\20379\21830\19978"
  , ""
  , "  proxy-tool                     \25171\24320\32456\31471\30028\38754"
  , "  proxy-tool list                \21015\20986\24050\37197\32622\30340 agent \19982\25552\20379\21830"
  , "  proxy-tool plan <agent> <provider> [--route auto|direct|proxy]"
  , "                                 \21482\26174\31034\21551\21160\35745\21010，\19981\30495\30340\21551\21160"
  , "  proxy-tool run  <agent> <provider> [--route auto|direct|proxy]"
  , "                                 \21551\21160 agent"
  , "  proxy-tool doctor              \26816\26597\37197\32622\12289\21629\20196\19982\23494\38053"
  , "  proxy-tool config path|show|init"
  , ""
  , "  --lang en|zh                   \26412\27425\36816\34892\30340\30028\38754\35821\35328"
  , ""
  , "\37197\32622\36335\24452：PROXY_TOOL_CONFIG\12290\35821\35328：PROXY_TOOL_LANG，"
  , "\25110\37197\32622\25991\20214\30340 \"lang\" \23383\27573\12290\40664\35748\20026\33521\25991\12290"
  ]
