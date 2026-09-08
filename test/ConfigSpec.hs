{-# LANGUAGE OverloadedStrings #-}

-- | Tests for config serialisation and editing.
--
-- The config is the tool's only persistent state and the TUI writes it on
-- every change, so a lossy round trip would quietly destroy a user's setup.
module ConfigSpec (spec) where

import Data.Aeson (decode, encode, toJSON, fromJSON, Result (Success, Error))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value (String))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import ProxyTool.Config
import ProxyTool.I18n
import ProxyTool.Types

spec :: Spec
spec = do
  roundTripSpec
  editSpec
  enumSpec
  templateSpec

roundTripSpec :: Spec
roundTripSpec = describe "serialisation" $ do

  it "round-trips the default config without loss" $
    decode (encode defaultConfig) `shouldBe` Just defaultConfig

  it "defaults to English and round-trips a chosen language" $ do
    -- The language is the one setting a user changes and then expects to
    -- survive a restart, so it has to be in the serialised form, not just in
    -- the interface's own state.
    cfgLang defaultConfig `shouldBe` LangEn
    let zh = defaultConfig { cfgLang = LangZh }
    fmap cfgLang (decode (encode zh)) `shouldBe` Just LangZh

  it "round-trips every secret form" $ do
    let trip s = case fromJSON (toJSON s) of
          Success s' -> s'
          Error e    -> error e
    map trip [SecretNone, SecretEnv "K", SecretLiteral "abc123"]
      `shouldBe` [SecretNone, SecretEnv "K", SecretLiteral "abc123"]

  it "preserves header order, which a JSON object would not" $ do
    let p = emptyProvider
          { provId = "p"
          , provBaseUrl = "https://x"
          , provHeaders = [("b-first", "1"), ("a-second", "2")]
          }
        p' = mustJust "decoded provider" (decode (encode p))
    provHeaders p' `shouldBe` [("b-first", "1"), ("a-second", "2")]

  it "round-trips a provider's full detail" $ do
    let p = emptyProvider
          { provId        = "custom"
          , provLabel     = "Custom"
          , provWire      = WireAnthropic
          , provBaseUrl   = "https://api.example.com"
          , provKey       = SecretEnv "MY_KEY"
          , provModel     = "some-model"
          , provFastModel = Just "small-model"
          , provEffort    = Just EffortXHigh
          , provHeaders   = [("x-org", "acme")]
          , provExtraBody = Just (KM.fromList [("cache", String "on")])
          }
    decode (encode p) `shouldBe` Just p

  it "round-trips an agent binding" $ do
    let a = mustJust "codex agent" (lookupAgent "codex")
    decode (encode a) `shouldBe` Just a

  it "fills in defaults for a config file that omits sections" $ do
    let c = mustJust "decoded config" (decode "{}")
    cfgAgents c    `shouldBe` defaultAgents
    cfgProviders c `shouldBe` defaultProviders
    cfgRoute c     `shouldBe` RouteAuto
    cfgPort c      `shouldBe` Nothing
    -- A config written before the language field existed still loads, in
    -- English.
    cfgLang c      `shouldBe` LangEn

  it "rejects an unknown wire name rather than guessing one" $
    (decode "{\"id\":\"x\",\"wire\":\"smoke-signals\"}" :: Maybe Provider)
      `shouldBe` Nothing

-- | Total helpers, so a broken expectation reports itself rather than
-- throwing from Prelude.
mustJust :: String -> Maybe a -> a
mustJust what Nothing  = error ("expected " <> what <> ", got Nothing")
mustJust _    (Just a) = a

dropFirst :: [a] -> [a]
dropFirst = drop 1

lookupAgent :: Text -> Maybe Agent
lookupAgent aid = case filter ((== aid) . agId) defaultAgents of
  (a : _) -> Just a
  []      -> Nothing

editSpec :: Spec
editSpec = describe "editing" $ do

  it "replaces a provider in place, keeping its position" $ do
    let cfg = defaultConfig
        orig = mustJust "openai provider" (findProvider "openai" cfg)
        edited = orig { provModel = "changed" }
        cfg' = upsertProvider edited cfg
    map provId (cfgProviders cfg') `shouldBe` map provId (cfgProviders cfg)
    fmap provModel (findProvider "openai" cfg') `shouldBe` Just "changed"

  it "appends a provider it has not seen" $ do
    let cfg' = upsertProvider (emptyProvider { provId = "new" }) defaultConfig
    length (cfgProviders cfg') `shouldBe` length (cfgProviders defaultConfig) + 1
    fmap provId (findProvider "new" cfg') `shouldBe` Just "new"

  it "removes a provider" $ do
    let cfg' = removeProvider "openai" defaultConfig
    findProvider "openai" cfg' `shouldBe` Nothing
    length (cfgProviders cfg') `shouldBe` length (cfgProviders defaultConfig) - 1

  it "derives an id that is not already taken" $ do
    freshProviderId defaultConfig "OpenAI" `shouldBe` "openai-2"
    freshProviderId defaultConfig "Something New" `shouldBe` "something-new"

  it "falls back to a usable id for a label with nothing to slugify" $
    freshProviderId defaultConfig "" `shouldBe` "provider"

  it "finds shipped agents by id" $
    map agId defaultAgents `shouldBe` ["claude", "codex", "kimi"]

enumSpec :: Spec
enumSpec = describe "enumerations" $ do

  it "round-trips every effort slug" $
    map (parseEffort . effortSlug) allEfforts `shouldBe` map Just allEfforts

  it "parses effort case-insensitively" $
    parseEffort "  XHigh " `shouldBe` Just EffortXHigh

  it "clamps only the levels above the documented OpenAI range" $
    map effortOpenAIWire allEfforts
      `shouldBe` ["none", "minimal", "low", "medium", "high", "xhigh", "xhigh", "xhigh"]

  it "gives no thinking budget for effort none, and increasing ones after" $ do
    effortThinkingBudget EffortNone `shouldBe` Nothing
    let budgets = [b | e <- dropFirst allEfforts, Just b <- [effortThinkingBudget e]]
    budgets `shouldBe` [1024, 4096, 12288, 24576, 32768, 48000, 63000]
    and (zipWith (<) budgets (dropFirst budgets)) `shouldBe` True

  it "accepts the aliases people actually type for wires" $ do
    map parseWire ["anthropic", "openai", "chat", "openai-chat"]
      `shouldBe` [Just WireAnthropic, Just WireOpenAIChat
                 , Just WireOpenAIChat, Just WireOpenAIChat]
    parseWire "nonsense" `shouldBe` Nothing

  it "round-trips route modes" $
    map (parseRoute . routeSlug) [RouteAuto, RouteDirect, RouteProxy]
      `shouldBe` [Just RouteAuto, Just RouteDirect, Just RouteProxy]

  it "never shows more than the last four characters of a literal secret" $ do
    secretDisplay (SecretLiteral "sk-abcdefghijkl") `shouldBe` "literal:***********ijkl"
    secretDisplay (SecretEnv "OPENAI_API_KEY")      `shouldBe` "env:OPENAI_API_KEY"
    -- A dash, not a word: this is rendered inside launch previews in every
    -- interface language, and it names an absence rather than saying one.
    secretDisplay SecretNone                        `shouldBe` "-"

  it "masks a short secret completely" $
    secretDisplay (SecretLiteral "ab") `shouldBe` "literal:**"

templateSpec :: Spec
templateSpec = describe "renderTemplate" $ do

  it "substitutes known placeholders" $
    renderTemplate [("model", "kimi"), ("effort", "high")]
                   "model={model} effort={effort}"
      `shouldBe` "model=kimi effort=high"

  it "leaves an unknown placeholder visible instead of blanking it" $
    -- A silently emptied placeholder would produce a plausible-looking but
    -- wrong command line; a visible {foo} shows up in the launch preview.
    renderTemplate [("model", "m")] "a={model} b={nope}"
      `shouldBe` "a=m b={nope}"

  it "handles an unclosed brace without dropping the rest" $
    renderTemplate [("a", "1")] "x={a} y={unterminated"
      `shouldBe` "x=1 y={unterminated"

  it "is the identity on text with no placeholders" $
    renderTemplate [("a", "1")] "nothing here" `shouldBe` "nothing here"

  it "substitutes an empty value as empty" $
    T.null (renderTemplate [("a", "")] "{a}") `shouldBe` True
