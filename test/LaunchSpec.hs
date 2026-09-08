{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for launch planning.
--
-- Two things here are worth guarding: the routing decision, because getting
-- it wrong sends requests somewhere the user did not choose, and the
-- environment ownership rule, because getting that wrong leaks a credential
-- to a third party.
module LaunchSpec (spec) where

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import ProxyTool.Config
import ProxyTool.I18n
import ProxyTool.Launch
import ProxyTool.Types

spec :: Spec
spec = do
  routingSpec
  argSpec
  envSpec
  previewSpec

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

claudeAgent :: Agent
claudeAgent = fromMaybe (error "claude agent missing") (byId "claude")

codexAgent :: Agent
codexAgent = fromMaybe (error "codex agent missing") (byId "codex")

byId :: Text -> Maybe Agent
byId aid = case filter ((== aid) . agId) defaultAgents of
  (a : _) -> Just a
  []      -> Nothing

anthropicProvider :: Provider
anthropicProvider = emptyProvider
  { provId      = "anth"
  , provWire    = WireAnthropic
  , provBaseUrl = "https://api.example.com/anthropic"
  , provKey     = SecretEnv "SOME_KEY"
  , provModel   = "a-model"
  }

openAIProvider :: Provider
openAIProvider = emptyProvider
  { provId      = "oai"
  , provWire    = WireOpenAIChat
  , provBaseUrl = "https://api.example.com/v1"
  , provKey     = SecretEnv "SOME_KEY"
  , provModel   = "o-model"
  }

planOf :: Agent -> Provider -> RouteMode -> LaunchPlan
planOf a p r = case planLaunch a p r of
  Left err -> error ("plan refused: " <> T.unpack (render LangEn err))
  Right pl -> pl

-- ---------------------------------------------------------------------------
-- Routing
-- ---------------------------------------------------------------------------

routingSpec :: Spec
routingSpec = describe "planLaunch" $ do

  it "goes direct on auto when the dialects match" $ do
    let pl = planOf claudeAgent anthropicProvider RouteAuto
    lpUseProxy pl  `shouldBe` False
    lpTranslate pl `shouldBe` False

  it "proxies on auto when the dialects differ" $ do
    let pl = planOf claudeAgent openAIProvider RouteAuto
    lpUseProxy pl  `shouldBe` True
    lpTranslate pl `shouldBe` True

  it "proxies on request even when the dialects match" $ do
    -- Forcing the proxy is how you get model and effort rewriting against an
    -- endpoint that already speaks your dialect.
    let pl = planOf claudeAgent anthropicProvider RouteProxy
    lpUseProxy pl  `shouldBe` True
    lpTranslate pl `shouldBe` False

  it "refuses direct routing across dialects instead of silently proxying" $
    planLaunch claudeAgent openAIProvider RouteDirect
      `shouldSatisfy` refusedWith (\case MsgDirectNeedsMatch{} -> True; _ -> False)

  it "refuses the unimplemented translation direction up front" $
    -- OpenAI-speaking agent, Anthropic endpoint: caught at plan time rather
    -- than after the agent has already been started.
    planLaunch codexAgent anthropicProvider RouteAuto
      `shouldSatisfy` refusedWith (\case MsgCannotBridge{} -> True; _ -> False)

  it "refuses a provider with no base URL" $
    planLaunch claudeAgent (openAIProvider { provBaseUrl = "" }) RouteAuto
      `shouldSatisfy` refusedWith (\case MsgProviderNoBaseUrl{} -> True; _ -> False)

  it "warns when no model is set" $
    lpWarnings (planOf claudeAgent (anthropicProvider { provModel = "" }) RouteAuto)
      `shouldSatisfy` anyMsg (\case MsgWarnNoModel -> True; _ -> False)

  it "warns about an unauthenticated remote endpoint" $
    lpWarnings (planOf claudeAgent (anthropicProvider { provKey = SecretNone }) RouteAuto)
      `shouldSatisfy` anyMsg isUnauthenticated

  it "does not warn about a missing key for a loopback endpoint" $
    lpWarnings (planOf claudeAgent
                  (anthropicProvider { provKey = SecretNone
                                     , provBaseUrl = "http://127.0.0.1:11434" })
                  RouteAuto)
      `shouldNotSatisfy` anyMsg isUnauthenticated

  it "warns that a Codex-only effort will be clamped on the OpenAI wire" $
    lpWarnings (planOf codexAgent (openAIProvider { provEffort = Just EffortUltra })
                  RouteAuto)
      `shouldSatisfy` anyMsg (\case MsgWarnEffortClamped{} -> True; _ -> False)

  it "surfaces an unverified agent binding as a warning" $
    lpWarnings (planOf (fromMaybe (error "kimi") (byId "kimi"))
                  anthropicProvider RouteAuto)
      `shouldSatisfy` anyMsg (\case MsgWarnUnverifiedBinding{} -> True; _ -> False)

-- | Assertions name the message, not the sentence.  Matching on the English
-- wording would make these tests fail when the English is reworded and pass
-- when the Chinese is wrong, which is exactly backwards.
refusedWith :: (Msg -> Bool) -> Either Msg a -> Bool
refusedWith p = either p (const False)

anyMsg :: (Msg -> Bool) -> [Msg] -> Bool
anyMsg = any

isUnauthenticated :: Msg -> Bool
isUnauthenticated = \case
  MsgWarnUnauthenticated -> True
  _                      -> False

-- ---------------------------------------------------------------------------
-- Argument rendering
-- ---------------------------------------------------------------------------

argSpec :: Spec
argSpec = describe "renderArgList" $ do

  let vars = [("model", "gpt-x"), ("effort", ""), ("base_url", "http://127.0.0.1:1")]
      codexArgs =
        [ "-c", "model_providers.p.base_url={base_url}"
        , "-m", "{model}"
        , "-c", "model_reasoning_effort={effort}"
        ]

  it "drops an argument with an empty placeholder and the flag before it" $
    renderArgList vars codexArgs `shouldBe`
      [ "-c", "model_providers.p.base_url=http://127.0.0.1:1"
      , "-m", "gpt-x" ]

  it "keeps the argument once the placeholder has a value" $
    renderArgList (("effort", "high") : filter ((/= "effort") . fst) vars) codexArgs
      `shouldBe`
        [ "-c", "model_providers.p.base_url=http://127.0.0.1:1"
        , "-m", "gpt-x"
        , "-c", "model_reasoning_effort=high" ]

  it "drops only the flag, not an unrelated preceding value" $
    -- "value=x" is not a bare flag, so it must survive when the next argument
    -- is dropped.
    renderArgList [("m", "")] ["value=x", "-m", "{m}"] `shouldBe` ["value=x"]

  it "leaves arguments without placeholders alone" $
    renderArgList [("a", "")] ["--flag", "literal"] `shouldBe` ["--flag", "literal"]

  it "returns nothing for an empty template list" $
    renderArgList [("a", "1")] [] `shouldBe` []

-- ---------------------------------------------------------------------------
-- Environment ownership
-- ---------------------------------------------------------------------------

envSpec :: Spec
envSpec = describe "resolveLaunch" $ do

  it "claims every variable its binding names, so none can be inherited" $ do
    -- This is the credential-leak guard: with an ANTHROPIC_API_KEY left over
    -- in the shell, an unclaimed variable would be handed to whichever third
    -- party the user just selected.
    let r = resolveLaunch (planOf claudeAgent anthropicProvider RouteAuto)
                          "https://endpoint" "the-key"
    rlClearEnv r `shouldMatchList`
      [ "ANTHROPIC_BASE_URL", "ANTHROPIC_AUTH_TOKEN", "ANTHROPIC_MODEL"
      , "ANTHROPIC_SMALL_FAST_MODEL", "ANTHROPIC_API_KEY"
      , "ANTHROPIC_DEFAULT_HAIKU_MODEL" ]

  it "sets the endpoint, credential and model" $ do
    let r = resolveLaunch (planOf claudeAgent anthropicProvider RouteAuto)
                          "https://endpoint" "the-key"
    lookup "ANTHROPIC_BASE_URL" (rlSetEnv r)   `shouldBe` Just "https://endpoint"
    lookup "ANTHROPIC_AUTH_TOKEN" (rlSetEnv r) `shouldBe` Just "the-key"
    lookup "ANTHROPIC_MODEL" (rlSetEnv r)      `shouldBe` Just "a-model"

  it "omits a variable whose value is empty, leaving it unset rather than blank" $ do
    let r = resolveLaunch (planOf claudeAgent anthropicProvider RouteAuto)
                          "https://endpoint" "the-key"
    -- No fast model is configured on this provider.
    lookup "ANTHROPIC_SMALL_FAST_MODEL" (rlSetEnv r) `shouldBe` Nothing
    -- ... but it is still claimed, so a stale one cannot leak through.
    ("ANTHROPIC_SMALL_FAST_MODEL" `elem` rlClearEnv r) `shouldBe` True

  it "expands templated extra environment entries" $ do
    let p = anthropicProvider { provFastModel = Just "fast-one" }
        r = resolveLaunch (planOf claudeAgent p RouteAuto) "https://e" "k"
    lookup "ANTHROPIC_DEFAULT_HAIKU_MODEL" (rlSetEnv r) `shouldBe` Just "fast-one"

  it "builds the Codex command line from its binding" $ do
    let p = openAIProvider { provEffort = Just EffortHigh }
        r = resolveLaunch (planOf codexAgent p RouteAuto) "http://127.0.0.1:9" "tok"
    rlCommand r `shouldBe` "codex"
    rlArgs r `shouldSatisfy`
      elem "model_providers.proxytool.base_url=http://127.0.0.1:9"
    rlArgs r `shouldSatisfy` elem "model_reasoning_effort=high"
    rlArgs r `shouldSatisfy` elem "o-model"

  it "expands placeholders in an agent's fixed arguments too" $ do
    -- Fixed arguments and binding arguments are both templates.  When only
    -- the latter expanded, a "{effort}" written in the agent's own arguments
    -- reached the process verbatim.
    let a = codexAgent { agArgs = ["--preset", "effort={effort}"] }
        p = openAIProvider { provEffort = Just EffortLow }
        r = resolveLaunch (planOf a p RouteAuto) "http://127.0.0.1:9" "tok"
    rlArgs r `shouldSatisfy` elem "effort=low"
    rlArgs r `shouldNotSatisfy` elem "effort={effort}"

  it "omits the Codex effort flag when no effort is configured" $ do
    let r = resolveLaunch (planOf codexAgent openAIProvider RouteAuto)
                          "http://127.0.0.1:9" "tok"
    rlArgs r `shouldNotSatisfy` any ("model_reasoning_effort" `T.isInfixOf`)

  it "exposes a thinking budget to templates" $ do
    let pl = planOf claudeAgent (anthropicProvider { provEffort = Just EffortHigh })
                    RouteAuto
    lookup "thinking_budget" (templateVars pl "e" "k") `shouldBe` Just "24576"

  it "leaves the thinking budget empty when effort is unset" $ do
    let pl = planOf claudeAgent anthropicProvider RouteAuto
    lookup "thinking_budget" (templateVars pl "e" "k") `shouldBe` Just ""

-- ---------------------------------------------------------------------------
-- Preview
-- ---------------------------------------------------------------------------

previewSpec :: Spec
previewSpec = describe "describeLaunch / previewEndpoint" $ do

  it "points at the provider directly when not proxying" $
    previewEndpoint (planOf claudeAgent anthropicProvider RouteAuto) Nothing
      `shouldBe` "https://api.example.com/anthropic"

  it "points at loopback when proxying, naming the port when it is fixed" $ do
    previewEndpoint (planOf claudeAgent openAIProvider RouteAuto) (Just 8123)
      `shouldBe` "http://127.0.0.1:8123"
    previewEndpoint (planOf claudeAgent openAIProvider RouteAuto) Nothing
      `shouldBe` "http://127.0.0.1:<auto>"

  it "strips a trailing slash from the provider base URL" $
    previewEndpoint (planOf claudeAgent
                       (anthropicProvider { provBaseUrl = "https://x/anthropic/" })
                       RouteAuto) Nothing
      `shouldBe` "https://x/anthropic"

  it "never prints a credential in the preview, in any language" $ do
    -- describeLaunch output is meant to be safe to paste into a bug report,
    -- and that has to hold for every language it can be rendered in -- a
    -- translation is exactly the kind of edit that could reintroduce the key.
    let pl = planOf claudeAgent anthropicProvider RouteAuto
        r  = resolveLaunch pl "https://endpoint" "SUPER-SECRET-VALUE"
    mapM_ (\lang ->
            ("SUPER-SECRET-VALUE" `isInfixOf` previewText lang pl r)
              `shouldBe` False)
          allLangs

  it "reports the translation as a route summary, not as prose" $
    -- The summary is what doctor and the preview both read, so it is the
    -- thing worth pinning; the wording around it is free to change.
    routeSummary (planOf claudeAgent openAIProvider RouteAuto)
      `shouldSatisfy` (\case MsgRouteProxyTranslate{} -> True; _ -> False)

  it "spells the translation out in the rendered preview" $ do
    let pl = planOf claudeAgent openAIProvider RouteAuto
        r  = resolveLaunch pl "http://127.0.0.1:1" "tok"
    previewText LangEn pl r `shouldSatisfy`
      isInfixOf "translating Anthropic Messages -> OpenAI Chat Completions"

  it "flags warning lines structurally rather than by their wording" $ do
    let pl = planOf claudeAgent (anthropicProvider { provModel = "" }) RouteAuto
        r  = resolveLaunch pl "https://endpoint" "tok"
    length (filter plWarning (describeLaunch LangZh pl r))
      `shouldBe` length (lpWarnings pl)

previewText :: Lang -> LaunchPlan -> ResolvedLaunch -> String
previewText lang pl r =
  T.unpack (T.unlines (map plText (describeLaunch lang pl r)))
