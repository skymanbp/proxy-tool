{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the message catalogue.
--
-- The type checker already guarantees that both renderers are total over
-- 'Msg', so a missing translation cannot compile.  What it cannot see is a
-- translation that is empty, that is still the English string, or that
-- silently drops the parameter it was given -- and a dropped parameter is the
-- expensive one, because it turns \"no such agent: kimi\" into \"no such
-- agent\" and takes the answer with it.
module I18nSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import ProxyTool.I18n

spec :: Spec
spec = do
  langSpec
  widthSpec
  catalogueSpec

-- ---------------------------------------------------------------------------
-- Language names
-- ---------------------------------------------------------------------------

langSpec :: Spec
langSpec = describe "languages" $ do

  it "starts in English, because that is the documented default" $
    minBound `shouldBe` LangEn

  it "round-trips every language through its slug" $
    map (parseLang . langSlug) allLangs `shouldBe` map Just allLangs

  it "accepts the spellings people actually type" $
    map parseLang ["en", "EN", " english ", "zh", "cn", "zh-CN", "zh_cn", "Chinese"]
      `shouldBe` [ Just LangEn, Just LangEn, Just LangEn, Just LangZh
                 , Just LangZh, Just LangZh, Just LangZh, Just LangZh ]

  it "rejects an unknown language instead of guessing one" $
    parseLang "klingon" `shouldBe` Nothing

  it "names each language in itself, which is what a picker should show" $ do
    langLabel LangEn `shouldBe` "English"
    langLabel LangZh `shouldSatisfy` (not . T.null)
    langLabel LangZh `shouldNotBe` langLabel LangEn

-- ---------------------------------------------------------------------------
-- Column widths
-- ---------------------------------------------------------------------------

widthSpec :: Spec
widthSpec = describe "textWidth / padWidth" $ do

  it "counts ASCII as one column per character" $
    textWidth "Base URL" `shouldBe` 8

  it "counts CJK as two columns per character" $
    -- Four ideographs, eight columns: this is the whole reason the label
    -- columns are computed rather than assumed.
    textWidth "\25552\20379\21830\21015" `shouldBe` 8

  it "adds the parts of a mixed string" $
    textWidth ("id " <> "\27169\22411") `shouldBe` 7

  it "is empty for empty text" $
    textWidth "" `shouldBe` 0

  it "pads to a column count, not a character count" $ do
    textWidth (padWidth 12 "\25552\20379\21830") `shouldBe` 12
    textWidth (padWidth 12 "Provider")           `shouldBe` 12

  it "never truncates text that is already too wide" $
    padWidth 2 "a long label" `shouldBe` "a long label"

-- ---------------------------------------------------------------------------
-- The catalogue itself
-- ---------------------------------------------------------------------------

catalogueSpec :: Spec
catalogueSpec = describe "catalogue" $ do

  it "says something in every language, for every sampled message" $
    mapM_ (\m -> mapM_ (\l -> render l m `shouldSatisfy` (not . T.null)) allLangs)
          (sampled <> parameterised)

  it "keeps every parameter it was handed, in every language" $
    -- A translation that drops its interpolation still renders, still reads
    -- like a sentence, and has thrown away the only useful part.
    mapM_ (\(m, needles) ->
             mapM_ (\l -> mapM_ (\n -> render l m `shouldSatisfy` T.isInfixOf n)
                                needles)
                   allLangs)
          parameterisedWith

  it "actually translates prose, rather than echoing the English" $
    -- Catches the copy-paste failure: an entry added to the Chinese renderer
    -- by duplicating the English line and forgetting to translate it.
    mapM_ (\m -> render LangZh m `shouldNotBe` render LangEn m) prose

  it "leaves identifiers alone in both languages" $ do
    -- Route and effort names are typed back in by the user, so they are not
    -- prose and must not be translated.
    mapM_ (\l -> render l (MsgCliRoute "auto") `shouldSatisfy` T.isInfixOf "auto")
          allLangs
    mapM_ (\l -> render l (MsgUnknownRoute "sideways")
                   `shouldSatisfy` T.isInfixOf "sideways")
          allLangs

  it "documents the language flag in every language's usage text" $
    mapM_ (\l -> usageText l `shouldSatisfy` T.isInfixOf "--lang") allLangs

  it "writes a help screen in every language" $
    mapM_ (\l -> filter (not . T.null . T.strip) (helpLines l)
                   `shouldSatisfy` ((> 5) . length))
          allLangs

-- | A spread across the catalogue's sections: planning refusals, warnings,
-- preview labels, proxy lifecycle, interface chrome, CLI errors and doctor
-- output.
sampled :: [Msg]
sampled =
  [ MsgWarnNoModel, MsgWarnUnauthenticated
  , MsgLblAgent, MsgLblProvider, MsgLblRoute, MsgLblWarning
  , MsgRouteDirect, MsgRouteProxyRewrite
  , MsgProxyModeRewrite
  , MsgPaneAgents, MsgPaneProviders, MsgLaunchPreview
  , MsgKeyLaunch, MsgKeyLang, MsgKeyQuit, MsgAnyKeyReturns
  , MsgNewProvider, MsgFldBaseUrl, MsgHintBaseUrl, MsgValueUnset
  , MsgErrBaseUrlRequired, MsgErrCommandRequired
  , MsgStatusSelectBoth, MsgPressEnterReturn
  , MsgExpectedPair, MsgRouteNeedsValue, MsgLangNeedsValue
  , MsgDocAgents, MsgDocProviders, MsgDocAllOk, MsgDocNoCred
  ]

parameterisedWith :: [(Msg, [Text])]
parameterisedWith =
  [ (MsgProviderNoBaseUrl "my-provider",            ["my-provider"])
  , (MsgCannotBridge "A" "B" "C" "D",               ["A", "B", "C", "D"])
  , (MsgDirectNeedsMatch "W" "X" "Y" "Z",           ["W", "X", "Y", "Z"])
  , (MsgWarnEffortClamped "ultra",                  ["ultra"])
  , (MsgWarnUnverifiedBinding "kimi",               ["kimi"])
  , (MsgEnvVarNotSet "MY_API_KEY",                  ["MY_API_KEY"])
  , (MsgCommandNotFound "codex",                    ["codex"])
  , (MsgCouldNotRead "cfg.json" "denied",           ["cfg.json", "denied"])
  , (MsgCouldNotWrite "cfg.json" "denied",          ["cfg.json", "denied"])
  , (MsgInvalidConfig "cfg.json" "bad key",         ["cfg.json", "bad key"])
  , (MsgWroteFile "cfg.json",                       ["cfg.json"])
  , (MsgProxyBindFailed "in use",                   ["in use"])
  , (MsgProxyListening "http://127.0.0.1:9" "http://up", ["127.0.0.1:9", "http://up"])
  , (MsgNoSuchAgent "nope",                         ["nope"])
  , (MsgNoSuchProvider "nope",                      ["nope"])
  , (MsgUnknownLang "martian",                      ["martian"])
  , (MsgUnexpectedArg "--wat",                      ["--wat"])
  , (MsgUnrecognisedArgs "a b c",                   ["a b c"])
  , (MsgEditProvider "openai",                      ["openai"])
  , (MsgEditAgent "claude",                         ["claude"])
  , (MsgStatusSaved "cfg.json",                     ["cfg.json"])
  , (MsgStatusDeleted "openai",                     ["openai"])
  , (MsgExitedWith "Claude Code" "status 0",        ["Claude Code", "status 0"])
  , (MsgExitStatus 137,                             ["137"])
  , (MsgDocNeedAttention 3,                         ["3"])
  , (MsgDocCredAvailable "env:MY_KEY",              ["env:MY_KEY"])
  ]

parameterised :: [Msg]
parameterised = map fst parameterisedWith

-- | Messages that are whole sentences rather than labels made of identifiers,
-- so the two languages must genuinely differ.
prose :: [Msg]
prose =
  [ MsgWarnNoModel, MsgWarnUnauthenticated
  , MsgRouteDirect, MsgRouteProxyRewrite
  , MsgHintBaseUrl, MsgHintKey, MsgHintAgCommand
  , MsgErrBaseUrlRequired, MsgErrBaseUrlScheme, MsgErrCommandRequired
  , MsgStatusSelectBoth, MsgStatusNothingToDelete
  , MsgPressEnterReturn, MsgAnyKeyReturns
  , MsgExpectedPair, MsgRouteNeedsValue
  , MsgDocAllOk, MsgDocNoCred, MsgDocAutoPort
  ]
