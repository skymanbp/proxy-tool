module Main (main) where

import Test.Hspec

import qualified ConfigSpec
import qualified I18nSpec
import qualified LaunchSpec
import qualified TranslateSpec

main :: IO ()
main = hspec $ do
  describe "ProxyTool.Translate" TranslateSpec.spec
  describe "ProxyTool.Config"    ConfigSpec.spec
  describe "ProxyTool.Launch"    LaunchSpec.spec
  describe "ProxyTool.I18n"      I18nSpec.spec
