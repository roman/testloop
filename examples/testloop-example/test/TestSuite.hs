module Main where

import Test.Hspec
import Test.Hspec.Runner (defaultConfig, hspecWith)

import App

main :: IO ()
main = hspecWith defaultConfig specs >> return ()

specs :: Spec
specs = describe "App" $ do
  it "foo is always a string with foo" $ do
    foo `shouldBe` "foo"
  it "inc should always increment" $ do
    -- TODO: Fix this error
    --
    -- execute ./startTestLoop on the root of the testloop-example
    -- project and then fix this code
    inc 1 `shouldBe` 2
