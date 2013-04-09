module Main where

import           Test.Hspec
import           Test.Hspec.Runner (defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig specs >> return ()

specs = describe "Foo" $ do
  it "bar" $ do
    True `shouldBe` True
  it "baz" $ do
    True `shouldBe` False
