module Kata.MultNumAsStringsSpec where

import           Kata.MultNumAsStrings
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "your_multiply_function" $ do
    it "should_work_for_some_simple_fixed_tests" $ do
        multiply "2" "3" `shouldBe` "6"
        multiply "30" "69" `shouldBe` "2070"
        multiply "11" "85" `shouldBe` "935"
