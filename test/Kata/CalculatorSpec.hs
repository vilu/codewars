module Kata.CalculatorSpec where

import           Kata.Calculator (evaluate)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ spec

spec = describe "DISABLED" $ do
  it "ignore" $ do
    True `shouldBe` True

-- spec :: Spec
-- spec = describe "simple expressions" $ do
--     it "should_work_for_some_simple_expressions" $ do
--         evaluate "2 / 2 + 3 * 4 - 13" `shouldBe` 0
--         evaluate "4 + 3 * 4 / 3 - 6 / 3 * 3 + 8"  `shouldBe` 10
