module Kata.DoublesSpec where
import Kata.Doubles (doubles)   

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)
import Control.Monad (when)

shouldBeFuzzy :: Double -> Double -> Expectation
shouldBeFuzzy act expec =
    when (abs (act - expec) > 1e-6) $ expectationFailure msg
  where msg = "abs(actual - expected) must be <= 1e-6. Expected was " ++ show expec ++ " but got: " ++ show act

testDouble :: Int -> Int -> Double -> Spec
testDouble k n u = 
    it (printf "should return doubles for k: %d n: %d " k n) $
        doubles k n `shouldBeFuzzy` u

main = hspec spec 

spec = do
    describe "doubles" $ do
        testDouble 1 10 0.5580321939764581
        testDouble 10 1000 0.6921486500921933
        testDouble 15 1000 0.6921486782113179
        testDouble 20 1500 0.6924811798693108
        

                    