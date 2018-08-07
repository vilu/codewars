module Kata.RailFenceCipherSpec (spec) where

import Test.Hspec
import Kata.RailFenceCipher (encode,decode)

spec :: Spec
spec = do
  describe "Rail Fence Cipher" $ do
    it "Example Tests" $ do
      encode "WEAREDISCOVEREDFLEEATONCE" 3 `shouldBe` "WECRLTEERDSOEEFEAOCAIVDEN"
      decode "WECRLTEERDSOEEFEAOCAIVDEN" 3 `shouldBe` "WEAREDISCOVEREDFLEEATONCE"
      encode "Hello, World!" 3 `shouldBe` "Hoo!el,Wrdl l"
      "Hello, World!" `encode` 3 `decode` 3 `shouldBe` "Hello, World!"