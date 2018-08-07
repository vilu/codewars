module Kata.RailFenceCipher  where
-- module Kata.RailFenceCipher (encode,decode) where

import Data.List
import Data.Ord

encode :: [a] -> Int -> [a]
encode xs n = fmap fst $ sortOn snd $ zip xs (fence n)

decode :: [a] -> Int -> [a]
decode xs n = fmap fst $ sortOn snd $ zip xs $ decodeOrder xs n

decodeOrder xs n = fmap fst $ sortOn snd $ zip [0..(length xs) - 1] (fence n)

fence n = cycle $ [0..n - 1] ++ [n-2,n-3..1]
