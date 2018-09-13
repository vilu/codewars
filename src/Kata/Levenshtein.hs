module Kata.Levenshtein where

import           Data.List
import           Data.Ord

levenshtein :: String -> String -> Int
levenshtein s1 s2
              |min (length s1) (length s2) == 0 = max (length s1) (length s2)
              |last s1 == last s2 = levenshtein (init s1) (init s2)
              |otherwise = minimum [
                  1 + levenshtein (init s1) s2
                , 1 + levenshtein s1 (init s2)
                , 1 + levenshtein (init s1) (init s2)
              ]
