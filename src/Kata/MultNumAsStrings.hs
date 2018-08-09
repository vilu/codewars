module Kata.MultNumAsStrings where

import           Data.Char (digitToInt, intToDigit)

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply xs ys = show $ x * y
                where
                    x = (read xs) :: Int
                    y = (read ys) :: Int
