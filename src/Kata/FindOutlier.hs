module Kata.FindOutlier where

findOutlier :: [Int] -> Int 
findOutlier = head . filterOnParity

filterOnParity :: [Int] -> [Int]
filterOnParity xs = if evenInts xs > 1 then filter (not . isEven) xs else filter isEven xs

evenInts :: [Int] -> Int
evenInts = length . (filter isEven) . take 3

isEven x = x `mod` 2 == 0