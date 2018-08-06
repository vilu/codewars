module Kata.Doubles where

doubles :: Int -> Int -> Double
doubles maxk maxn = sum $ fmap (\(r, c) -> cell r c) [(r, c) | r <- [1..maxk], c <- [1..maxn]]


cell :: Int -> Int -> Double
cell row col = 1 / (k * ((n + 1) ^ (2 * row)))
                where
                    k = fromIntegral row
                    n = fromIntegral col
