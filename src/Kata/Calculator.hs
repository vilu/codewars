module Kata.Calculator where

data Expression = Division | Multiplication | Addition | Subtraction | Val Double deriving (Show)


evaluate :: String -> Double
evaluate = val . secondPass . firstPass . readExp

readExp :: String -> [Expression]
readExp s = fmap parseExp $ words s
            where
                parseExp "*" = Multiplication
                parseExp "/" = Division
                parseExp "+" = Addition
                parseExp "-" = Subtraction
                parseExp xs  = Val (read xs :: Double)

firstPass :: [Expression] -> [Expression]
firstPass xs = reverse $ foldl go [] xs
                where
                    go :: [Expression] -> Expression -> [Expression]
                    go acc'@(Multiplication:(Val x):acc) (Val y) = (Val (x*y)):acc
                    go acc'@(Division:(Val x):acc)       (Val y) = (Val (x/y)):acc
                    go  acc                              e        = e:acc

secondPass :: [Expression] -> [Expression]
secondPass xs = reverse $ foldl go [] xs
                where
                    go :: [Expression] -> Expression -> [Expression]
                    go  (Addition:(Val x):acc)     (Val y) = (Val (x+y)):acc
                    go  (Subtraction:(Val x):acc)  (Val y) = (Val (x-y)):acc
                    go  acc                        e       = e:acc

val :: [Expression] -> Double
val ((Val d):ds) = d
