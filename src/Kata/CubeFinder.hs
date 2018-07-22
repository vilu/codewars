module Kata.CubeFinder where


    -- Your task is to construct a building which will be a pile of n cubes. The cube at the bottom will have a volume of n^3, the cube above will have volume of (n-1)^3 and so on until the top which will have a volume of 1^3.

    -- You are given the total volume m of the building. Being given m can you find the number n of cubes you will have to build?

    -- The parameter of the function findNb (find_nb, find-nb, findNb) will be an integer m and you have to return the integer n such as n^3 + (n-1)^3 + ... + 1^3 = m if such a n exists or -1 if there is no such n.

    -- Examples:
    -- findNb(1071225) --> 45
    -- findNb(91716553919377) --> -1

-- TRY 1: failed because of timeout
-- Brute force, didn't work
-- findNb :: Integer -> Integer
-- findNb m = recurse m 0 where
--             recurse 0 n = n - 1
--             recurse m n = if (m < 0)
--                             then - 1
--                             else recurse (m - (n ^ 3)) (n + 1)



-- Try 2: Fails on high M because of a loss of floating point precision on sqrt
findNb :: Integer -> Integer
findNb m = if (isInt n) then round n else -1 
            where
                n = cuadraticFormula (fromInteger 1) (fromInteger 1) c
                c = -2 * sqrt (fromInteger m )


isInt x = x == fromInteger (round x)


cuadraticFormula :: Double -> Double -> Double -> Double
cuadraticFormula a b c = ((-b) + sqrt (b ^ 2 - (4 * a * c))) / (2 * a)