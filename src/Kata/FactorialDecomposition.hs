module Kata.FactorialDecomposition (decomp) where


-- The aim of the kata is to decompose n! (factorial n) into its prime factors.

-- Examples:

-- n = 12; decomp(12) -> "2^10 * 3^5 * 5^2 * 7 * 11"
-- since 12! is divisible by 2 ten times, by 3 five times, by 5 two times and by 7 and 11 only once.

-- n = 22; decomp(22) -> "2^19 * 3^9 * 5^4 * 7^3 * 11^2 * 13 * 17 * 19"

-- n = 25; decomp(25) -> 2^22 * 3^10 * 5^6 * 7^3 * 11^2 * 13 * 17 * 19 * 23
-- Prime numbers should be in increasing order. When the exponent of a prime is 1 don't put the exponent.

-- Notes

-- the function is decomp(n) and should return the decomposition of n! into its prime factors in increasing order of the primes, as a string.
-- factorial can be a very big number (4000! has 12674 digits, n will go from 300 to 4000).
-- In Fortran - as in any other language - the returned string is not permitted to contain any redundant trailing whitespace: you can use dynamically allocated character strings.
-- Translators are welcome for all languages.


--  it "Example tests" $ do
--         decomp 17 `shouldBe` "2^15 * 3^6 * 5^3 * 7^2 * 11 * 13 * 17"
--         decomp 5 `shouldBe` "2^3 * 3 * 5"
--         decomp 22 `shouldBe` "2^19 * 3^9 * 5^4 * 7^3 * 11^2 * 13 * 17 * 19"
--         decomp 14 `shouldBe` "2^11 * 3^5 * 5^2 * 7^2 * 11 * 13"
--         decomp 25 `shouldBe` "2^22 * 3^10 * 5^6 * 7^3 * 11^2 * 13 * 17 * 19 * 23"
import           Data.List

decomp :: Int -> String
decomp = expsToString . toExp . decompose . toInteger


factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

primes :: Integer -> [Integer]
primes n = sieve [2..n]
                where
                    sieve (x:xs) = x : sieve (xs \\ [x,x+x..n])
                    sieve []     = []

decompose :: Integer -> [Integer]
decompose n = recurse (reverse $ primes n) (factorial n)

recurse primes@(p:ps) x = if (x `mod` p == 0) then
            p:(recurse primes (x `div` p))
        else
            recurse ps x
recurse [] _ = []


toExp :: [Integer] -> [(Integer, Integer)]
toExp = foldl incrementExps []
            where
                incrementExps ((x,e):xes) n = if (n == x) then (x, e + 1):xes else (n, 1):(x,e):xes
                incrementExps [] n = (n, 1):[]

expsToString :: [(Integer, Integer)] -> String
expsToString xs = intercalate " * " $ map formatEX xs
                where
                    formatEX (x,1) = show x
                    formatEX (x,e) = (show x) ++ "^" ++ (show e)

