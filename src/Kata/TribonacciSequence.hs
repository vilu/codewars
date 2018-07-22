module Kata.TribonacciSequence where

-- tribonacci :: Int -> [Int] -> [Int]
-- tribonacci n s = take n $ trib s
--                     where 
--                         trib n
--                             | n < 3 = [s !! n] ++
--                             | n = trib (n - 1) ++ trib (n - 2) ++ trib (n - 3)

tribonacci signature n = take n $ map (trib signature) [0..]
        where
            trib sig 0 = sig !! 0
            trib sig 1 = sig !! 1
            trib sig 2 = sig !! 2
            trib sig n' = (trib sig (n' - 3)) + (trib sig (n' - 2)) + (trib sig (n' - 1))


fibonacci n = take n (map fib [0..])
            where
                fib 0 = 0
                fib 1 = 1
                fib n = fib (n - 1) + fib (n - 2)