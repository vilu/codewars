module Kata.Huffman ( 
      frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import           Data.List
import           Data.Maybe
import           Control.Monad

data Bit = Z | O deriving (Eq, Show)

data Tree a = Leaf (a, Int)  | Node (Tree a) Int (Tree a) deriving (Eq, Show)

weight :: Tree a -> Int
weight (Node l w r) = w
weight (Leaf (_, w)) = w

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode freqs as| length freqs < 2 = Nothing
               | otherwise = fmap join $ sequence bits
                        where 
                            bits :: [Maybe [Bit]]
                            bits = fmap (\a -> (aToBits freqs >>= (\aTB -> findBitsForA aTB a))) as

                            findBitsForA :: Ord a => [(a, [Bit])] -> a -> Maybe [Bit]
                            findBitsForA aToBit a  = fmap (snd) $ find (\(a', _) -> a == a') aToBit

                            aToBits :: [(a, Int)] -> Maybe [(a, [Bit])]
                            aToBits freqs = fmap treeToBits $ constructTree freqs

                            treeToBits :: Tree a -> [(a, [Bit])]
                            treeToBits t = recurse [] t
                                                where
                                                    recurse pos (Leaf (a, _)) = [(a, (reverse pos))]
                                                    recurse pos (Node l _ r) = (recurse (Z:pos) l) ++ (recurse (O:pos) r)



-- | Decode a bit sequence using the given frequencies.
decode :: (Ord a, Show a) => [(a, Int)] -> [Bit] -> Maybe [a]
decode freqs bits| length freqs < 2 = Nothing 
                 | length bits == 0 = Just []
                 | otherwise = rootTree >>= (\root -> decodeRec root root  bits (Just []))
                                    where 
                                        rootTree = constructTree freqs 

                                        decodeRec :: (Ord a, Show a) => Tree a -> Tree a -> [Bit] -> Maybe [a] -> Maybe [a]
                                        decodeRec root (Leaf (a,_)) [] (Just acc) = Just (reverse $ (a:acc))
                                        decodeRec root (Leaf (a,_)) bits (Just acc) = decodeRec root root bits (Just (a:acc))
                                        decodeRec root (Node l _ r) (b:bits) acc = decodeRec root (if b == Z then l else r) bits acc


frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = sortOnSnd . (foldl freq [])
                where
                    freq ((a,n):as) b|a == b = (a,n+1):as
                    freq as a         = (a,1):as

                    sortOnSnd :: Ord b => [(a, b)] -> [(a, b)]
                    sortOnSnd = (sortOn snd)


constructTree :: [(a, Int)] -> Maybe (Tree a)
constructTree = mergeLeafs . mkLeafs
                    where 
                        mkLeafs :: [(a, Int)] -> [Tree a]
                        mkLeafs = fmap Leaf

                        merge :: Tree a -> Tree a -> Tree a
                        merge l r = Node l ((weight l) + (weight r)) r

                        mergeLeafs :: [Tree a] -> Maybe (Tree a)
                        mergeLeafs ts = if length ts < 2 then Nothing else go ts
                                where 
                                    go ts = case (sortOn weight ts) of [] -> Nothing
                                                                       t1:t2:ts -> go $ (merge t1 t2):ts
                                                                       t:[] -> Just t



-- -- test :: [(Char, Int)]
-- test = (aToBits . frequencies) "aaaaabbccccdeeeeeeeefff"
