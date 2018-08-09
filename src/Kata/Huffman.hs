module Kata.Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import           Data.List

data Bit = Z | O deriving (Eq, Show)

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = error "frequencies not yet implemented"

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode = error "encode not yet implemented"

-- | Decode a bit sequence using the given frequencies.
decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode = error "decode not yet implemented"
