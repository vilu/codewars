module Kata.Reduction where

data Direction = North | East | West | South deriving (Eq, Show)

dirReduce :: [Direction] -> [Direction]
dirReduce = reverse . (foldl reduce [])
        where
            reduce (d:ds) dir = if (opposites d dir) then ds else (dir:d:ds)
            reduce [] dir = [dir]
            
            opposites North South = True
            opposites South North = True
            opposites West East = True
            opposites East West = True
            opposites _ _ = False
            



