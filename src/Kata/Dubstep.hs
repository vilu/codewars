module Kata.Dubstep where

songDecoder :: String -> String
songDecoder = recurse ""
                where 
                    recurse :: String -> String -> String
                    recurse acc str@(s:ss) = if (startsWith "WUB" str)
                                                then recurse (appendSingleSpace acc) $ drop 3 str 
                                                else recurse (s:acc) ss
                    recurse acc [] = (trim . reverse . trim) acc

                    appendSingleSpace :: String -> String
                    appendSingleSpace (' ':acc) = ' ':acc
                    appendSingleSpace acc = ' ':acc


                    trim :: String -> String
                    trim (' ':ss) = ss
                    trim str = str


startsWith :: Eq a => [a] -> [a] -> Bool
startsWith target = (==) target . take l
                        where
                            l = if (length target == 0) then 1 else length target


