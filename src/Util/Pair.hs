module Util.Pair where

listToPair :: [a] -> (a, a)
listToPair [a, b] = (a, b)
listToPair x = error $ "listToPair: " ++ show (length x)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
