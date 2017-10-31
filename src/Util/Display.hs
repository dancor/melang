module Util.Display where

max0 :: (Num a, Ord a) => [a] -> a
max0 l = if null l then 0 else maximum l

{-
-- Insert spaces after columns
colSpace :: [[String]] -> [[String]]
-}

spaceTable :: [[String]] -> [String]
spaceTable [] = []
spaceTable ([]:_) = []
-- One column, but deal gracefully with some being empty.
spaceTable t@([_]:_) = map (\xs -> if null xs then "" else head xs) t
spaceTable t = zipWith (++) (spaceBlock col) $ spaceTable rest where
  (col, rest) = unzip $ map (\ (x:xs) -> (x, xs)) t

-- rename to spaceCol?
spaceBlock :: [String] -> [String]
spaceBlock b = let
    lens = map length b
    w = max0 lens in
  zipWith (++) b $ map (\ l -> take (w - l) $ repeat ' ') lens

-- if you want to equally-space several blocks but keep them separate
spaceBlocks :: [[String]] -> [[String]]
spaceBlocks bs = let
    lenss = map (map length) bs
    w = max0 $ map max0 lenss in
  zipWith
   (\ b lens -> zipWith (++) b $ map (\ l -> take (w - l) $ repeat ' ') lens)
    bs lenss
