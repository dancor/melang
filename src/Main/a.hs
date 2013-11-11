import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as DT

import Cmn.KiloDeck

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x : xs

pinyinParts :: String -> [String]
pinyinParts [] = []
pinyinParts (c:rest) =
  if isAlpha c
    then onHead (c :) $ pinyinParts rest
    else [c] : pinyinParts rest

wdChars :: KiloLine -> [(Char, String)]
wdChars l = zip (DT.unpack $ kLWord l)
    (pinyinParts . filter (\c -> isAlpha c || isDigit c) .
    DT.unpack . DT.toLower . killNum $ kLPinyin l)

makeTable :: [String] -> [String]
makeTable = map (intercalate "\t" . map (\(a, b) -> a ++ b)) .
    groupBy ((==) `on` fst) . map partify
  where
    partify :: String -> (String, String)
    partify = break (`elem` "aeiou")

main :: IO ()
main = do
    dict <- readKiloDeck "/home/danl/p/l/melang/data/cmn/dict"
    let pyToChars = Map.fromListWith (flip Map.union) .
            map 
            (\(l, (char, py)) -> (filter isAlpha py, Map.singleton char l)) $
            concatMap (\l -> map ((,) l) $ wdChars l) dict
    mapM_ putStrLn .
        --map (\(py, n) -> show n ++ "\t" ++ py) .
        makeTable .
        map fst .
        Map.toList . Map.map Map.size $
        Map.filter ((> 1) . Map.size) pyToChars
    {-
    mapM_ putStrLn .
        map (\(py, l) -> py ++ ":\t" ++ show (kLNum l) ++ "\t" ++
            DT.unpack (showKiloLine l)) .
        sortBy (compare `on` kLNum . snd) .
        map (\(py, [(_, l)]) -> (py, l)) .
        Map.toList . Map.map Map.toList $
        Map.filter ((== 1) . Map.size) pyToChars
    -}
