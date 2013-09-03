-- Combine words across capitalization differences.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as DT
import qualified Data.Text.Lazy.IO as DTI
import Data.Char
import Data.Function
import Data.List
import Data.Ord
import qualified Data.HashMap.Strict as HMS

{-
srcFile :: FilePath
srcFile = "/home/danl/p/l/melang/data/cmn/eng-dict-2"

outFile :: FilePath
outFile = "/home/danl/p/l/melang/data/cmn/eng-dict-3"
-}

data GLine = GLine
    { lWd           :: !DT.Text
    , lPartOfSpeech :: !DT.Text
    , lOccurs       :: !Int
    } deriving Show

parseLine :: DT.Text -> GLine
parseLine l =
    GLine wd partOfSpeech (read $ DT.unpack occursStr)
  where
    wd:partOfSpeech:occursStr:[] = DT.splitOn "\t" l

markSortUnmark :: (Ord b) => (a -> b) -> [a] -> [a]
markSortUnmark f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

markGroupUnmark :: (Ord b) => (a -> b) -> [a] -> [[a]]
markGroupUnmark f =
    map (map snd) . groupBy ((==) `on` fst) . map (\x -> (f x, x))

doGroup :: [GLine] -> GLine
doGroup xs =
    GLine (lWd x) (lPartOfSpeech x) (sum $ map lOccurs xs)
  where
    x = maximumBy (comparing lOccurs) xs

main :: IO ()
main = do
    -- c <- DTI.readFile srcFile
    c <- DTI.getContents
    -- DTI.writeFile outFile .
    DTI.putStr .
        DT.unlines .
        map (\x -> DT.intercalate "\t" [lWd x, lPartOfSpeech x,
            DT.pack (show $ lOccurs x)]) .
{-
        -- sortBy (flip $ comparing lOccurs) .
        HMS.elems .
        HMS.fromListWith (\l1 l2 -> l1 {lOccurs = lOccurs l1 + lOccurs l2}) .

        map (\x -> (map toLower . DT.unpack $ lWd x, x)) .
-}
        map doGroup .
        markGroupUnmark (DT.toLower . lWd) .

        -- filter (DT.any isAlpha . lWd) .

        map parseLine $
        DT.lines c
