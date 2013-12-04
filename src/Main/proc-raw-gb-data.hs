{-# LANGUAGE OverloadedStrings #-}

-- Process the raw Google Books 1-grams data for various languages.
-- We specifically treat the part-of-speech-tagged data released in 2012.
--
-- We only consider the alphabetic files "a.gz" to "z.gz" and
-- not "0.gz" or "other.gz" or such. "Words" that start with numbers or
-- punctuation are not as interesting for learning vocabularly.
--
-- This currently works with "Chinese (Simplified)" or "English".
-- Adding other data-sets is simple.
--
-- We only consider Google Books published on or after 1980.
-- I'm not sure how much some languages may change when you look back
-- beyond that, but in a simple test the top 100 and top 200 Chinese words
-- (since 1980) are about equally common in the years before 1980 but quite
-- distinct after 1980:
-- - https://books.google.com/ngrams/
--   graph?content=或,成&year_start=1960&year_end=2000&corpus=23
--
-- For Chinese:
-- - Exclude words that contain no CJK characters (not "Chinese" words).
-- For English:
-- - Exclude words that contain no "a..z"/"A..Z" characters.
--
-- Then:
-- 1) Sum occurrences across years (drop year information).
-- 2) Order words by total occurrences (ignoring part-of-speech)
-- descending.
-- 3) For each word, show all part-of-speech possibilities >= 10%,
-- ordered by frequency descending.
--
-- For English:
-- - Combine words case-insensitively.

import Control.Arrow
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.IO as DTLI
import System.Environment
import System.FilePath

import Util.SciSigFig

data RawLine
    = RawLine
    { rWord   :: !DTL.Text
    , rSpPart :: !DTL.Text
    , rOccurs :: !Int
    }
    deriving (Show)

type MyMap = Map.Map DTL.Text (Map.Map DTL.Text Int)

tagSet :: Set.Set DTL.Text
tagSet = Set.fromList
    [ "ADJ", "ADP", "ADV", "CONJ", "DET", "NOUN", "NUM"
    , "PRON", "PRT", "VERB", "X"]

isCjkCommon :: Char -> Bool
isCjkCommon c = i >= 0x4E00 && i <= 0x9FCC where i = ord c

-- Only 17 "non-common" characters in GB1980.
isCjk :: Char -> Bool
isCjk c =
    i >= 0x4E00 && i <= 0x9FCC ||
    i >= 0x3400 && i <= 0x4DB5 ||
    i >= 0x20000 && i <= 0x2A6D6 ||
    i >= 0x2A700 && i <= 0x2B734 ||
    i >= 0x2B740 && i <= 0x2B81D
  where i = ord c

readRawLine :: DTL.Text -> Maybe RawLine
readRawLine l = case DTL.split (== '\t') l of
  [wordSpPart, yearStr, occursStr, _bkOccursStr] ->
      if DTL.all (not . isCjk) wordSpPart ||
          DTL.all (not . (== '_')) wordSpPart ||
          spPart `Set.notMember` tagSet ||
          year < (1980 :: Int)
        then Nothing
        else Just $ RawLine word spPart occurs
    where
      (wordUnd, spPart) = DTL.breakOnEnd "_" wordSpPart
      word = DTL.init wordUnd
      year = read $ DTL.unpack yearStr
      occurs = read $ DTL.unpack occursStr
  _ -> error "CSV format error."

readRawLines :: DTL.Text -> [RawLine]
readRawLines = catMaybes . map readRawLine . DTL.lines

collateRawLines :: [RawLine] -> MyMap
collateRawLines = Map.fromListWith (Map.unionWith (+)) .
    map (\x -> (rWord x, Map.singleton (rSpPart x) (rOccurs x)))

mapToResults :: MyMap -> [((DTL.Text, Int), [(DTL.Text, Int)])]
mapToResults = 
    sortBy (flip compare `on` snd . fst) . map doWordResult . Map.toList
  where
    doWordResult (word, m) =
        ( (word, totOccurs)
        , filter ((>= 10) . snd) $ map (second perC) spPartAndOccurs
        )
      where
        totOccurs = sum $ map snd spPartAndOccurs
        perC x = round $ x * 100 % totOccurs
        spPartAndOccurs =
            sortBy (flip compare `on` snd) $ Map.toList m

showResult :: Int -> ((DTL.Text, Int), [(DTL.Text, Int)]) -> DTL.Text
showResult grandTotOccurs ((word, wordOccurs), spPartLines) =
    DTL.intercalate "\t"
    [ word
    , DTL.pack . showN . round $ grandTotOccurs % wordOccurs
    , DTL.intercalate "/" spParts
    , DTL.intercalate "/" $ map (DTL.pack . show) spPartOccurs
    ]
  where
    (spParts, spPartOccurs) = unzip spPartLines

rawFiles :: String -> [FilePath]
rawFiles dataSetName =
    [ "/home/danl/data/goog-ngrams/20120701/1grams" </> dataSetName </>
      "googlebooks-" ++ dataSetName ++ "-all-1gram-20120701-" ++ [x] ++ ".gz"
    | x <- ['a'..'z']
    ]

main :: IO ()
main = do
    args <- getArgs
    let dataSetName = case args of
          [x] -> x
          _ -> error "usage"
    fields <- fmap concat . mapM (fmap readRawLines . DTLI.readFile) $
        rawFiles dataSetName
    let grandTotOccurs = sum $ map rOccurs fields
    DTLI.writeFile "out" . DTL.unlines . map (showResult grandTotOccurs) .
        mapToResults $! collateRawLines fields
