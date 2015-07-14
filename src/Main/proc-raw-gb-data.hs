{-# LANGUAGE OverloadedStrings #-}

-- Process the raw Google Books 1-grams data for various languages.
-- We specifically treat the part-of-speech-tagged data released in 2012.
--
-- We only consider the alphabetic files "a.gz" to "z.gz" and
-- not "0.gz" or "other.gz" or such. Words that start with numbers or
-- punctuation are not as interesting for learning vocabularly.
--
-- I use this currently for data-sets "chi-sim" and "eng".
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
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio
import qualified Data.Set as Set
import qualified Data.Text as DT
import System.Environment
import System.FilePath
import System.IO
import System.Process

data RawLine
    = RawLine
    { rWord   :: !DT.Text
    , rSpPart :: !DT.Text
    , rOccurs :: !Int
    }
    deriving (Eq, Show)

isCjk :: Char -> Bool
isCjk c =
    i >= 0x4E00 && i <= 0x9FCC ||
    i >= 0x3400 && i <= 0x4DB5 ||
    i >= 0x20000 && i <= 0x2A6D6 ||
    i >= 0x2A700 && i <= 0x2B734 ||
    i >= 0x2B740 && i <= 0x2B81D
  where i = ord c

tagSet :: Set.Set DT.Text
tagSet = Set.fromList
    [ "ADJ", "ADP", "ADV", "CONJ", "DET", "NOUN", "NUM"
    , "PRON", "PRT", "VERB", "X"]

data Lang = Eng | Chi | Ger | Spa

showInt :: Int -> String
showInt = show

readCols :: DT.Text -> [DT.Text]
readCols = DT.split (== '\t')

showCols :: [DT.Text] -> DT.Text
showCols = DT.intercalate "\t"

readRawLine :: Lang -> DT.Text -> Maybe RawLine
readRawLine lang l = case readCols l of
  [wordSpPart, yearStr, occursStr, _bkOccursStr] ->
      if year < (1980 :: Int) ||
          spPart `Set.notMember` tagSet ||
          DT.null wordUnd ||
          langBadWord word
        then Nothing
        else Just $ RawLine word spPart occurs
    where
      langBadWord = case lang of
        Chi -> DT.all (not . isCjk)
        _   -> DT.all (not . isAlpha)
      (wordUnd, spPart) = DT.breakOnEnd "_" wordSpPart
      word = DT.init wordUnd
      year = read $ DT.unpack yearStr
      occurs = read $ DT.unpack occursStr
  _ -> error "CSV format error."

rawLineCols :: RawLine -> [DT.Text]
rawLineCols (RawLine word spPart occurs) =
    [word, spPart, DT.pack $ show occurs]

sumSameWordSpPart :: Monad m => Conduit RawLine m RawLine
sumSameWordSpPart =
    CL.groupBy (\x y ->
        DT.toLower (rWord x) == DT.toLower (rWord y) &&
        rSpPart x == rSpPart y)
    =$= CL.map (\xs@(x:_) ->
        RawLine (rWord x) (rSpPart x) (sum $ map rOccurs xs))

combineSameWord :: Monad m => Conduit RawLine m [DT.Text]
combineSameWord =
    CL.groupBy ((==) `on` DT.toLower . rWord) =$= CL.map spPartProc
  where
    useMostCommonCapitalization :: [(DT.Text, Int)] -> DT.Text
    useMostCommonCapitalization = fst . maximumBy (compare `on` snd) .
        Map.toList . Map.fromListWith (+)

    spPartTenPercentCutoff
        :: Int -> [(DT.Text, Int)] -> ([DT.Text], [Ratio Int])
    spPartTenPercentCutoff totPoss =
        unzip . sortBy (flip compare `on` snd) .
        filter ((>= 1 % 10) . snd) . map (second (% totPoss)) .
        Map.toList . Map.fromListWith (+)

    spPartProc :: [RawLine] -> [DT.Text]
    spPartProc rs =
        [ useMostCommonCapitalization $ map (\r -> (rWord r, rOccurs r)) rs
        , DT.pack $ show rsOccursSum
        , DT.intercalate "/" commonSpParts
        , DT.intercalate "/" $
            map (DT.pack . showInt . round . (* 100)) commonSpPartFreqs
        ]
      where
        rsOccursSum = sum $ map rOccurs rs
        (commonSpParts, commonSpPartFreqs) = spPartTenPercentCutoff
            rsOccursSum $ map (\r -> (rSpPart r, rOccurs r)) rs

colsToRawLine :: [DT.Text] -> RawLine
colsToRawLine [word, spPart, occurs] =
    RawLine word spPart (read $ DT.unpack occurs)
colsToRawLine x = error $ "Bad raw line: " ++ show x

doErr :: Handle -> IO ThreadId
doErr = forkIO . go
  where
    go h = do
        eof <- hIsEOF h
        unless eof $ do
            l <- hGetLine h
            hPutStrLn stderr l
            go h
    
doFile :: Lang -> Handle -> FilePath -> IO ()
doFile lang bigSortIn fp = do
    (zIn, zOut, zErr, _) <- runInteractiveProcess "zcat" [fp] Nothing (Just [])
    hClose zIn
    (sortIn, sortOut, sortErr, _) <- runInteractiveProcess "sort"
        ["-f", "-t", "\t", "-k", "1,1"] Nothing (Just [])
    _ <- doErr zErr
    _ <- doErr sortErr
    _ <- forkIO $ runResourceT (CC.sourceHandle zOut
        $$ CC.linesUnbounded
        =$ CL.mapMaybe (readRawLine lang)
        =$ sumSameWordSpPart
        =$ CL.map ((<> "\n") . showCols . rawLineCols)
        =$ CC.sinkHandle sortIn) >> hClose sortIn
    runResourceT $ CC.sourceHandle sortOut
        $$ CC.linesUnbounded
        =$ CL.map (colsToRawLine . readCols)
        =$ combineSameWord
        =$ CL.map ((<> "\n") . showCols)
        =$ CC.sinkHandle bigSortIn

main :: IO ()
main = do
    args <- getArgs
    let dataSetName = case args of
          [x] -> x
          _ -> error "usage, e.g.: proc-raw-gb-data spa"
        dataDir = "/home/danl/data/goog-ngrams/cur" </> dataSetName
        lang = case dataSetName of
          "chi-sim" -> Chi
          "ger" -> Ger
          "eng" -> Eng
          "spa" -> Spa
          _ -> error "unknown data-set"
        inFiles = map (\x -> dataDir </> "1grams/" ++ x ++ ".gz")
            -- ["a".."z"]
            ["d"]
        outFile = dataDir </> "wds.txt"
    (bigSortIn, bigSortOut, bigSortErr, bigSortProc) <- runInteractiveProcess
        "sort" ["-t", "\t", "-k", "2,2nr", "-o", outFile] Nothing (Just [])
    _ <- doErr bigSortOut
    _ <- doErr bigSortErr
    mapM_ (doFile lang bigSortIn) inFiles
    hClose bigSortIn
    void $ waitForProcess bigSortProc
