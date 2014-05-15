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

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Util as CU
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

data Lang = Eng | Chi

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
        Eng -> DT.all (not . isAlpha)
        Chi -> DT.all (not . isCjk)
      (wordUnd, spPart) = DT.breakOnEnd "_" wordSpPart
      word = DT.init wordUnd
      year = read $ DT.unpack yearStr
      occurs = read $ DT.unpack occursStr
  _ -> error "CSV format error."

rawLineCols :: RawLine -> [DT.Text]
rawLineCols (RawLine word spPart occurs) =
    [word, spPart, DT.pack $ show occurs]

rawFiles :: String -> [(String, FilePath)]
rawFiles dataSetName =
    [ ( x
      , "/home/danl/data/goog-ngrams/20120701/1grams" </> dataSetName </>
        "googlebooks-" ++ dataSetName ++ "-all-1gram-20120701-" ++ x ++ ".gz"
      )
    -- | x <- map (:[]) ['a'..'z']
    | x <- map (:[]) ['r']
    ]

sumSameWordSpPart :: Monad m => Conduit RawLine m RawLine
sumSameWordSpPart =
    CL.groupBy (\x y ->
        DT.toLower (rWord x) == DT.toLower (rWord y) &&
        rSpPart x == rSpPart y)
    =$= CL.map (\xs@(x:_) ->
        RawLine (rWord x) (rSpPart x) (sum $ map rOccurs xs))

showInt :: Int -> String
showInt = show

combineSameWord :: Monad m => Conduit RawLine m (Integer, [DT.Text])
combineSameWord =
    CL.groupBy ((==) `on` DT.toLower . rWord)
    =$= CL.map spPartProc
  where
    -- Take the capitalization which is most common, over all
    -- possibilites.
    takeMostCommon :: [(DT.Text, Int)] -> DT.Text
    takeMostCommon = fst . maximumBy (compare `on` snd) .
        Map.toList . Map.fromListWith (+)

    -- Consider spPart occurs as a percentage of the total occurs for the
    -- word, and only show those with >= 10%.
    takeCommon :: Int -> [(DT.Text, Int)] -> ([DT.Text], [Ratio Int])
    takeCommon totPoss = unzip . sortBy (flip compare `on` snd) .
        filter ((>= 1 % 10) . snd) . map (second (% totPoss)) .
        Map.toList . Map.fromListWith (+)

    spPartProc :: [RawLine] -> (Integer, [DT.Text])
    spPartProc rs = (,) (fromIntegral rsOccursSum)
        [ takeMostCommon $ map (\r -> (rWord r, rOccurs r)) rs
        , DT.pack $ show rsOccursSum
        , DT.intercalate "/" commonSpParts
        , DT.intercalate "/" $
            map (DT.pack . showInt . round . (* 100)) commonSpPartFreqs
        ]
      where
        rsOccursSum = sum $ map rOccurs rs
        (commonSpParts, commonSpPartFreqs) =
            takeCommon rsOccursSum $ map (\r -> (rSpPart r, rOccurs r)) rs

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
    

doFile :: Lang -> Handle -> (String, FilePath) -> IO Integer
doFile lang bigSortIn (_name, fp) = do
    --(_, zOut, _, _) <- runInteractiveProcess "zcat" [fp] Nothing (Just [])
    (zIn, zOut, zErr, _) <- runInteractiveProcess "zgrep" ["-i", "^r", fp] Nothing (Just [])
    hClose zIn
    (sortIn, sortOut, sortErr, _) <- runInteractiveProcess "sort"
        ["-f", "-t", "\t", "-k", "1,1"] Nothing (Just [])
    _ <- doErr zErr
    _ <- doErr sortErr
    _ <- forkIO $ do
        runResourceT $ CB.sourceHandle zOut $$ CT.decode CT.utf8 =$ CT.lines
            =$ CL.mapMaybe (readRawLine lang)
            =$ sumSameWordSpPart
            =$ CL.map ((<> "\n") . showCols . rawLineCols)
            =$ CT.encode CT.utf8 =$ CB.sinkHandle sortIn
        hClose sortIn
    (totOccurs, ()) <- runResourceT $ CB.sourceHandle sortOut
        $$ CT.decode CT.utf8 =$ CT.lines
        =$ CL.map (colsToRawLine . readCols)
        =$ combineSameWord
        =$ CU.zipSinks
           (CL.map fst =$ CL.fold (+) 0)
           (CL.map ((<> "\n") . showCols . snd) =$ CT.encode CT.utf8
               =$ CB.sinkHandle bigSortIn)
    return totOccurs

main :: IO ()
main = do
    args <- getArgs
    let dataSetName = case args of
          [x] -> x
          _ -> error "usage"
        lang = case dataSetName of
          "chi-sim" -> Chi
          "eng" -> Eng
          "spa" -> Eng
          _ -> error "unknown data-set"
        myFiles = rawFiles dataSetName
    (bigSortIn, bigSortOut, bigSortErr, bigSortProc) <-
        runInteractiveProcess "sort"
        ["-t", "\t", "-k", "2,2nr", "-o", "word-list"] Nothing (Just [])
    _ <- doErr bigSortOut
    _ <- doErr bigSortErr
    totOccurs <- sum <$> mapM (doFile lang bigSortIn) myFiles
    hClose bigSortIn
    _ <- waitForProcess bigSortProc
    print totOccurs
