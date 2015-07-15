{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- Combine alternate form words. Currently only for German, for the 1996
-- spelling reform.

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Function
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Ratio
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import Safe
import System.Environment
import System.IO
import System.Process

import Util.DT
import Util.Process

type Row = [DT.Text]

data WdInfo
    = WdInfo
    { _wOccurs :: !Int
    , _wSpPart :: ![DT.Text]
    , _wSpPartFreq :: ![Int]
    }
    deriving (Eq, Show)

parseWdInfo :: Row -> WdInfo
parseWdInfo [occurCol, spPartCol, spPartFreqCol] = WdInfo
    (read $ DT.unpack occurCol)
    (DT.split (== '/') spPartCol)
    (map (read . DT.unpack) $ DT.split (== '/') spPartFreqCol)
parseWdInfo _ = error "parseWdInfo: bad input"

unparseWdInfo :: WdInfo -> Row
unparseWdInfo (WdInfo o s f) = [DT.pack $ show o, DT.intercalate "/" s,
    DT.intercalate "/" $ map (DT.pack . show) f]

type CompCount = (Int, HMS.HashMap DT.Text Int)

wdInfoToCompCount :: WdInfo -> CompCount
wdInfoToCompCount (WdInfo o s f) = (o, HMS.fromList . zip s $ map (o *) f)

combineCompCounts :: [CompCount] -> CompCount
combineCompCounts =
    foldl' (\(o, m) -> (o +) *** HMS.unionWith (+) m) (0, HMS.empty)

compCountToWdInfo :: CompCount -> WdInfo
compCountToWdInfo (o, m) = WdInfo o s $ map (round . (% o)) oTimesF
  where
    (s, oTimesF) = unzip . sortBy (flip compare `on` snd) . HMS.toList $
        HMS.filter (>= 10 * o) m

combineSameWord :: Monad m => Conduit Row m Row
combineSameWord =
    CL.groupBy ((==) `on` headNote "combineSameWord") =$= CL.map spPartProc
  where
    spPartProc :: [Row] -> Row
    spPartProc [r] = r
    spPartProc rs@((wd:_):_) = (wd :) . unparseWdInfo . compCountToWdInfo .
        combineCompCounts $ map (wdInfoToCompCount . parseWdInfo . tail) rs
    spPartProc _ = error "spPartProc got impossible input"

tryRepl :: HMS.HashMap DT.Text DT.Text -> [DT.Text] -> [DT.Text]
tryRepl !m x@(x0:xs) = case HMS.lookup x0 m of
  Just x0Repl -> x0Repl:xs
  _ -> x
tryRepl _ _ = error "tryRepl: bad input"

main :: IO ()
main = do
    args <- getArgs
    let dataSetName = case args of
          [x] -> x
          _ -> error "usage: proc-raw-gb-data ger"
        (altFormsFile, inFile, outFile) = case dataSetName of
          "ger" ->
            ( "/home/danl/data/wikt/de/alt-forms.txt"
            , "/home/danl/data/goog-ngrams/cur/ger/wds-alts.txt"
            , "/home/danl/data/goog-ngrams/cur/ger/wds.txt"
            )
          _ -> error "unknown data-set"

    altToRepls <- HMS.fromList . map (\(x:y:_) -> (x, y)) .
        map (DT.split (== '\t')) . filter (DT.all (/= ' ')) . DT.lines <$>
        DTI.readFile altFormsFile

    (sort1In, sort1Out, _) <-
        procPassErr "sort" ["-f", "-t", "\t", "-k", "1,1"]
    runResourceT (CC.sourceFile inFile
        $$ conduitLines (CL.map (showCols . tryRepl altToRepls . readCols))
        =$ CC.sinkHandle sort1In) >> hClose sort1In

    (sort2In, sort2Out, sort2Proc) <-
        procPassErr "sort" ["-t", "\t", "-k", "2,2nr", "-o", outFile]
    hPassErr sort2Out
    runResourceT (CC.sourceHandle sort1Out
        $$ conduitLines (CL.map readCols =$ combineSameWord =$ CL.map showCols)
        =$ CC.sinkHandle sort2In) >> hClose sort2In
    void $ waitForProcess sort2Proc
