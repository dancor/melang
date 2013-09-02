{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Text as DT
import Data.Char
import Data.HashMap.Strict as HMS
import System.Directory
import System.FilePath

srcFile :: FilePath
srcFile = "/home/danl/p/l/melang/data/cmn/out"

outFile :: FilePath
outFile = "/home/danl/p/l/melang/data/cmn/out2"

data GLine = GLine
    { lWd           :: !DT.Text
    , lPartOfSpeech :: !DT.Text
    , lYr           :: !Int
    , lOccurs       :: !Int
    } deriving Show

parseLine :: Data.Text -> Maybe GLine
parseLine l =
    if DT.null wdUnd
      then Nothing
      else Just $ GLine wd partOfSpeech (read yrStr) (read occurStr)
  where
    wdP:yrStr:occursStr:_ = DT.splitOn "\t" l
    (wdUnd, partOfSpeech) = DT.breakOnEnd "_" wdP
    wd = DT.init wdUnd

main :: IO ()
main = do
    c <- DT.readFile srcFile
    DT.writeFile outFile .
        DT.unlines .
        map (DT.intercalate "\t") .
        map (\xs@(x:_) -> [lWd x, lPartOfSpeech x, sum $ map lOccurs xs]) .
        groupBy
            (\x y -> lWd x == lWd y && lPartOfSpeech x == lPartOfSpeech y) .
        filter (DT.any isIdeographic . lWd) .
        map parseLine .
        DT.lines c
