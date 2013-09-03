-- Combine counts from different years.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as DT
import qualified Data.Text.Lazy.IO as DTI
import Data.Char
-- import Data.Char.Properties
import Data.List
import Data.Maybe

-- srcFile :: FilePath
-- srcFile = "/home/danl/p/l/melang/data/cmn/out"

outFile :: FilePath
outFile = "/home/danl/p/l/melang/data/cmn/out2"

data GLine = GLine
    { lWd           :: !DT.Text
    , lPartOfSpeech :: !DT.Text
    , lYr           :: !Int
    , lOccurs       :: !Int
    } deriving Show

parseLine :: DT.Text -> Maybe GLine
parseLine l =
    if DT.null wdUnd || DT.any isLower partOfSpeech
      then Nothing
      else
        Just $ GLine wd partOfSpeech (read $ DT.unpack yrStr)
        (read $ DT.unpack occursStr)
  where
    wdP:yrStr:occursStr:_ = DT.splitOn "\t" l
    (wdUnd, partOfSpeech) = DT.breakOnEnd "_" wdP
    wd = DT.init wdUnd

main :: IO ()
main = do
    -- c <- DTI.readFile srcFile
    c <- DTI.getContents
    -- DTI.writeFile outFile .
    DTI.putStr .
        DT.unlines .
        map (DT.intercalate "\t") .
        map (\xs@(x:_) ->
            [ lWd x, lPartOfSpeech x
            , DT.pack . show . sum $ map lOccurs xs]) .
        groupBy
            (\x y -> lWd x == lWd y && lPartOfSpeech x == lPartOfSpeech y) .

        -- filter (DT.any isIdeographic . lWd) .
        
        filter (DT.any isAscii . lWd) .
        filter ((>= 1980) . lYr) .

        catMaybes .
        map parseLine $
        DT.lines c
