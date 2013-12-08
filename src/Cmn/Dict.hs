{-# LANGUAGE OverloadedStrings #-}

module Cmn.Dict where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.HashMap.Strict as HMS

type Wd = DT.Text

data DictEntry = DictEntry
    { eN             :: !Int
    , eWord          :: !Wd
    , ePinyin        :: !Wd
    , eSpPartFreqs   :: !Wd
    , eSpParts       :: !Wd
    , eGloss         :: !Wd
    } deriving (Eq, Ord, Show)

instance NFData DictEntry

type Dict = [DictEntry]

type WdDict = HMS.HashMap Wd DictEntry

loadDict :: IO Dict
loadDict =
    zipWith readDictEntry [1..] . map (DT.split (== '\t')) .
    DT.lines <$>
    DTI.readFile "/home/danl/p/l/melang/data/cmn/dict"
  where
    readDictEntry n [wd, pinyin, spPartFreqs, spParts, gloss] =
        DictEntry n wd pinyin spPartFreqs spParts gloss
    readDictEntry n x =
        error $ "loadDict: invalid line " ++ show n ++ ": " ++ show x

writeDict :: Dict -> IO ()
writeDict =
    DTI.writeFile "/home/danl/p/l/melang/data/cmn/dict" . DT.unlines .
    map (\ (DictEntry _ wd pinyin spPartFreqs spParts gloss) ->
    DT.intercalate "\t" [wd, pinyin, spPartFreqs, spParts, gloss])

showEntry :: DictEntry -> DT.Text
showEntry (DictEntry n wd pinyin spPartFreqs spParts gloss) =
    DT.intercalate "\t"
    [DT.pack $ show n, wd, pinyin, spPartFreqs, spParts, gloss]

dictToWdDict :: Dict -> WdDict
dictToWdDict dict = HMS.fromList [(eWord entry, entry) | entry <- dict]
