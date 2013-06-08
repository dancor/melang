{-# LANGUAGE OverloadedStrings #-}

module LangCmn where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Text.Encoding as DTE

import BSUtil

-- Frequencies from books in Google Books published since 1980.
-- Line format:
--   freqPerMillionWords<tab>
--   simplifiedChineseWord_PartOfSpeechAbbr
freqFile :: String
freqFile = "/home/danl/p/l/melang/data/ngrams/20120701/cmn/out/1980"

-- CEDICT.
-- Line format:
--   traditionalChineseWord<space>
--   simplifiedChinsesWord<space>
--   [pinyinWithSpaces]<space>
--   /def1/def2/etc/
cedictFile :: String
cedictFile = "/home/danl/l/l/z/cedict/dict"

-- "Word Info": Google Books words with CEDICT entries.
-- Line format:
--   lineNumber<tab>
--   freqPerMillionWords<tab>
--   simplifiedChineseWord<tab>
--   PartOfSpeechAbbr<tab>
--   [pinyin1WithSpaces] /def1a/def1b/etc/; [pinyin2WithSpaces] /etc/
wdInfoFile :: String
wdInfoFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

data FreqLine = FreqLine
    { fRank          :: !Int
    , fNumPerMillion :: !Float
    , fWd            :: !DT.Text
    , fPartOfSpeech  :: !DT.Text
    } deriving Show

data CedictLine = CedictLine
    { cTrad :: !DT.Text
    , cSimp :: !DT.Text
    , cDef  :: !DT.Text
    } deriving Show

parseFreqLine :: Int -> BS.ByteString -> FreqLine
parseFreqLine n str =
    FreqLine n (read $ BSC.unpack a) (DTE.decodeUtf8 b) (DTE.decodeUtf8 c)
  where
    (a, bAndC) = breakTab str
    (b, c) = second BS.tail $ BS.breakByte (fromIntegral $ ord '_') bAndC

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine _n str =
    CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) (DTE.decodeUtf8 def)
  where
    (trad, simpAndDef) = doSplit str
    (simp, def) = doSplit simpAndDef
    doSplit = second BS.tail . BS.breakByte (fromIntegral $ ord ' ')

type Wd = DT.Text

data PyDef = PyDef
    { pdPy   :: ![DT.Text]
    , pdDef  :: !DT.Text
    , pdFreq :: !Float
    } deriving (Eq, Ord, Show)

data WdInfo = WdInfo
    { wiNumPerMillion :: !Float
    , wiWd            :: !Wd
    , wiPartOfSpeech  :: !DT.Text
    , wiDef           :: ![PyDef]
    } deriving (Eq, Ord, Show)

instance NFData PyDef where
    rnf (PyDef a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData WdInfo where
    rnf (WdInfo a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

parseWdInfoLine :: Int -> BS.ByteString -> WdInfo
parseWdInfoLine _n str =
    WdInfo (read $ BSC.unpack a) wd (DTE.decodeUtf8 c) .
    map (
        (\ (py, def) -> PyDef
            (fixPinyins . DT.words . DT.replace "u:" "v" $ DT.drop 1 py)
            (DT.drop 2 def)
            0
        ) .
        DT.break (== ']')) .
    DT.splitOn "; " $ DTE.decodeUtf8 d
  where
    fixPinyins = zipWith fixPinyin (DT.unpack wd)
    fixPinyin zh py =
        -- Characters with varying tone or elision, even within words:
        -- TODO: Should we also try to treat 3->2 tone sandhi?
        case zh of
          '一' -> "yi421"
          '不' -> "bu42"
          '儿' -> "{er2}"
          _ -> py
    wd = DTE.decodeUtf8 b
    (_weirdN, afterN) = breakTab str
    (a, afterA) = breakTab afterN
    (b, afterB) = breakTab afterA
    (c, d) = breakTab afterB
