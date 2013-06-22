{-# LANGUAGE OverloadedStrings #-}

module Cmn.WdInfo where

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

import BSUtil

-- "Word Info": Google Books words with CEDICT entries.
-- Line format:
--   lineNumber<tab>
--   freqPerMillionWords<tab>
--   simplifiedChineseWord<tab>
--   PartOfSpeechAbbr<tab>
--   [pinyin1WithSpaces] /def1a/def1b/etc/; [pinyin2WithSpaces] /etc/
wdInfoFile :: String
wdInfoFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

type Wd = DT.Text

data PyDef = PyDef
    { pdPy   :: ![DT.Text]
    , pdDef  :: !DT.Text
    , pdFreq :: !Float
    } deriving (Eq, Ord, Show)

data WdInfo = WdInfo
    { wiNumPerMillion :: !Float
    , wiN             :: !Int
    , wiWd            :: !Wd
    , wiPartOfSpeech  :: !DT.Text
    , wiDef           :: ![PyDef]
    } deriving (Eq, Ord, Show)

instance NFData PyDef where
    rnf (PyDef a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData WdInfo where
    rnf (WdInfo a b c d e) =
        rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

parseWdInfoLine :: Int -> BS.ByteString -> WdInfo
parseWdInfoLine n str =
    WdInfo (read $ BSC.unpack a) n wd (DTE.decodeUtf8 c) .
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
