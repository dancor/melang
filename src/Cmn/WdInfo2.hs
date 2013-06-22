{-# LANGUAGE OverloadedStrings #-}

module Cmn.WdInfo2 where

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

import BSUtil

-- "Word Info": Google Books word frequencies with dictionary entries.
-- Line format:
--   freqPerMillionWords<tab>
--   simplifiedChineseWord<tab>
--   PartOfSpeechAbbr<tab>
--   pinyin1NoSpaces \ pinyin2 \ etc<tab>
--   def1 \ def2 \ etc
wdInfoFile :: String
wdInfoFile = "/home/danl/p/l/melang/data/wdInfo"

type Wd = DT.Text

data WdInfo = WdInfo
    { wiNumPerMillion :: !Float
    , wiN             :: !Int
    , wiWd            :: !Wd
    , wiDef           :: !DT.Text
    } deriving (Eq, Ord, Show)

instance NFData WdInfo where
    rnf (WdInfo a b c d) =
        rnf a `seq` rnf b `seq` rnf c `seq` rnf d

parseWdInfoLine :: Int -> BS.ByteString -> WdInfo
-- parseWdInfoLine n str = WdInfo (error "numPerMillion todo") n wd def
parseWdInfoLine n str = WdInfo 0 n wd def
  where
    {-
    fixPinyins = zipWith fixPinyin (DT.unpack wd)
    fixPinyin zh py =
        -- Characters with varying tone or elision, even within words:
        -- TODO: Should we also try to treat 3->2 tone sandhi?
        case zh of
          '一' -> "yi421"
          '不' -> "bu42"
          '儿' -> "{er2}"
          _ -> py
    -}
    (col1, afterCol1) = breakTab str
    (_col2, colRest)   = breakTab afterCol1
    wd  = DTE.decodeUtf8 col1
    def = DTE.decodeUtf8 colRest
