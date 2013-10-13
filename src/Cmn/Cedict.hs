{-# LANGUAGE OverloadedStrings #-}

module Cmn.Cedict where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

-- CEDICT.
-- Line format:
--   traditionalChineseWord<space>
--   simplifiedChinsesWord<space>
--   [pinyinWithSpaces]<space>
--   /def1/def2/etc/
cedictFile :: String
cedictFile =
    "/home/danl/p/l/melang/data/cmn/cedict/cedict_1_0_ts_utf-8_mdbg.txt"

data CedictLine = CedictLine
    { cTrad :: !DT.Text
    , cSimp :: !DT.Text
    , cDef  :: !DT.Text
    } deriving Show

parseCedictLine :: BS.ByteString -> CedictLine
parseCedictLine line =
    CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp)
        (DTE.decodeUtf8 def)
  where
    (trad, simpAndDef) = doSplit line
    (simp, def) = doSplit simpAndDef
    doSplit = second BS.tail . BS.breakByte (fromIntegral $ ord ' ')

loadCedict :: IO [CedictLine]
loadCedict =
    map (parseCedictLine . BSL.toStrict) .
    dropWhile ("#" `BSL.isPrefixOf`) . BSLC.lines <$>
    BSL.readFile cedictFile
