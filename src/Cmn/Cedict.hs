module Cmn.Cedict where

import Control.Arrow
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

-- CEDICT.
-- Line format:
--   traditionalChineseWord<space>
--   simplifiedChinsesWord<space>
--   [pinyinWithSpaces]<space>
--   /def1/def2/etc/
cedictFile :: String
cedictFile = "/home/danl/l/l/z/cedict/dict"

data CedictLine = CedictLine
    { cTrad :: !DT.Text
    , cSimp :: !DT.Text
    , cDef  :: !DT.Text
    } deriving Show

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine _n str =
    CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) (DTE.decodeUtf8 def)
  where
    (trad, simpAndDef) = doSplit str
    (simp, def) = doSplit simpAndDef
    doSplit = second BS.tail . BS.breakByte (fromIntegral $ ord ' ')
