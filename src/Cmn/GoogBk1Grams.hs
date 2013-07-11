module Cmn.GoogBk1Grams
    ( Entry(..)
    , load
    ) where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

import Util.BS

data Entry = Entry
    { rank          :: !Int
    , numPerMillion :: !Float
    , wd            :: !DT.Text
    , partOfSpeech  :: !DT.Text
    } deriving Show

parseEntry :: Int -> BS.ByteString -> Entry
parseEntry n str =
    Entry n (read $ BSC.unpack a) (DTE.decodeUtf8 b) (DTE.decodeUtf8 c)
  where
    (a, bAndC) = breakTab str
    (b, c) = second BS.tail $ BS.breakByte (fromIntegral $ ord '_') bAndC

-- Frequencies from books in Google Books published since 1980.
-- Line format:
--   freqPerMillionWords<tab>
--   simplifiedChineseWord_PartOfSpeechAbbr
recentFile :: String
recentFile = "/home/danl/p/l/melang/data/ngrams/20120701/cmn/out/1980"

load :: IO [Entry]
load = zipWith parseEntry [1..] . BSC.lines <$> BS.readFile recentFile
