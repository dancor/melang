module LangCmn where

import BSUtil
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

freqFile :: String
freqFile = "/home/danl/p/l/melang/data/ngrams/20120701/cmn/out/1980"

cedictFile :: String
cedictFile = "/home/danl/l/l/z/cedict/dict"

defsFile :: String
defsFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

data FreqLine = FreqLine
    { fRank          :: Int
    , fNumPerMillion :: Float
    , fWd            :: DT.Text
    , fPartOfSpeech  :: DT.Text
    } deriving Show

data CedictLine = CedictLine
    { cTrad :: DT.Text
    , cSimp :: DT.Text
    , cDef  :: DT.Text
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

