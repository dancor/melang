module LangCmn where

import qualified Data.Text as DT

freqFile :: String
freqFile = "/home/danl/p/l/melang/data/ngrams/20120701/cmn/out/1980"

cedictFile :: String
cedictFile = "/home/danl/l/l/z/cedict/dict"

defsFile :: String
defsFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

data FreqLine = FreqLine
    { fRank           :: Int
    , fFreqPerMillion :: DT.Text
    , fWd             :: DT.Text
    , fPartOfSpeech   :: DT.Text
    } deriving Show

data CedictLine = CedictLine
    { cTrad :: DT.Text
    , cSimp :: DT.Text
    , cDef  :: DT.Text
    } deriving Show

