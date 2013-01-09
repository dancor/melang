module NGrams where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data NGram = NGram
    { ngFreq :: Double
    , ngWord :: BS.ByteString
    , ngPartOfSpeech :: BS.ByteString
    }

readNGrams :: FilePath -> IO [NGram]
readNGrams f = map readNGramLine . BSC.lines <$> BS.readFile f

readNGramLine :: BS.ByteString -> NGram
readNGramLine l = NGram (read $ BSC.unpack freqS) word partOfSpeech
  where
    (freqS, tabRest) = BSC.break (== '\t') l
    rest = BS.tail tabRest
    (wordUnderscore, partOfSpeech) = BSC.breakEnd (== '_') $ rest
    word = BS.init wordUnderscore
