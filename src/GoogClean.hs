-- Convert words to lowercase and exclude some known non-words.

import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

import BSUtil

splitTagOff :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitTagOff = BSC.breakEnd (== '_')

toLowerBeforeTag :: BS.ByteString -> BS.ByteString
toLowerBeforeTag =
    uncurry BS.append .
    first (DTE.encodeUtf8 . DT.toLower . DTE.decodeUtf8) .
    splitTagOff

main :: IO ()
main = bsInteractLErr $ map Right
    . map (\ (n, w) -> n `BS.append` BS.cons 9 w)
    . map (second toLowerBeforeTag)
    . map breakTab

    {-
    runResourceT $ sourceHandle SIO.stdin $= decode utf8
    $=
    $= encode utf8 $$ sinkHandle SIO.stdout
    -}
