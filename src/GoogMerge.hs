-- Sum any consecutive repeat words.

import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function
import Data.List

import BSUtil
import SigFig

main :: IO ()
main = bsInteractLErr $ map Right
    . map (\ (n, w) -> BSC.pack (sigFig 5 n) `BS.append` BS.cons 9 w)
    . map (first sum . second head . unzip) . groupBy ((==) `on` snd)
    . map (first (read . BSC.unpack) . breakTab)
