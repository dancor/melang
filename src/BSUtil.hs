module BSUtil where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char
import System.IO

breakTab :: BS.ByteString -> (BS.ByteString, BS.ByteString)
breakTab = second BS.tail . BS.break (== 9)

bsReadPosInt :: BS.ByteString -> Int
bsReadPosInt =
    foldl1 ((+) . (10 *)) . map ((subtract $ ord '0') . fromIntegral) .
    BS.unpack

bslToBs :: BSL.ByteString -> BS.ByteString
bslToBs = BS.concat . BSL.toChunks

bsInteractLErr ::
    ([BS.ByteString] -> [Either BS.ByteString BS.ByteString]) ->
    IO ()
bsInteractLErr f = do
    ls <- f . map bslToBs . BSLC.lines <$> BSL.getContents
    mapM_ (either (BSC.hPutStrLn stderr) BSC.putStrLn) ls
