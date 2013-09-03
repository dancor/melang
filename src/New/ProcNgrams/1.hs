-- Filter to get foreign words (containing non-Ascii) and do year cutoff.

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import System.Directory
import System.FilePath

earliestYearAllowed :: Int
earliestYearAllowed = 1980

srcDir :: FilePath
srcDir = "/home/danl/p/l/melang/data/cmn/google-ngrams/20120701/1grams/orig"

outFile :: FilePath
outFile = "/home/danl/p/l/melang/data/cmn/out"

main :: IO ()
main = do
    files <- filter ((/= '.') . head) <$> getDirectoryContents srcDir
    foreignLines <- fmap concat . forM files $ \file -> do
        ls <- BSC.lines <$> BS.readFile (srcDir </> file)
        return $ filter (BSC.any (not . isAscii)) ls
    let recentLines = filter ((>= earliestYearAllowed) . read . BSC.unpack .
            BS.takeWhile (/= 9) . BS.tail . BS.dropWhile (/= 9))
            foreignLines
    BS.writeFile outFile $ BSC.unlines recentLines
