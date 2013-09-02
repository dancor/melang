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

{-
import BSUtil

data GLine a = GLine
    { lWd :: a
    , lPartOfSpeech :: a
    , lYr :: Int
    , lOccurs :: Int
    } deriving Show

parseLine :: BS.ByteString
          -> GLine BS.ByteString
parseLine s =
    GLine a (bsReadPosInt b) (bsReadPosInt c)
  where
    (a, s2) = breakTab s
    (b, s3) = breakTab s2
    (c, _) = breakTab s3

procLines :: Int -> [BS.ByteString] -> [BS.ByteString]
procLines earliestYearAllowed =
    map (\ (c, n) -> BSC.pack (show n) `BS.append` BS.cons 9 c) .
    map (\ g -> (lWd $ head g, sum $ map lOccurs g)) .
    groupBy ((==) `on` lWd) .
    filter ((>= earliestYearAllowed) . lYr) .
    map parseLine
-}

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
