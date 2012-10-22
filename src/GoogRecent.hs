import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function
import Data.List
import System.Environment

import BSUtil

data GLine a = GLine
    { lWd :: a
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

main :: IO ()
main = do
    [earliestYearAllowedStr] <- getArgs
    bsInteractLErr $ map Right . procLines (read earliestYearAllowedStr)
