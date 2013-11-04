module Cmn.GoogBk where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

import Util.BS

-- Frequencies from books in Google Books published since 1980.
data GWdInfo = GWdInfo
    { gWd :: !DT.Text
    , gPos :: !DT.Text
    , gOccurs :: !Int
    } deriving Show

type GWdInfos = [GWdInfo]

parseGWdInfo :: BS.ByteString -> GWdInfo
parseGWdInfo line =
    GWdInfo (DTE.decodeUtf8 a) (DTE.decodeUtf8 b) (read $ BSC.unpack c)
  where
    (a, bAndC) = breakTab line
    (b, c) = breakTab bAndC

recentFile :: String
recentFile = "/home/danl/p/l/melang/data/cmn/gb-rec"

loadGoogBk :: IO [GWdInfo]
loadGoogBk = map parseGWdInfo . BSC.lines <$> BS.readFile recentFile
