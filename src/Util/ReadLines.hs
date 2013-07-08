module Util.ReadLines where

import qualified Data.ByteString as BS
import System.IO
--import System.IO.Error

readLines :: FilePath -> IO [BS.ByteString]
readLines f = withFile f ReadMode hReadLines

hReadLines :: Handle -> IO [BS.ByteString]
hReadLines h =
    liftM2 (:) (BS.hGetLine h) (hReadLines h)
    `catch`
    (\e -> if isEOFError e then return [] else ioError e)
