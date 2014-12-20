module Util.BS where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char
import System.IO
import System.IO.Error

breakTab :: BS.ByteString -> (BS.ByteString, BS.ByteString)
breakTab = second BS.tail . BS.break (== 9)

bsReadPosInt :: BS.ByteString -> Int
bsReadPosInt =
    foldl1 ((+) . (10 *)) . map ((subtract $ ord '0') . fromIntegral) .
    BS.unpack

{-
-- This uses hReadLines which is probably faster than BSL.toStrict.
bsInteractL :: ([BS.ByteString] -> [BS.ByteString]) -> IO ()
bsInteractL f = (f <$> hReadLines stdin) >>= mapM_ BSC.putStrLn
-}

bsInteractL ::
    ([BS.ByteString] -> [BS.ByteString]) ->
    IO ()
bsInteractL f = do
    ls <- f . map BSL.toStrict . BSLC.lines <$> BSL.getContents
    mapM_ BSC.putStrLn ls

bsInteractLErr ::
    ([BS.ByteString] -> [Either BS.ByteString BS.ByteString]) ->
    IO ()
bsInteractLErr f = do
    ls <- f . map BSL.toStrict . BSLC.lines <$> BSL.getContents
    mapM_ (either (BSC.hPutStrLn stderr) BSC.putStrLn) ls

bsInteractLErrIO ::
    ([BS.ByteString] -> IO [Either BS.ByteString BS.ByteString]) ->
    IO ()
bsInteractLErrIO f = do
    ls <- f =<< map BSL.toStrict . BSLC.lines <$> BSL.getContents
    mapM_ (either (BSC.hPutStrLn stderr) BSC.putStrLn) ls

readLines :: FilePath -> IO [BS.ByteString]
readLines f = withFile f ReadMode hReadLines

hReadLines :: Handle -> IO [BS.ByteString]
hReadLines h =
    liftM2 (:) (BS.hGetLine h) (hReadLines h)
    `catch`
    (\e -> if isEOFError e then return [] else ioError e)

hLinesFold :: (a -> BS.ByteString -> IO a) -> a -> Handle -> IO a
hLinesFold f a h = do
    lMb <- (Just <$> BS.hGetLine h) `catch`
        (\e -> if isEOFError e then return Nothing else ioError e)
    case lMb of
      Just l -> do
        a2 <- f a l
        hLinesFold f a2 h
      _ -> return a
