module Util.Process where

import Control.Concurrent
import Control.Monad
import System.IO
import System.Process

hPassErr :: Handle -> IO ()
hPassErr h = void $ forkIO go where
    go = do
        eof <- hIsEOF h
        unless eof $ hGetLine h >>= hPutStrLn stderr >> go

procPassErr :: FilePath -> [String] -> IO (Handle, Handle, ProcessHandle)
procPassErr cmd args = do
    (pIn, pOut, pErr, pId) <- runInteractiveProcess cmd args Nothing (Just [])
    hPassErr pErr
    return (pIn, pOut, pId)
