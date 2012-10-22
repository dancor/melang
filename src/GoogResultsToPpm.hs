{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import BSUtil
import SigFig

parseLine :: BS.ByteString
          -> (Int, BS.ByteString)
parseLine = first bsReadPosInt . breakTab

procLines :: [BS.ByteString] -> [BS.ByteString]
procLines ls =
    map (\ (n, w) -> BSC.pack n `BS.append` BS.cons 9 w)
    . map (first (\ n ->
        sigFig 5 (1000000 * fromIntegral n / fromIntegral totalWds)
        ))
    $ lsP
  where
    totalWds = sum $ map fst lsP
    lsP = map parseLine ls

main :: IO ()
main = bsInteractLErr $ map Right . procLines
