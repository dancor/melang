{-# LANGUAGE OverloadedStrings #-}

module Util.DT where

import Control.Arrow
import Control.Monad.Catch
import Data.Conduit
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as DT

breakOnCh :: Char -> DT.Text -> (DT.Text, DT.Text)
breakOnCh c = second DT.tail . DT.break (== c)

readCols :: DT.Text -> [DT.Text]
readCols = DT.split (== '\t')

showCols :: [DT.Text] -> DT.Text
showCols = DT.intercalate "\t"

-- Simpler implementations I tried were slower or corrupted characters.
conduitLines
    :: MonadThrow m
    => Conduit DT.Text m DT.Text
    -> Conduit BS.ByteString m BS.ByteString
conduitLines f =
    CT.decode CT.utf8 =$ CT.lines =$ f =$ CL.map (<> "\n") =$ CT.encode CT.utf8
