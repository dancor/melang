{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Function
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Monoid
import System.Environment
import System.FilePath

import Util.BS
import Wikt.ProcDefs

main :: IO ()
main = do
    args <- getArgs
    lang <- case args of
      [arg] -> return arg
      _ -> error $ concat
        [ "usage e.g.: "
        , "/usr/bin/time < ~/data/wikt/spa ./wikt-to-defs spa > out"
        ]
    dict <- HMS.fromList .
        zipWith (\n (word:_freq:spPart:stats:_)  ->
            ( BSC.map toLower word
            , Entry word (Left "???") (spPart <> " " <> stats) n
            )) [1..] .
        map (BS.split 9) . BSC.lines <$>
        BS.readFile ("/home/danl/p/l/melang/data" </> lang </> "wds-100k")
    bsInteractLErr $ map Right .
        map (\e -> BS.intercalate "\t" [eWd e,
            showDef . onEachDefLine (derefVerb dict) $ eDef e, eStats e]) .
        sortBy (compare `on` eN) . HMS.elems .
        procLines dict
