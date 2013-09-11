{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Centudeck
import GB1

main :: IO ()
main = do
    let deckF = "/home/danl/p/l/melang/data/cmn/centudeck/deck.txt"
        dictF = "/home/danl/p/l/melang/data/cmn/dict"
    deck <- map readCentuline . DT.lines <$> DTI.readFile deckF
    dict <-
        filter ((/= "一条") . dlWord) .
        zipWith readDictline [1..] . DT.lines <$> DTI.readFile dictF
    let deckWordSet = Set.fromList $ map clWord deck
        dictToDeck :: Dictline -> Centuline
        dictToDeck l =
            Centuline (dlWord l) "" (dlPartOfSpeech l `DT.append` ":")
        deckNext =
            take 40 .
            map dictToDeck $
            filter (not . (`Set.member` deckWordSet) . dlWord) dict

        wdToPos = HMS.fromList $ map (\l -> (dlWord l, dlOccurs l)) dict
        deck' =
            map snd .
            sortBy (flip $ comparing fst) $
            map (\x -> (fromJust $ HMS.lookup (clWord x) wdToPos, x)) deck
    DTI.writeFile deckF . DT.unlines . map showCentuline $ deck' ++ deckNext
