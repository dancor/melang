{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Centudeck

-- | Example: Dictline "的" "PRT" 1680518088 1
data Dictline = Dictline
    { dlWord         :: !DT.Text
    , dlPartOfSpeech :: !DT.Text
    , dlOccurs       :: !Int
    , dlN            :: !Int
    } deriving (Show)

instance NFData Dictline where
    rnf (Dictline a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

readDictline :: Int -> DT.Text -> Dictline
readDictline n s =
    case DT.splitOn "\t" s of
      [w, p, o] -> Dictline w p (read $ DT.unpack o) n
      x -> error $ "readDictline: " ++ show x

showDictline :: Dictline -> DT.Text
showDictline (Dictline w p o n) =
    DT.intercalate "\t" [w, p, DT.pack $ show o, DT.pack $ show n]

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
