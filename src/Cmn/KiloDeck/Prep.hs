{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.FilePath

import Cmn.KiloDeck
import GB1


prepPinyin :: DT.Text -> DT.Text
prepPinyin = DT.pack . f . DT.unpack
  where
    f [] = []
    f ('*':x) = f x
    f ('(':x) = f . drop 1 $ dropWhile (/= ')') x
    f ('[':x) = f . drop 1 $ dropWhile (/= ']') x
    f (';':_) = ""
    f (' ':_) = error "Pinyin cannot contain ' '!"
    f ('/':_) = error "Pinyin cannot contain '/'!"
    f ('\\':_) = error "Pinyin cannot contain '\\'!"
    f (c:x) = c : f x

prepGloss :: DT.Text -> DT.Text
prepGloss g =
    if partOfSpeech `elem`
        [ "ADJ", "ADV", "AUXV", "CONJ", "MEAS", "NOUN"
        , "NUM", "PREP", "PRON", "PRT", "VERB"
        ]
      then g
      else error $ "Unknown part-of-speech: " ++ show partOfSpeech
  where
    (partOfSpeech, _) = DT.breakOn ":" g
   
kiloPrepPinyin :: KiloDeck -> KiloDeck
kiloPrepPinyin = map (onKLPinyin prepPinyin)

prefNum :: Int -> DT.Text
prefNum n = DT.pack ('#' : show n ++ ":")

kiloPrepPinyinUniq :: KiloDeck -> KiloDeck
kiloPrepPinyinUniq = snd . foldl' f (Map.empty, [])
  where
    f (!seen, !kDeck) kLine =
        ( Map.insertWith (+) pinyin (1 :: Int) seen
        , kDeck ++ [kLine']
        )
      where
        pinyin = kLPinyin kLine
        kLine' = case Map.lookup pinyin seen of
          Just n -> kLine {
            kLPinyin = prefNum (n + 1) `DT.append` pinyin}
          _ -> kLine

kiloPrepGlossUniq :: KiloDeck -> KiloDeck
kiloPrepGlossUniq = snd . foldl' f (Map.empty, [])
  where
    f (!seen, !kDeck) kLine =
        ( Map.insertWith (+) gloss (1 :: Int) seen
        , kDeck ++ [kLine']
        )
      where
        gloss = prepGloss $ kLGloss kLine
        kLine' = case Map.lookup gloss seen of
          Just n -> kLine {
            kLGloss = prefNum (n + 1) `DT.append` gloss}
          _ -> kLine

kiloKillNums :: KiloLine -> KiloLine
kiloKillNums (KiloLine wd py gloss) =
    KiloLine wd (killNum py) (killNum gloss)

kiloPrep :: KiloDeck -> KiloDeck
kiloPrep =
    kiloPrepGlossUniq . kiloPrepPinyinUniq . kiloPrepPinyin .
    map kiloKillNums

{- To add new words:

    let deckWordSet = Set.fromList $ map kLWord deck
        dictToDeck :: Dictline -> KiloLine
        dictToDeck l =
            KiloLine (dlWord l) "" (dlPartOfSpeech l `DT.append` ":")
        deckNext = take 40 . map dictToDeck $
            filter (not . (`Set.member` deckWordSet) . dlWord) dict
    DTI.writeFile deckF . DT.unlines . map showKiloLine $ deck' ++ deckNext
-}

main :: IO ()
main = do
    -- Load dict for reordering purposes.
    let dictF = "/home/danl/p/l/melang/data/cmn/dict"
    dict <- filter ((/= "一条") . dlWord) .
        zipWith readDictline [1..] . DT.lines <$> DTI.readFile dictF
    let wdToPos = HMS.fromList $ map (\l -> (dlWord l, dlOccurs l)) dict

    let deckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"
        deckF = deckDir </> "mando-gloss-1k.txt"
    deck <- kiloPrep <$> loadKiloDeck deckF
    
    let deck' = map snd .
            sortBy (flip $ comparing fst) $
            map (\x -> (fromJust $ HMS.lookup (kLWord x) wdToPos, x)) deck

    let deckOut = DT.unlines $ map showKiloLine deck'
    -- Why, on file format error, does outputing not kill the file but
    -- rnf does?
    --let _ = rnf deckOut
    DTI.putStr deckOut
    DTI.writeFile deckF deckOut
