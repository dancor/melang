{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment

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
      then partOfSpeech `DT.append` rest
      else error $ "Unknown part-of-speech: " ++ show partOfSpeech
  where
    (p1, rest) = DT.breakOn ":" g
    fixP "ADP" = "PREP"
    fixP "DET" = "PRON"
    fixP x = x
    partOfSpeech = fixP p1

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
        kLine' = if DT.null pinyin then kLine else
            case Map.lookup pinyin seen of
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
        kLine' = kLine {
            kLGloss = if ":" `DT.isSuffixOf` gloss then gloss else
              case Map.lookup gloss seen of
                Just n -> prefNum (n + 1) `DT.append` gloss
                _ -> gloss
        }

kiloKillNums :: KiloLine -> KiloLine
kiloKillNums (KiloLine wd py gloss) =
    KiloLine wd (killNum py) (killNum gloss)

kiloPrep :: KiloDeck -> KiloDeck
kiloPrep =
    kiloPrepGlossUniq . kiloPrepPinyinUniq . kiloPrepPinyin .
    map kiloKillNums

growDeck :: Map.Map DT.Text DT.Text -> Goog -> Int -> KiloDeck -> KiloDeck
growDeck _ _ 0 deck = deck
growDeck pronMap goog growSize deck = deck ++ deckNext
  where
    deckWordSet = Set.fromList $ map kLWord deck
    dictToDeck l = KiloLine w (fromMaybe "" $ Map.lookup w pronMap)
        (dlPartOfSpeech l `DT.append` ":")
      where w = dlWord l
    deckNext = take growSize . map dictToDeck $
        filter (not . (`Set.member` deckWordSet) . dlWord) goog

sortDeck :: Goog -> KiloDeck -> KiloDeck
sortDeck goog =
    map snd . sortBy (flip $ comparing fst) .
    map (\x -> (fromJust $ HMS.lookup (kLWord x) wdToPos, x))
  where
    wdToPos = HMS.fromList $ map (\l -> (dlWord l, dlOccurs l)) goog

main :: IO ()
main = do
    args <- getArgs
    growSize <- return $ case args of
      [] -> 0
      [n] -> read n
      _ -> error $ "Unknown grow-size: " ++ show args
    goog <- readGoog "/home/danl/p/l/melang/data/cmn/gb-rec"
    pronMap <- Map.fromList . map (breakOnCh '\t') . DT.lines <$>
        DTI.readFile "/home/danl/p/l/melang/data/cmn/pinyin"
    deck <- kiloPrep . sortDeck goog . growDeck pronMap goog growSize <$>
        loadKiloDecks kiloDeckDir
    writeKiloDecks kiloDeckDir $!! deck
