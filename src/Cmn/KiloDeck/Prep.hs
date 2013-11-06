{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as DT

import Cmn.KiloDeck

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
        , "X"
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

main :: IO ()
main = do
    dict <- kiloPrep <$> readKiloDeck kiloDictFile
    writeKiloDeck kiloDictFile $!! dict
    writeKiloDecks kiloDeckDir $!!
        takeWhile (not . (":" `DT.isSuffixOf`) . kLGloss) dict
