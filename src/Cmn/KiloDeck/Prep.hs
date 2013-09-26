{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

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

main :: IO ()
main = do
    let deckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"

        /mando-gloss-1k.txt"
    deckOut <-
        DT.unlines . map showKiloLine .
        kiloPrep .
        map readKiloLine . DT.lines <$> DTI.readFile deckF
    
    -- Why, on file format error, does outputing not kill the file but
    -- rnf does?
    --let _ = rnf deckOut
    DTI.putStr deckOut

    DTI.writeFile deckF deckOut
    {-
    args <- getArgs
    case args of
      [] -> do
        c <- map readKiloLine . DT.lines <$> DTI.getContents
        DTI.putStr . DT.unlines . map showKiloLine $ kiloPrep c
      _ -> do
        putStrLn
            "Usage: cat deck-tsv.txt | ./kilo-prep > prepped-deck-tsv.txt"
        putStrLn "Filters pinyin and numbers repeated glosses."
    -}
