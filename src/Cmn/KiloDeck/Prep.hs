{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment
import System.FilePath
import System.IO.Error

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
prepGloss "ADP:" = "PREP:"
prepGloss "DET:" = "PRON:"
-- It's most common for NUM in the Google data to mean MEAS.
prepGloss "NUM:" = "MEAS:"
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
        kLine' = if ":" `DT.isSuffixOf` gloss then kLine else
            case Map.lookup gloss seen of
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

growDeck :: Dict -> Int -> KiloDeck -> KiloDeck
growDeck _ 0 deck = deck
growDeck dict growSize deck = deck ++ deckNext
  where
    deckWordSet = Set.fromList $ map kLWord deck
    dictToDeck l = KiloLine (dlWord l) "" (dlPartOfSpeech l `DT.append` ":")
    deckNext = take growSize . map dictToDeck $
        filter (not . (`Set.member` deckWordSet) . dlWord) dict

intToDeckName :: Int -> FilePath
intToDeckName n = "mando-gloss-" ++ show n ++ "k.txt"

seqWhileJust :: [IO (Maybe a)] -> IO [a]
seqWhileJust [] = return []
seqWhileJust (x:xs) = do
    rMb <- x
    case rMb of
      Nothing -> return []
      Just r -> (r:) <$> seqWhileJust xs

loadDeckIfExists :: FilePath -> IO (Maybe KiloDeck)
loadDeckIfExists f = (Just <$> loadKiloDeck f) `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

loadDecks :: FilePath -> IO KiloDeck
loadDecks dir = concat <$>
    seqWhileJust (map (loadDeckIfExists . (dir </>) . intToDeckName) [1..])

writeDeck :: FilePath -> KiloDeck -> IO ()
writeDeck f = DTI.writeFile f . DT.unlines . map showKiloLine

writeDecks :: FilePath -> KiloDeck -> IO ()
writeDecks dir = zipWithM_
    (\n -> writeDeck (dir </> intToDeckName n)) [1..] . chunksOf 1000

sortDeck :: Dict -> KiloDeck -> KiloDeck
sortDeck dict =
    map snd . sortBy (flip $ comparing fst) .
    map (\x -> (fromJust $ HMS.lookup (kLWord x) wdToPos, x))
  where
    wdToPos = HMS.fromList $ map (\l -> (dlWord l, dlOccurs l)) dict

main :: IO ()
main = do
    let deckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"
        dictF = "/home/danl/p/l/melang/data/cmn/dict"
    args <- getArgs
    growSize <- return $ case args of
      [] -> 0
      [n] -> read n
      _ -> error $ "Unknown grow-size: " ++ show args
    dict <- readDict dictF
    deck <- loadDecks deckDir
    writeDecks deckDir . kiloPrep . sortDeck dict $
        growDeck dict growSize deck
