{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Centudeck

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
   
centuprepPinyin :: Centudeck -> Centudeck
centuprepPinyin = map (onClPinyin prepPinyin)

-- | Remove any prefix like "#1:", "#2:", ..
killNum :: DT.Text -> DT.Text
killNum = DT.pack . f . DT.unpack
  where
    f ('#':x) = dropWhile (== ':') $ dropWhile isDigit x
    f x = x

prefNum :: Int -> DT.Text
prefNum n = DT.pack ('#' : show n ++ ":")

centuprepPinyinUniq :: Centudeck -> Centudeck
centuprepPinyinUniq = snd . foldl' f (Map.empty, [])
  where
    f (!seen, !cDeck) cLine =
        ( Map.insertWith (+) pinyin (1 :: Int) seen
        , cDeck ++ [cLine']
        )
      where
        pinyin = killNum $ clPinyin cLine
        cLine' = case Map.lookup pinyin seen of
          Just n -> cLine {
            clPinyin = prefNum (n + 1) `DT.append` pinyin}
          _ -> cLine

centuprepGlossUniq :: Centudeck -> Centudeck
centuprepGlossUniq = snd . foldl' f (Map.empty, [])
  where
    f (!seen, !cDeck) cLine =
        ( Map.insertWith (+) gloss (1 :: Int) seen
        , cDeck ++ [cLine']
        )
      where
        gloss = prepGloss . killNum $ clGloss cLine
        cLine' = case Map.lookup gloss seen of
          Just n -> cLine {
            clGloss = prefNum (n + 1) `DT.append` gloss}
          _ -> cLine

centuprep :: Centudeck -> Centudeck
centuprep = centuprepGlossUniq . centuprepPinyinUniq . centuprepPinyin

main :: IO ()
main = do
    let deckF = "/home/danl/p/l/melang/data/cmn/centudeck/deck.txt"
    deckOut <-
        DT.unlines . map showCentuline .
        centuprep . map readCentuline . DT.lines <$> DTI.readFile deckF
    
    -- Why, on file format error, does outputing not kill the file but
    -- rnf does?
    --let _ = rnf deckOut
    DTI.putStr deckOut

    DTI.writeFile deckF deckOut
    {-
    args <- getArgs
    case args of
      [] -> do
        c <- map readCentuline . DT.lines <$> DTI.getContents
        DTI.putStr . DT.unlines . map showCentuline $ centuprep c
      _ -> do
        putStrLn
            "Usage: cat deck-tsv.txt | ./centuprep > prepped-deck-tsv.txt"
        putStrLn "Filters pinyin and numbers repeated glosses."
    -}
