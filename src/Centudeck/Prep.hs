{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment

-- | Example: Centuline "的" "de5" "PRT:of"
data Centuline = Centuline
    { clWord   :: !DT.Text
    , clPinyin :: !DT.Text
    , clGloss  :: !DT.Text
    , clExtra  :: ![DT.Text]
    } deriving (Show)

onClPinyin :: (DT.Text -> DT.Text) -> Centuline -> Centuline
onClPinyin f cl = cl {clPinyin = f $ clPinyin cl}

readCentuline :: DT.Text -> Centuline
readCentuline s =
    case DT.splitOn "\t" s of
      w:p:g:extra -> Centuline w p g extra
      x -> error $ "readCentuline: " ++ show x

showCentuline :: Centuline -> DT.Text
showCentuline (Centuline w p g extra) = DT.intercalate "\t" (w:p:g:extra)

type Centudeck = [Centuline]

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
        gloss = killNum $ clGloss cLine
        cLine' = case Map.lookup gloss seen of
          Just n -> cLine {
            clGloss = prefNum (n + 1) `DT.append` gloss}
          _ -> cLine

centuprep :: Centudeck -> Centudeck
centuprep = centuprepGlossUniq . centuprepPinyinUniq . centuprepPinyin

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
        c <- map readCentuline . DT.lines <$> DTI.getContents
        DTI.putStr . DT.unlines . map showCentuline $ centuprep c
      _ -> do
        putStrLn
            "Usage: cat deck-tsv.txt | ./centuprep > prepped-deck-tsv.txt"
        putStrLn "Filters pinyin and numbers repeated glosses."
