{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.FilePath

import Cmn.Dict

-- | Remove any prefix like "#1:", "#2:", ..
killNum :: DT.Text -> DT.Text
killNum x
  | "#" `DT.isPrefixOf` x =
    DT.dropWhile (== ':') . DT.dropWhile isDigit $ DT.drop 1 x
  | otherwise = x

kiloDeckDir :: FilePath
kiloDeckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"

intToDeckName :: Int -> FilePath
intToDeckName n = "mando-gloss-" ++ show n ++ "k.txt"

writeKiloDeck :: FilePath -> Dict -> IO ()
writeKiloDeck f = DTI.writeFile f . DT.unlines . map (\entry ->
    eWord entry <> "\t" <> ePinyin entry <> "\t" <> eGloss entry)

writeKiloDecks :: FilePath -> Dict -> IO ()
writeKiloDecks dir = zipWithM_
    (\n -> writeKiloDeck (dir </> intToDeckName n)) [1..] . chunksOf 1000

prefNum :: Int -> DT.Text
prefNum n = DT.pack ('#' : show n ++ ":")

deDupePinyins :: Dict -> Dict
deDupePinyins = reverse . snd . foldl' f (Map.empty, [])
  where
    f (!seen, !dict) entry =
        ( Map.insertWith (+) pinyin (1 :: Int) seen
        , entry':dict
        )
      where
        pinyin = ePinyin entry
        entry' = if pinyin == "?" then entry else
            case Map.lookup pinyin seen of
              Just n -> entry {ePinyin = prefNum (n + 1) <> pinyin}
              _ -> entry

glossIsEmpty :: DT.Text -> Bool
glossIsEmpty x = x == "?" || ":" `DT.isSuffixOf` x

deDupeGlosses :: Dict -> Dict
deDupeGlosses = reverse . snd . foldl' f (Map.empty, [])
  where
    f (!seen, !dict) entry =
        ( Map.insertWith (+) (DT.takeWhile (/= '\\') gloss) (1 :: Int) seen
        , entry':dict
        )
      where
        gloss = eGloss entry
        entry' = if glossIsEmpty gloss then entry else
            case Map.lookup gloss seen of
              Just n -> entry {eGloss = prefNum (n + 1) <> gloss}
              _ -> entry

entryKillNums :: DictEntry -> DictEntry
entryKillNums entry = entry
    { eGloss = killNum $ eGloss entry
    , ePinyin = killNum $ ePinyin entry
    }

main :: IO ()
main = do
    dict <- loadDict
    let dict' = deDupeGlosses $!! deDupePinyins $!! map entryKillNums dict
    writeDict $!! dict'
    writeKiloDecks kiloDeckDir $!!
        takeWhile (not . glossIsEmpty . eGloss) $!! dict'
