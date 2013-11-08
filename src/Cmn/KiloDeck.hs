{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmn.KiloDeck where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Char
import Data.List.Split
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.FilePath
import System.IO.Error

-- | Example: KiloLine 1 "的" "de5" "PRT:of"
data KiloLine = KiloLine
    { kLNum    :: !Int
    , kLWord   :: !DT.Text
    , kLPinyin :: !DT.Text
    , kLGloss  :: !DT.Text
    } deriving (Show)

instance NFData KiloLine

-- | Remove any prefix like "#1:", "#2:", ..
killNum :: DT.Text -> DT.Text
killNum = DT.pack . f . DT.unpack
  where
    f ('#':x) = dropWhile (== ':') $ dropWhile isDigit x
    f x = x

onKLPinyin :: (DT.Text -> DT.Text) -> KiloLine -> KiloLine
onKLPinyin f kL = kL {kLPinyin = f $ kLPinyin kL}

readKiloLine :: Int -> DT.Text -> KiloLine
readKiloLine n s =
    case DT.splitOn "\t" s of
      [w, p, g] -> KiloLine n w p g
      x -> error $ "readKiloLine: " ++ show n ++ ": " ++ show x

showKiloLine :: KiloLine -> DT.Text
showKiloLine (KiloLine _ w p g) = DT.intercalate "\t" [w, p, g]

kiloDictFile :: FilePath
kiloDictFile = "/home/danl/p/l/melang/data/cmn/dict"

kiloDeckDir :: FilePath
kiloDeckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"

readKiloDeck :: FilePath -> IO [KiloLine]
readKiloDeck = fmap (zipWith readKiloLine [1..] . DT.lines) . DTI.readFile

type KiloDeck = [KiloLine]

intToDeckName :: Int -> FilePath
intToDeckName n = "mando-gloss-" ++ show n ++ "k.txt"

seqWhileJust :: [IO (Maybe a)] -> IO [a]
seqWhileJust [] = return []
seqWhileJust (x:xs) = do
    rMb <- x
    case rMb of
      Nothing -> return []
      Just r -> (r:) <$> seqWhileJust xs

readDeckIfExists :: FilePath -> IO (Maybe KiloDeck)
readDeckIfExists f = (Just <$> readKiloDeck f) `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

readKiloDecks :: FilePath -> IO KiloDeck
readKiloDecks dir = concat <$>
    seqWhileJust (map (readDeckIfExists . (dir </>) . intToDeckName) [1..])

writeKiloDeck :: FilePath -> KiloDeck -> IO ()
writeKiloDeck f = DTI.writeFile f . DT.unlines . map showKiloLine

writeKiloDecks :: FilePath -> KiloDeck -> IO ()
writeKiloDecks dir = zipWithM_
    (\n -> writeKiloDeck (dir </> intToDeckName n)) [1..] . chunksOf 1000
