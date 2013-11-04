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

-- | Example: KiloLine "的" "de5" "PRT:of"
data KiloLine = KiloLine
    { kLWord   :: !DT.Text
    , kLPinyin :: !DT.Text
    , kLGloss  :: !DT.Text
    } deriving (Show)

instance NFData KiloLine where
    rnf (KiloLine a b c) = rnf a `seq` rnf b `seq` rnf c

-- | Remove any prefix like "#1:", "#2:", ..
killNum :: DT.Text -> DT.Text
killNum = DT.pack . f . DT.unpack
  where
    f ('#':x) = dropWhile (== ':') $ dropWhile isDigit x
    f x = x

onKLPinyin :: (DT.Text -> DT.Text) -> KiloLine -> KiloLine
onKLPinyin f kL = kL {kLPinyin = f $ kLPinyin kL}

readKiloLine :: DT.Text -> KiloLine
readKiloLine s =
    case DT.splitOn "\t" s of
      [w, p, g] -> KiloLine w p g
      x -> error $ "readKiloLine: " ++ show x

showKiloLine :: KiloLine -> DT.Text
showKiloLine (KiloLine w p g) = DT.intercalate "\t" [w, p, g]

kiloDeckDir :: FilePath
kiloDeckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"

loadKiloDeck :: FilePath -> IO [KiloLine]
loadKiloDeck = fmap (map readKiloLine . DT.lines) . DTI.readFile

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

loadDeckIfExists :: FilePath -> IO (Maybe KiloDeck)
loadDeckIfExists f = (Just <$> loadKiloDeck f) `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

loadKiloDecks :: FilePath -> IO KiloDeck
loadKiloDecks dir = concat <$>
    seqWhileJust (map (loadDeckIfExists . (dir </>) . intToDeckName) [1..])

writeKiloDeck :: FilePath -> KiloDeck -> IO ()
writeKiloDeck f = DTI.writeFile f . DT.unlines . map showKiloLine

writeKiloDecks :: FilePath -> KiloDeck -> IO ()
writeKiloDecks dir = zipWithM_
    (\n -> writeKiloDeck (dir </> intToDeckName n)) [1..] . chunksOf 1000
