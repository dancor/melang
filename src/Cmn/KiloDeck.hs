{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmn.KiloDeck where

import Control.DeepSeq
import Data.Char
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

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

kiloDir :: FilePath
kiloDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"

loadKiloDeck :: FilePath -> IO [KiloLine]
loadKiloDeck = fmap (map readKiloLine . DT.lines) . DTI.readFile

type KiloDeck = [KiloLine]
