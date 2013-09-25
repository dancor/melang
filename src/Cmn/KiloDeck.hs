{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmn.KiloDeck where

import Control.DeepSeq
import qualified Data.Text as DT

-- | Example: KiloLine "的" "de5" "PRT:of"
data KiloLine = KiloLine
    { kLWord   :: !DT.Text
    , kLPinyin :: !DT.Text
    , kLGloss  :: !DT.Text
    } deriving (Show)

instance NFData KiloLine where
    rnf (KiloLine a b c) = rnf a `seq` rnf b `seq` rnf c

onKLPinyin :: (DT.Text -> DT.Text) -> KiloLine -> KiloLine
onKLPinyin f kL = kL {kLPinyin = f $ kLPinyin kL}

readKiloLine :: DT.Text -> KiloLine
readKiloLine s =
    case DT.splitOn "\t" s of
      [w, p, g] -> KiloLine w p g
      x -> error $ "readKiloLine: " ++ show x

showKiloLine :: KiloLine -> DT.Text
showKiloLine (KiloLine w p g) = DT.intercalate "\t" [w, p, g]

type KiloDeck = [KiloLine]
