{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmn.Centudeck where

import Control.DeepSeq
import qualified Data.Text as DT

-- | Example: Centuline "的" "de5" "PRT:of"
data Centuline = Centuline
    { clWord   :: !DT.Text
    , clPinyin :: !DT.Text
    , clGloss  :: !DT.Text
    } deriving (Show)

instance NFData Centuline where
    rnf (Centuline a b c) = rnf a `seq` rnf b `seq` rnf c

onClPinyin :: (DT.Text -> DT.Text) -> Centuline -> Centuline
onClPinyin f cl = cl {clPinyin = f $ clPinyin cl}

readCentuline :: DT.Text -> Centuline
readCentuline s =
    case DT.splitOn "\t" s of
      [w, p, g] -> Centuline w p g
      x -> error $ "readCentuline: " ++ show x

showCentuline :: Centuline -> DT.Text
showCentuline (Centuline w p g) = DT.intercalate "\t" [w, p, g]

type Centudeck = [Centuline]
