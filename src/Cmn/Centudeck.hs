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
    , clExtra  :: ![DT.Text]
    } deriving (Show)

instance NFData Centuline where
    rnf (Centuline a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

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

