{-# LANGUAGE OverloadedStrings #-}

module GB1 where

import Control.DeepSeq
import qualified Data.Text as DT

-- | Example: Dictline "的" "PRT" 1680518088 1
data Dictline = Dictline
    { dlWord         :: !DT.Text
    , dlPartOfSpeech :: !DT.Text
    , dlOccurs       :: !Int
    , dlN            :: !Int
    } deriving (Show)

-- Actually the default instance is fine?
instance NFData Dictline where
    rnf (Dictline a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

readDictline :: Int -> DT.Text -> Dictline
readDictline n s =
    case DT.splitOn "\t" s of
      [w, p, o] -> Dictline w p (read $ DT.unpack o) n
      x -> error $ "readDictline: " ++ show x

showDictline :: Dictline -> DT.Text
showDictline (Dictline w p o n) =
    DT.intercalate "\t" [w, p, DT.pack $ show o, DT.pack $ show n]
