{-# LANGUAGE OverloadedStrings #-}

module GB1 where

import Control.DeepSeq
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

-- | Example: Dictline "的" "PRT" 1680518088 1
data DictLine = DictLine
    { dlWord         :: !DT.Text
    , dlPartOfSpeech :: !DT.Text
    , dlOccurs       :: !Int
    , dlN            :: !Int
    } deriving (Show)

instance NFData DictLine

type Dict = [DictLine]

readDict :: FilePath -> IO Dict
readDict = fmap (zipWith readDictLine [1..] . DT.lines) . DTI.readFile

readDictLine :: Int -> DT.Text -> DictLine
readDictLine n s =
    case DT.splitOn "\t" s of
      [w, p, o] -> DictLine w p (read $ DT.unpack o) n
      x -> error $ "readDictline: " ++ show x

showDictLine :: DictLine -> DT.Text
showDictLine (DictLine w p o n) =
    DT.intercalate "\t" [w, p, DT.pack $ show o, DT.pack $ show n]
