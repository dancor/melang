{-# LANGUAGE OverloadedStrings #-}

module GB1 where

import Control.DeepSeq
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

-- | Example: GoogLine "的" "PRT" 1680518088 1
data GoogLine = GoogLine
    { dlWord         :: !DT.Text
    , dlPartOfSpeech :: !DT.Text
    , dlOccurs       :: !Int
    , dlN            :: !Int
    } deriving (Show)

instance NFData GoogLine

type Goog = [GoogLine]

readGoog :: FilePath -> IO Goog
readGoog = fmap (zipWith readGoogLine [1..] . DT.lines) . DTI.readFile

readGoogLine :: Int -> DT.Text -> GoogLine
readGoogLine n s =
    case DT.splitOn "\t" s of
      [w, p, o] -> GoogLine w p (read $ DT.unpack o) n
      x -> error $ "readGoogline: " ++ show x

showGoogLine :: GoogLine -> DT.Text
showGoogLine (GoogLine w p o n) =
    DT.intercalate "\t" [w, p, DT.pack $ show o, DT.pack $ show n]
