{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Centudeck

-- | Example: Centuline "的" "PRT" 1680518088 1
data Dictline = Dictline
    { dlWord         :: !DT.Text
    , dlPartOfSpeech :: !DT.Text
    , dlOccurs       :: !Int
    , dlN            :: !Int
    } deriving (Show)

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

main :: IO ()
main = do
    let deckF = "/home/danl/p/l/melang/data/cmn/deck.txt"
    deck <- map readCentuline . DT.lines <$> DTI.readFile deckF
    let deckWordSet = Set.fromList $ map clWord deck
        dictF = "/home/danl/p/l/melang/data/cmn/dict"
    deckNext <-
        take 40 .
        map (\l -> Centuline (dlWord l) ""
            (dlPartOfSpeech l `DT.append` ":")) .
        filter (not . (`Set.member` deckWordSet) . dlWord) .
        zipWith readDictline [1..] . DT.lines <$> DTI.readFile dictF
    -- DTI.appendFile deckF . DT.unlines $
    DTI.writeFile deckF . DT.unlines . map showCentuline $ deck ++ deckNext
