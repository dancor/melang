{-# LANGUAGE OverloadedStrings #-}

-- | Parsing Mandarin sentences and fragments into individual words.

module Cmn.SentenceWords2 where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.WdInfo2

readWdInfoFile :: IO [WdInfo]
readWdInfoFile =
    zipWith parseWdInfoLine [1..] . BSC.lines <$> BS.readFile wdInfoFile

textFind :: DT.Text -> DT.Text -> Int
textFind needle haystack =
    if DT.null rest then -1 else DT.length pre
  where
    (pre, rest) = DT.breakOn needle haystack

loadDict :: IO (HMS.HashMap Wd WdInfo)
-- loadDict = HMS.fromList . map (\wi -> (wiWd wi, wi)) <$> readWdInfoFile
loadDict = HMS.fromList . map (\wi -> (wiWd wi, wi)) . take 100000 <$>
    readWdInfoFile

textWds :: HMS.HashMap Wd WdInfo -> DT.Text -> [Either DT.Text WdInfo]
textWds dict text =
    if DT.null text
      then []
      else res : textWds dict resRest
  where
    (res, resRest) = maybe ifNotFound (first Right) . listToMaybe .
        catMaybes $ map tryWdAndRest wdAndRests
    ifNotFound = (Left $ DT.take 1 text, DT.drop 1 text)
    tryWdAndRest :: (DT.Text, DT.Text) -> Maybe (WdInfo, DT.Text)
    tryWdAndRest (wd, rest) =
        (\wdInfo -> (wdInfo, rest)) <$> HMS.lookup wd dict
    wdAndRests =
        map (\n -> (DT.take n text, DT.drop n text)) $ reverse [1 .. maxWdLen]
    maxWdLen = 7

cleanSent :: DT.Text -> DT.Text
cleanSent = DT.replace "。" "." . DT.replace ", " "," . DT.replace "，" ","

-- | Provide a one-sig-dig summary like "top100" for 93 or "top20k" for 17234
sigSummary :: Int -> String
sigSummary n
    | n <= 0 = error "sigSummary: value is not positive"
    | n <= 100 = "top100"
    | otherwise = "top" ++
        (resultDig1 : take extraOrdMagNum (repeat '0')) ++ sciOrdMagStr
      where
        dig1Char:digRestStr = show n
        dig1                = read [dig1Char]
        ceilDig1 = if null digRestStr ||
                      read digRestStr == (0 :: Int) then dig1 else dig1 + 1
        resultDig1:extraDigFromCeil    = show (ceilDig1 :: Int)
        ordMagNum                      = length $ extraDigFromCeil ++ digRestStr
        (sciOrdMagNum, extraOrdMagNum) = divMod ordMagNum 3
        sciOrdMagStr =
            ("" : map (:[]) "kMGTPEZY" ++
            repeat (error "sigSummary: beyond metric prefixes")) !!
            sciOrdMagNum

doWi :: WdInfo -> DT.Text
doWi w = DT.intercalate "\t"
    [ DT.pack . sigSummary $ wiN w
    , wiWd w
    , wiDef w
    ]

doSent :: HMS.HashMap Wd WdInfo -> DT.Text -> IO ()
doSent dict =
    DTI.putStr .
    DT.unlines . map (either id doWi) . textWds dict . cleanSent

{-
processLine :: HMS.HashMap Wd WdInfo -> (DT.Text, DT.Text) -> DT.Text
processLine dict (en, zh) =
    DT.unlines [en, sentPy dict $ DT.init zh, zh]

textWdsFile :: IO ()
textWdsFile = do
    dict <- loadDict
    ls <- map (second DT.tail . DT.break (== '\t')) . DT.lines <$>
        DTI.readFile "/home/danl/l/l/z/mass-sentence-method/cmn/en2zh"
    DTI.writeFile "/home/danl/l/l/z/mass-sentence-method/cmn/list.txt" .
        DT.unlines $ map (processLine dict) ls
-}
