{-# LANGUAGE OverloadedStrings #-}

module LangCmn where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Text.Encoding as DTE

import BSUtil

-- Frequencies from books in Google Books published since 1980.
-- Line format:
--   freqPerMillionWords<tab>
--   simplifiedChineseWord_PartOfSpeechAbbr
freqFile :: String
freqFile = "/home/danl/p/l/melang/data/ngrams/20120701/cmn/out/1980"

-- CEDICT.
-- Line format:
--   traditionalChineseWord<space>
--   simplifiedChinsesWord<space>
--   [pinyinWithSpaces]<space>
--   /def1/def2/etc/
cedictFile :: String
cedictFile = "/home/danl/l/l/z/cedict/dict"

-- "Word Info": Google Books words with CEDICT entries.
-- Line format:
--   lineNumber<tab>
--   freqPerMillionWords<tab>
--   simplifiedChineseWord<tab>
--   PartOfSpeechAbbr<tab>
--   [pinyin1WithSpaces] /def1a/def1b/etc/; [pinyin2WithSpaces] /etc/
wdInfoFile :: String
wdInfoFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

data FreqLine = FreqLine
    { fRank          :: Int
    , fNumPerMillion :: Float
    , fWd            :: DT.Text
    , fPartOfSpeech  :: DT.Text
    } deriving Show

data CedictLine = CedictLine
    { cTrad :: DT.Text
    , cSimp :: DT.Text
    , cDef  :: DT.Text
    } deriving Show

parseFreqLine :: Int -> BS.ByteString -> FreqLine
parseFreqLine n str =
    FreqLine n (read $ BSC.unpack a) (DTE.decodeUtf8 b) (DTE.decodeUtf8 c)
  where
    (a, bAndC) = breakTab str
    (b, c) = second BS.tail $ BS.breakByte (fromIntegral $ ord '_') bAndC

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine _n str =
    CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) (DTE.decodeUtf8 def)
  where
    (trad, simpAndDef) = doSplit str
    (simp, def) = doSplit simpAndDef
    doSplit = second BS.tail . BS.breakByte (fromIntegral $ ord ' ')

type Wd = DT.Text

data WdInfoLine
    = WdInfoLine
    { wiN             :: Int
    , wiNumPerMillion :: Float
    , wiWd            :: Wd
    , wiPartOfSpeech  :: DT.Text
    , wiDef           :: [([DT.Text], DT.Text)]
    } deriving (Eq, Ord, Show)

instance NFData WdInfoLine where
    rnf (WdInfoLine a b c d e) =
        rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

parseWdInfoLine :: Int -> BS.ByteString -> WdInfoLine
parseWdInfoLine n str =
    WdInfoLine n (read $ BSC.unpack a) wd (DTE.decodeUtf8 c) .
    map (
        first (fixPinyins . DT.words . DT.replace "u:" "v" . DT.drop 1) .
        second (DT.drop 2) . 
        DT.break (== ']')) .
    DT.splitOn "; " $ DTE.decodeUtf8 d
  where
    fixPinyins = zipWith fixPinyin (DT.unpack wd)
    fixPinyin zh py =
        case zh of
          '一' -> "yi421"
          '不' -> "bu42"
          _ -> py
    wd = DTE.decodeUtf8 b
    (_weirdN, afterN) = breakTab str
    (a, afterA) = breakTab afterN
    (b, afterB) = breakTab afterA
    (c, d) = breakTab afterB

chooseADef :: [WdInfoLine] -> WdInfoLine -> WdInfoLine
chooseADef wdInfoLines wdInfoLine =
    fixup $ if length defs == 1
      then wdInfoLine
      else
        if length defsLower == 1
          then wdInfoLine {wiDef = take 1 defsLower}
          else
            -- TODO
            wdInfoLine {
                wiDef = [last $ defs ++ defsLower ++ defsLowerNoSee]}
  where
    fixup wi =
        case wiWd wi of
          "干" -> wi {wiDef = [(["gan41"], snd . head $ wiDef wi)]}
          "正" -> wi {wiDef = [(["zheng4"], snd . head $ wiDef wi)]}
          "看" -> wi {wiDef = [(["kan4"], snd . head $ wiDef wi)]}
          "得" -> wi {wiDef = [(["de{i3}52"], snd . head $ wiDef wi)]}
          "说" -> wi {wiDef = [(["shuo1"], snd . head $ wiDef wi)]}
          "个" -> wi {wiDef = [(["ge5"], snd . head $ wiDef wi)]}
          "那" -> wi {wiDef = [(["na4"], snd . head $ wiDef wi)]}
          "要" -> wi {wiDef = [(["yao4"], snd . head $ wiDef wi)]}
          "打" -> wi {wiDef = [(["da3"], snd . head $ wiDef wi)]}
          "累" -> wi {wiDef = [(["lei4"], snd . head $ wiDef wi)]}
          "带子" -> wi {wiDef = [(["dai4zi5"], snd . head $ wiDef wi)]}
          _ -> wi
    defs = wiDef wdInfoLine
    defsLower = filter (not . any (DT.any isUpper) . fst) defs
    defsLowerNoSee = filter (not . ("/see " `DT.isPrefixOf`) . snd) defsLower

readWdInfoFile :: IO [WdInfoLine]
readWdInfoFile =
    zipWith parseWdInfoLine [1..] . BSC.lines <$> BS.readFile wdInfoFile

loadDict :: IO (Map.Map Wd WdInfoLine)
loadDict = do
    wdInfoLines <- readWdInfoFile
    let wdInfoLinesOneDef =
            map (chooseADef wdInfoLines) wdInfoLines
        wdToInfo =
            Map.fromList $ map (\wi -> (wiWd wi, wi)) wdInfoLinesOneDef
    return wdToInfo

wdToPy :: WdInfoLine -> DT.Text
wdToPy = DT.concat . fst . head . wiDef

wdToPropDef :: WdInfoLine -> DT.Text
wdToPropDef = snd . head . wiDef

textPy :: Map.Map Wd WdInfoLine -> DT.Text -> [Either DT.Text DT.Text]
textPy dict = map (wdToPy <$>) . textWds dict 

textWds :: Map.Map Wd WdInfoLine -> DT.Text -> [Either DT.Text WdInfoLine]
textWds dict text =
    if DT.null text
      then []
      else res : textWds dict resRest
  where
    (res, resRest) = maybe ifNotFound (first Right) . listToMaybe .
        catMaybes $ map tryWdAndRest wdAndRests
    ifNotFound = (Left $ DT.take 1 text, DT.drop 1 text)
    tryWdAndRest :: (DT.Text, DT.Text) -> Maybe (WdInfoLine, DT.Text)
    tryWdAndRest (wd, rest) =
        (\wdInfo -> (wdInfo, rest)) <$> Map.lookup wd dict
        {-
        (\wdInfo ->
        (DT.concat . fst . head $ wiDef wdInfo, rest)) <$>
        Map.lookup wd dict
        -}
    wdAndRests =
        map (\n -> (DT.take n text, DT.drop n text)) $ reverse [1 .. maxWdLen]
    maxWdLen = 7

processLine :: Map.Map Wd WdInfoLine -> (DT.Text, DT.Text) -> DT.Text
processLine dict (en, zh) =
    DT.unlines [en, sentPy dict $ DT.init zh, zh]

cleanSent :: DT.Text -> DT.Text
cleanSent = DT.replace "。" "." . DT.replace ", " "," . DT.replace "，" ","

sentPy :: Map.Map Wd WdInfoLine -> DT.Text -> DT.Text
sentPy dict = DT.unwords . map (either id id) . textPy dict . cleanSent

doWd :: WdInfoLine -> DT.Text
doWd w = DT.unwords [wiWd w, wdToPy w, wdToPropDef w]

doSent :: Map.Map Wd WdInfoLine -> DT.Text -> IO ()
doSent dict =
    DTI.putStr .
    DT.unlines . map (either id doWd) . textWds dict . cleanSent

textWdsFile :: IO ()
textWdsFile = do
    dict <- loadDict
    ls <- map (second DT.tail . DT.break (== '\t')) . DT.lines <$>
        DTI.readFile "/home/danl/mass-sentence-method/cmn/en2zh"
    DTI.writeFile "/home/danl/mass-sentence-method/cmn/list.txt" . DT.unlines $
        map (processLine dict) ls
