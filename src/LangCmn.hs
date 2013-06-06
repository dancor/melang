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
import Data.Ord
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
    { fRank          :: !Int
    , fNumPerMillion :: !Float
    , fWd            :: !DT.Text
    , fPartOfSpeech  :: !DT.Text
    } deriving Show

data CedictLine = CedictLine
    { cTrad :: !DT.Text
    , cSimp :: !DT.Text
    , cDef  :: !DT.Text
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

data PyDef = PyDef
    { pdPy   :: ![DT.Text]
    , pdDef  :: !DT.Text
    , pdFreq :: !Float
    } deriving (Eq, Ord, Show)

data WdInfo = WdInfo
    { wiNumPerMillion :: !Float
    , wiWd            :: !Wd
    , wiPartOfSpeech  :: !DT.Text
    , wiDef           :: ![PyDef]
    } deriving (Eq, Ord, Show)

instance NFData PyDef where
    rnf (PyDef a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData WdInfo where
    rnf (WdInfo a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

parseWdInfoLine :: Int -> BS.ByteString -> WdInfo
parseWdInfoLine _n str =
    WdInfo (read $ BSC.unpack a) wd (DTE.decodeUtf8 c) .
    map (
        (\ (py, def) -> PyDef
            (fixPinyins . DT.words . DT.replace "u:" "v" $ DT.drop 1 py)
            (DT.drop 2 def)
            0
        ) .
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

doGloss :: WdInfo -> DT.Text
doGloss =
    DT.intercalate "/" . map (DT.concat . pdPy) .
    sortBy (flip $ comparing pdFreq) . wiDef

readWdInfoFile :: IO [WdInfo]
readWdInfoFile =
    zipWith parseWdInfoLine [1..] . BSC.lines <$> BS.readFile wdInfoFile

loadDictAsIs :: IO (Map.Map Wd [WdInfo])
loadDictAsIs =
    Map.fromListWith (++) . map (\wi -> (wiWd wi, [wi])) <$> readWdInfoFile

dictKillPos :: Map.Map Wd [WdInfo] -> Map.Map Wd WdInfo
dictKillPos = Map.map wisKillPos
  where
    wisKillPos wis =
        WdInfo (sum $ map wiNumPerMillion wis) (wiWd $ head wis) ("???")
        (nub $ concatMap wiDef wis)

onDef :: ([PyDef] -> [PyDef]) -> WdInfo -> WdInfo
onDef f wi = wi {wiDef = f $ wiDef wi}

dictCollateSamePy :: Map.Map Wd WdInfo -> Map.Map Wd WdInfo
dictCollateSamePy = Map.map (onDef collateSamePy)
  where
    collateSamePy =
        map (\(py, d) -> PyDef py d 0) .
        Map.toList . Map.map DT.concat .
        -- Later, we could switch this to preserve some capitalization
        -- info.
        Map.fromListWith (++) .
        map pdTuple
    pdTuple (PyDef py d 0) = (map DT.toLower py, [d])
    pdTuple p =
        error $ "dictCollateSamePy: PyDef pdFreq not 0: " ++ show p

dictKillDumbDefs :: Map.Map Wd WdInfo -> Map.Map Wd WdInfo
dictKillDumbDefs = Map.map (onDef $ filter (not . isDumb . pdDef))
  where
    isDumb x =
        "/see " `DT.isPrefixOf` x ||
        "/variant " `DT.isPrefixOf` x

pyByChar :: WdInfo -> [[(Char, DT.Text)]]
pyByChar wi = map (pyByChar1 . pdPy) $ wiDef wi
  where
    pyByChar1 = zip (DT.unpack $ wiWd wi)

textFind :: DT.Text -> DT.Text -> Int
textFind needle haystack =
    if DT.null rest then -1 else DT.length pre
  where
    (pre, rest) = DT.breakOn needle haystack

loadDict :: IO (Map.Map Wd WdInfo)
loadDict = do
    dict <-
        dictCollateSamePy . dictKillDumbDefs . dictKillPos <$> loadDictAsIs
    let dupes =
            -- sortBy (flip $ comparing wiNumPerMillion) .
            map snd . Map.toList $ Map.filter ((> 1) . length . wiDef) dict
        defPretty (PyDef pys d _) = DT.unwords [DT.concat pys, d]
        allWds = Map.keys dict

        wdsContaining :: Wd -> [(Wd, Int)]
        wdsContaining wd =
            map (\ w -> (w, textFind wd w)) $
            filter (\ x -> wd `DT.isInfixOf` x && wd /= x) allWds

        wiAddDefFreq :: WdInfo -> WdInfo
        wiAddDefFreq wi =
            if length (wiDef wi) > 1
              then onDef (map pyDefMod) wi
              else wi
          where
            pyDefMod pd = pd {pdFreq = getFreq $ pdPy pd}
            wd = wiWd wi
            getFreq py
              -- overrides
              | wd == "的" && py == ["de5"]    ||
                wd == "与" && py == ["yu3"]    ||
                wd == "都" && py == ["dou1"]   ||
                wd == "应" && py == ["gai1"]   ||
                wd == "长" && py == ["chang2"] ||
                wd == "几" && py == ["ji3"]    ||
                wd == "分" && py == ["fen4"]   ||
                wd == "给" && py == ["gei3"]   ||
                wd == "地方" && py == ["di4", "fang5"] ||
                wd == "带子" && py == ["dai4", "zi5"]
                = 100000
              | otherwise = fromMaybe 0 $ Map.lookup py freqMap
            freqMap =
                Map.fromListWith (+) .
                map (\(offset, freq, pys) ->
                    (take (DT.length $ wiWd wi) $ drop offset pys, freq)) .
                concatMap (\(w, offset) ->
                    map ((,,) offset (wiNumPerMillion w) . pdPy) $ wiDef w) .
                map (first $ fromJust . flip Map.lookup dict) .
                wdsContaining $ wiWd wi
        {-
        linesForWi :: WdInfo -> DT.Text
        linesForWi wi = DT.unlines $
            [DT.unwords [wiWd wi, DT.pack . show $ wiNumPerMillion wi]] ++
            map defPretty (wiDef wi) ++
            (
                map (DT.pack . show) .
                sortBy (flip $ comparing snd) .
                Map.toList . Map.fromListWith (+) .
                map (\(offset, freq, pys) ->
                    (take (DT.length $ wiWd wi) $ drop offset pys, freq)) .
                concatMap (\(w, offset) ->
                    map ((,,) offset (wiNumPerMillion w) . pdPy) $ wiDef w) .
                map (first $ fromJust . flip Map.lookup dict) .
                wdsContaining $ wiWd wi
            )
        -}
    --return . DT.unlines $ map linesForWi dupes
    return $ Map.map wiAddDefFreq dict

textWds :: Map.Map Wd WdInfo -> DT.Text -> [Either DT.Text WdInfo]
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
        (\wdInfo -> (wdInfo, rest)) <$> Map.lookup wd dict
    wdAndRests =
        map (\n -> (DT.take n text, DT.drop n text)) $ reverse [1 .. maxWdLen]
    maxWdLen = 7

processLine :: Map.Map Wd WdInfo -> (DT.Text, DT.Text) -> DT.Text
processLine dict (en, zh) =
    DT.unlines [en, sentPy dict $ DT.init zh, zh]

cleanSent :: DT.Text -> DT.Text
cleanSent = DT.replace "。" "." . DT.replace ", " "," . DT.replace "，" ","

sentPy :: Map.Map Wd WdInfo -> DT.Text -> DT.Text
sentPy dict = DT.unwords . map (either id doGloss) . textWds dict . cleanSent

doWd :: WdInfo -> DT.Text
doWd w = DT.unwords [wiWd w, doGloss w, DT.concat . map pdDef $ wiDef w]

doSent :: Map.Map Wd WdInfo -> DT.Text -> IO ()
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
