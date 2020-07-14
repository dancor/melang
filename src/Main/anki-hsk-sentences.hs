{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Each entry has the English sentence and the Mandarin sentence
-- segmented into words. We need to look up pinyin and gloss info
-- for the Mandarin words from CEDICT.

import Control.Arrow
import qualified Control.Concurrent as Conc
import Control.Monad
import Control.Monad.Extra
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Function
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.List.Split as Spl
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import HSH
import Safe
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.Read

import Codec.Serialise
import Lang.Zh.Anki
import Lang.Zh.Cedict
import Lang.Zh.WdPinyinGloss

data Entry = Entry
  { eOriginTag :: Char
  , eNum       :: Int
  , eZhWords   :: [Text]
  , eEn        :: Text
  }

colorMap :: Char -> Text
colorMap '1' = "#f00"
colorMap '2' = "#080"
colorMap '3' = "#00f"
colorMap '4' = "#808"
colorMap _   = "#888"

colorPy :: Text -> [Char] -> Text
colorPy _ [] = ""
colorPy acc (x:xs) = if isAlpha x then colorPy (acc <> T.singleton x) xs
  else "<span style='color:" <> colorMap x <> "'>" <> acc <> T.singleton x <>
       "</span>" <> colorPy "" xs

ziHtml :: WdPinyinGloss -> Text
ziHtml (WdPinyinGloss zi py gloss) =
    "<span style=\"float:left;margin-left:10px;height:80px\">" <>
        "<div style=\"font-size:200%\">" <> zi <> "</div>" <>                  
        colorPy "" (T.unpack py) <>                                            
        "<div style=\"font-size:80%\">" <> gloss <> "</div>" <>     
    "</span>"                                                                  

sentHtml :: Text -> [WdPinyinGloss] -> Text
sentHtml enSent wdPinyinGlosses = "<div style=\"overflow:hidden\">"
    <> "<div style=\"color:#aa0\">" <> enSent <> "</div>"
    <> T.concat (map ziHtml wdPinyinGlosses)      
    <> "</div>"

showEntry :: Text -> [WdPinyinGloss] -> Text
showEntry en wdPinyinGlosses = sentHtml en wdPinyinGlosses

isGoodEntry :: Entry -> Bool
isGoodEntry = null . drop 20 . eZhWords

loadSentenceInfos :: FilePath -> IO [Entry]
loadSentenceInfos = fmap (filter isGoodEntry .
    map (\(a,b,c,d) -> Entry a b c d) . deserialise) . BL.readFile

dictLookup :: Text -> Cedict -> Maybe WdPinyinGloss
dictLookup wd@"个" _ = Just $ WdPinyinGloss wd "ge4" "ge"
dictLookup wd@"次" _ = Just $ WdPinyinGloss wd "ci4" "time"
dictLookup wd@"第" _ = Just $ WdPinyinGloss wd "di4" "#"
dictLookup wd dict = go 0 wd where
    go :: Int -> Text -> Maybe WdPinyinGloss
    go depth curWd = case HM.lookup curWd dict of
      Just (CedictEntry p res) -> case res of 
        CedictGloss g -> Just $ WdPinyinGloss wd p g
        CedictRef ref -> if depth > 10
          then error $ "Depth greater than 10 started:" ++ T.unpack wd
          else go (depth + 1) ref
      Nothing -> Nothing

properTextSpans :: Text -> [(Text, Text)]
properTextSpans t
 | T.length t < 2 = error $ "properTextSpans: " ++ show t
 | otherwise = init . tail $ zip (T.inits t) (T.tails t)

pullFstMb :: (Maybe a, b) -> Maybe (a, b)
pullFstMb (Just a, b) = Just (a, b)
pullFstMb _ = Nothing

combineWdPinyinGloss :: WdPinyinGloss -> WdPinyinGloss -> WdPinyinGloss
combineWdPinyinGloss (WdPinyinGloss w1 p1 g1) (WdPinyinGloss w2 p2 g2) =
    WdPinyinGloss (w1 <> w2) (c p1 p2) (c g1 g2)
  where
    c x y = x <> "+" <> y

wdGloss :: Cedict -> Text -> WdPinyinGloss
wdGloss !dict !wd
 | T.all (\c -> isAscii c || isPunctuation c) wd = WdPinyinGloss wd "" ""
 | otherwise = case dictLookup wd dict of
  Just res -> res
  Nothing -> if T.length wd == 1
    then WdPinyinGloss wd "?" "?"
    else case catMaybes $ map (pullFstMb . first (flip dictLookup dict)) $
        reverse $ properTextSpans wd of
      (res, rest):_ -> combineWdPinyinGloss res $ wdGloss dict rest
      [] -> combineWdPinyinGloss (WdPinyinGloss (T.take 1 wd) "?" "?") $
          wdGloss dict $ T.drop 1 wd

joinExtras :: [WdPinyinGloss] -> [WdPinyinGloss]
joinExtras [] = []
joinExtras (WdPinyinGloss wd1 "" "" : WdPinyinGloss wd2 "" "" : xs) =
    joinExtras (WdPinyinGloss (wd1 <> inter <> wd2) "" "" : xs)
  where
    inter = if T.all isDigit (T.takeEnd 1 wd1 <> T.take 1 wd2) then " " else ""
joinExtras (x:xs) = x : joinExtras xs

procEntry :: Cedict -> Entry -> Text
procEntry dict (Entry _ _ zhWords en) =
    showEntry en $ joinExtras $ map (wdGloss dict) zhWords

doMultiFile :: FilePath -> [Text] -> FilePath -> IO [(Text, [Entry])]
doMultiFile multiDir hskWords file = do
    entries <- loadSentenceInfos $ multiDir </> file
    return [(wd, [entry])
        | entry <- entries, let s = HS.fromList (eZhWords entry)
        , wd <- hskWords,  wd `HS.member` s]

procLine :: [Text] -> (Int, Text)
procLine [numText, _, sent] = (num, sent)
  where
    Just num = readMaybe $ T.unpack numText
procLine x = error $ "procLine: " ++ show x

splitCols :: Text -> [Text]
splitCols = T.splitOn "\t"

readNumToSents :: FilePath -> IO [(Int, Text)]
readNumToSents = fmap (map (procLine . splitCols) . T.lines) . readResource

hskSentGoodness :: HashSet Char -> HashMap Int [Int] -> (Int, Text) -> Float
hskSentGoodness hskZiSet zhNumToEnNumsMap (n, sent) =
    enGoodness - fromIntegral (len `div` 30) + 
    fromIntegral (T.length (T.filter (`HS.member` hskZiSet) allZis)) /
    fromIntegral len
  where
    allZis = T.filter (\c -> not (isPunctuation c) && isAlpha c) sent
    len = T.length allZis
    hasEn = isJust $ HM.lookup n zhNumToEnNumsMap
    enGoodness = if hasEn then 100 else 0

wdTatEntryMb
    :: HashSet Char
    -> HashMap Int [Int]
    -> [(Int, Text)]
    -> HashMap Int Text
    -> Text
    -> Maybe (Text, (Text, Text))
wdTatEntryMb hskZiSet zhNumToEnNumsMap zhNumSents enMap wd =
    if null contSents then Nothing else Just (bestSent, (wd, enSent))
  where
    contSents = filter ((wd `T.isInfixOf`) . snd) zhNumSents
    (n, bestSent) = maximumBy
        (compare `on` hskSentGoodness hskZiSet zhNumToEnNumsMap)
        contSents
    enSentNs = fromMaybe [] $ HM.lookup n zhNumToEnNumsMap
    enSent = if null enSentNs then "?" else
        maximumBy (compare `on` T.length)
        [ fromJustNote "enSent" (HM.lookup enSentN enMap)
        | enSentN <- enSentNs]

readResource :: FilePath -> IO Text
readResource f = do
    exists <- doesFileExist f
    if exists
      then T.readFile f
      else do
        let fXz = f <> ".xz"
        existsXz <- doesFileExist fXz
        unless existsXz $ error $ "readResource: " ++ f
        T.decodeUtf8 <$> run ("xzcat" :: String , [fXz])


doTat :: FilePath -> [Text] -> IO [(Text, [Entry])]
doTat home hskWords = do
    let tatDir = home </> "data" </> "tatoeba"
    enMap <- HM.fromList <$> readNumToSents (tatDir </> "eng.csv")
    zhNumSents <- readNumToSents $ tatDir </> "simp-cmn.csv"
    let zhMap = HM.fromList zhNumSents
        hskZiSet = HS.fromList $ T.unpack $ T.concat hskWords
    zhNumToEnNumsMap <- HM.fromListWith (++) . map (\[a, b] -> (a, [b])) .
        filter (\[a, b] -> a `HM.member` zhMap && b `HM.member` enMap) .
        map (map (readNote "Expecting Int in links.csv.xz" . T.unpack) .
            splitCols) .
        T.lines . T.decodeUtf8 <$>
        run ("xzcat" :: String, [tatDir </> "links.csv.xz" :: String])
    let (zhSents, wdEns) = unzip $ catMaybes $ map
            (wdTatEntryMb hskZiSet zhNumToEnNumsMap zhNumSents enMap)
            hskWords
    zhWordss <- getPinyins zhSents
    return $ zipWith
        (\zhWords (wd, enSent) -> (wd, [Entry 'T' 0 zhWords enSent]))
        zhWordss wdEns

doMulti :: FilePath -> [Text] -> IO [(Text, [Entry])]   
doMulti home hskWords = do
    let multiDir = home </> "p/l/melang/zh-sents/MultiUN"
    files <- take 100 . drop 2 <$> getDirectoryContents multiDir
    concatMapM (doMultiFile multiDir hskWords) files

procTreeLine :: Text -> Text
procTreeLine l = wd where [_, _, _, wd] = T.splitOn "\t" l

getPinyins :: [Text] -> IO [[Text]]
getPinyins sentences = do
    home <- getHomeDirectory
    (Just hIn, Just hOut, _, _) <- createProcess
        (proc (home </> "p/l/melang/zh-sentence-trees.py") [])
        {std_in = CreatePipe, std_out = CreatePipe}
    _ <- Conc.forkIO $ mapM_ (T.hPutStrLn hIn) sentences >> hClose hIn
    ls <- TL.lines <$> TL.hGetContents hOut
    return . map (map (procTreeLine . TL.toStrict)) $ Spl.splitWhen TL.null ls

main :: IO ()
main = do
    home <- getHomeDirectory
    notes <- loadZhAnkiNotes
    dict <- loadCedictGlossMap
    let hskWords = map zWord notes
    tatRes <- doTat home hskWords
    multiRes <- doMulti home hskWords
    let wdEntriesMap = HM.fromListWith (flip (++)) $ tatRes ++ multiRes
    forM_ (HM.toList wdEntriesMap) $ \(wd, entries) -> do
        let note:_ = filter ((== wd) . zWord) notes
        updateNote $ note {zHtml = T.concat . map (procEntry dict) $
            take 2 entries}
        T.putStrLn wd
