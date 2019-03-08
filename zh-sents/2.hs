#include <h>

-- Each entry has the English sentence and the Mandarin sentence
-- segmented into words. We need to look up pinyin and gloss info
-- for the Mandarin words from CEDICT.

import Codec.Serialise
import Lang.Zh.Cedict

import WdPinyinGloss

spaceCol :: [Text] -> [Text]
spaceCol [] = []
spaceCol rows = zipWith
    (\row width -> row <> T.replicate (maxWidth - width) " ") rows widths
  where
    widths = map (sum . map wcwidth . T.unpack) rows
    maxWidth = maximum widths

lol :: Text -> [WdPinyinGloss] -> IO ()
lol en wdPinyinGlosses = do
    --when (any (\(WdPinyinGloss w _ _) -> w == "哪怕") wdPinyinGlosses) $ do
        T.putStrLn en
        mapM_ (T.putStrLn . T.intercalate " ") $ transpose
            [spaceCol [w, p, g] | WdPinyinGloss w p g <- wdPinyinGlosses]
        T.putStrLn ""
        hFlush stdout

type SentenceInfo = (Char, Int, [Text], Text)

loadSentenceInfos :: FilePath -> IO [SentenceInfo]
loadSentenceInfos = fmap deserialise . BL.readFile

wdGloss :: Int -> Cedict -> Text -> [WdPinyinGloss]
wdGloss !n !dict !wd = case HM.lookup wd dict of
  Just (CedictEntry p (CedictGloss g)) -> [WdPinyinGloss wd p g]
  Just (CedictEntry p (CedictRef ref)) -> if n > 10
    then error "Depth greater than 10."
    else wdGloss (n + 1) dict ref
  Nothing -> if T.length wd == 1
    then [WdPinyinGloss wd "" ""]
    else concatMap (wdGloss 0 dict . T.singleton) $ T.unpack wd

procInfo dict (_setLtr, _numInSet, zhWds, enSent) =
    lol enSent $ concatMap (wdGloss 0 dict) zhWds

main :: IO ()
main = do
    dict <- loadCedictGlossMap
    infos <- loadSentenceInfos "MultiUN/1"
    mapM_ (procInfo dict) $ take 1 infos
