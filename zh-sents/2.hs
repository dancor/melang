#include <h>

-- Each entry has the English sentence and the Mandarin sentence
-- segmented into words. We need to look up pinyin and gloss info
-- for the Mandarin words from CEDICT.

import Codec.Serialise
import Lang.Zh.Cedict

import WdPinyinGloss

wdPinyinGlossHtml :: WdPinyinGloss -> Text
wdPinyinGlossHtml (WdPinyinGloss w p g) = "<span class=w>" <> w <>
    "<div class=p>" <> p <> "</div>" <>
    "<div class=g>" <> g <> "</div>" <> "</span>"

showEntry :: Text -> [WdPinyinGloss] -> IO ()
showEntry en wdPinyinGlosses = do
    --when (any (\(WdPinyinGloss w _ _) -> w == "哪怕") wdPinyinGlosses) $ do
        T.putStrLn $ "<div class=e>" <> en <> "<br>" <>
            T.concat (map wdPinyinGlossHtml wdPinyinGlosses) <>
            "</div><br><br>"
        --hFlush stdout

type SentenceInfo = (Char, Int, [Text], Text)

loadSentenceInfos :: FilePath -> IO [SentenceInfo]
loadSentenceInfos = fmap deserialise . BL.readFile

{-
dictLookup :: Text -> Cedict -> Maybe WdPinyinGloss
dictLookup wd dict = loop 0 where
    loop depth = case HM.lookup wd dict of
      Just (CedictEntry p res) -> case res of 
        CedictGloss g -> Just $ WdPinyinGloss wd p g
        CedictRef ref -> if depth > 10
          then error $ "Depth greater than 10 on:" ++ T.unpack wd
          else loop (depth + 1)
      Nothing -> Nothing
-}
dictLookup :: Text -> Cedict -> Maybe WdPinyinGloss
dictLookup wd@"个" _ = Just $ WdPinyinGloss wd "ge4" "ge"
dictLookup wd@"次" _ = Just $ WdPinyinGloss wd "ci4" "time"
dictLookup wd@"第" _ = Just $ WdPinyinGloss wd "di4" "#"
dictLookup wd dict = loop 0 wd where
    loop depth curWd = case HM.lookup curWd dict of
      Just (CedictEntry p res) -> case res of 
        CedictGloss g -> Just $ WdPinyinGloss wd p g
        CedictRef ref -> if depth > 10
          then error $ "Depth greater than 10 started:" ++ T.unpack wd
          else loop (depth + 1) ref
      Nothing -> Nothing

textSpans t = zip (T.inits t) (T.tails t)

properTextSpans t
 | T.length t < 2 = error $ "properTextSpans: " ++ show t
 | otherwise = init . tail $ zip (T.inits t) (T.tails t)

pullFstMb :: (Maybe a, b) -> Maybe (a, b)
pullFstMb (Just a, b) = Just (a, b)
pullFstMb _ = Nothing

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

joinExtras [] = []
joinExtras (WdPinyinGloss wd1 "" "" : WdPinyinGloss wd2 "" "" : xs) =
    joinExtras (WdPinyinGloss (wd1 <> inter <> wd2) "" "" : xs)
  where
    inter = if T.all isDigit (T.takeEnd 1 wd1 <> T.take 1 wd2) then " " else ""
joinExtras (x:xs) = x : joinExtras xs

procEntry dict (_setLtr, _numInSet, zhWds, enSent) =
    showEntry enSent $ joinExtras $ map (wdGloss dict) zhWds

main :: IO ()
main = do
    dict <- loadCedictGlossMap
    infos <- loadSentenceInfos "MultiUN/1"
    putStrLn "<html><head><link rel=\"stylesheet\" href=\"a.css\"></head>"
    putStrLn "<body>"
    mapM_ (procEntry dict) $ take 1000 infos
    putStrLn "</body></html>"
