#include <h>

-- Each entry has the English sentence and the Mandarin sentence
-- segmented into words. We need to look up pinyin and gloss info
-- for the Mandarin words from CEDICT.

import Codec.Serialise
import Lang.Zh.Cedict
--import Lang.Zh.Anki

import WdPinyinGloss

colorMap '1' = "#faa"
colorMap '2' = "#afa"
colorMap '3' = "#acf"
colorMap '4' = "#faf"
colorMap _   = "#ff8"

colorPy _ [] = ""
colorPy acc (x:xs) = if isAlpha x then colorPy (acc <> T.singleton x) xs
  else "<span style='color:" <> colorMap x <> "'>" <> acc <> T.singleton x <>
       "</span>" <> colorPy "" xs

ziHtml (WdPinyinGloss zi py gloss) =
    "<span style=\"float:left;margin-left:10px;height:80px\">" <>                          
        "<div style=\"font-size:200%\">" <> zi <> "</div>" <>                  
        colorPy "" (T.unpack py) <>                                            
        "<div style=\"font-size:80%;color:#ff8\">" <> gloss <> "</div>" <>     
    "</span>"                                                                  
                                                                               
sentHtml enSent wdPinyinGlosses = "<div style=\"overflow:hidden\">"
    <> "<div style=\"color:#ff8\">" <> enSent <> "</div>"
    <> T.concat (map ziHtml wdPinyinGlosses)      
    <> "</div>"

wdPinyinGlossHtml :: WdPinyinGloss -> Text
wdPinyinGlossHtml (WdPinyinGloss w p g) = "<span class=w>" <> w <>
    "<div class=p>" <> p <> "</div>" <>
    "<div class=g>" <> g <> "</div>" <> "</span>"

showEntry :: Text -> [WdPinyinGloss] -> IO ()
showEntry en wdPinyinGlosses = do
    T.putStrLn $ sentHtml en wdPinyinGlosses
    {-
	T.putStrLn $ "<div class=e>" <> en <> "<br>" <>
		T.concat (map wdPinyinGlossHtml wdPinyinGlosses) <>
		"</div><br><br>"
    -}

type SentenceInfo = (Char, Int, [Text], Text)

{-
isGoodEntry ('U', 7216814, _, _) = False -- Super long translation "sentence"..
isGoodEntry ('U', 4080416, _, _) = False -- Super long translation "sentence"..
-}
isGoodEntry (_, _, zhWds, _) = null $ drop 20 zhWds

loadSentenceInfos :: FilePath -> IO [SentenceInfo]
loadSentenceInfos = fmap (filter isGoodEntry . deserialise) . BL.readFile

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

procEntry dict (setLtr, numInSet, zhWds, enSent) =
    showEntry (T.pack (setLtr : show numInSet) <> ": " <> enSent) $
    joinExtras $ map (wdGloss dict) zhWds

entryHasWord c (_, _, zhWds, _) = any (c `T.isInfixOf`) zhWds

type Entry = (Char, Int, [Text], Text)

type EntryGatherer = HashMap Text (HashSet Entry)

main :: IO ()
main = do
    home <- getHomeDirectory
    hskWords <- map (T.takeWhile (not . isSpace)) . T.lines <$>
        T.readFile (home </> "hsk.txt")
    hPrint stderr $ length hskWords
    --let gatherer = HM.fromList [(w, HS.empty) | w <- hskWords]
    dict <- loadCedictGlossMap
    putStrLn "<html><head><link rel=\"stylesheet\" href=\"a.css\"></head>"
    putStrLn "<body>"
    files <- take 1 . drop 2 <$> getDirectoryContents "MultiUN"
    res <- HM.fromListWith (flip (++)) . concat <$> forM files (\file -> do
        entries <- loadSentenceInfos $ "MultiUN" </> file
        return [(wd, [entry])
            | entry@(_,_,zhWds,_) <- entries, let s = HS.fromList zhWds
            , wd <- hskWords,  wd `HS.member` s]
        --mapM_ (procEntry dict) $ filter (entryHasWord "活跃") entries
        )
    hPrint stderr $ length $ HM.keys res
    forM_ (HM.toList res) $ \(wd, entries) -> do
        T.putStrLn wd
        mapM_ (procEntry dict) $ take 5 entries
        --mapM_ (\(_,_,_,en) -> T.putStrLn en) $ take 2 entries
    putStrLn "</body></html>"
