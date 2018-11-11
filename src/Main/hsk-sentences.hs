#include <h>

import Lang.Zh.Anki
import Lang.Zh.Gloss
import Lang.Zh.WdPinyin

procLine :: [Text] -> (Int, Text)
procLine [numText, _, sent] = (num, sent)
  where
    Just num = readMaybe $ T.unpack numText
procLine x = error $ "procLine: " ++ show x

splitCols :: Text -> [Text]
splitCols = T.splitOn "\t"

readNumToSents :: FilePath -> IO [(Int, Text)]
readNumToSents filename =
    map (procLine . splitCols) . T.lines . T.decodeUtf8 <$>
    run ("xzcat" :: String , [filename :: String])

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

wdAddBestSentMb hskZiSet zhNumToEnNumsMap zhNumSents enMap wd =
    if null contSents then Nothing else Just (wd, bestSent, enSent)
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

{-
colorMap '1' = "#055"
colorMap '2' = "#505"
colorMap '3' = "#530"
colorMap '4' = "#050"
colorMap _   = "#007"
-}

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
    {-
    "<span style=\"float:left;margin-left:10px;font-size:200%\">" <>
        zi <>
        "<div style=\"font-size:50%\">" <> colorPy "" (T.unpack py) <>
            "</div>" <>
        "<div style=\"font-size:40%;color:#aaa\">" <> gloss <> "</div>" <>
    "</span>"
    -}
    "<span style=\"float:left;margin-left:10px\">" <>
        "<div style=\"font-size:200%\">" <> zi <> "</div>" <>
        colorPy "" (T.unpack py) <>
        "<div style=\"font-size:80%;color:#007\">" <> gloss <> "</div>" <>
    "</span>"

sentHtml enSent wdPinyinGlosses =
    "<div style=\"color:#007\">" <> enSent <> "</div>" <>
    T.concat (map ziHtml wdPinyinGlosses)

main :: IO ()
main = do
    args <- getArgs
    saveChanges <- case args of
        [] -> return False
        ["--dry-run"] -> return False
        ["--save-changes"] -> return True
        _ -> fail "Usage"
    notes <- loadZhAnkiNotes
    let hskWds = map zWord notes
        hskZiSet = HS.fromList $ T.unpack $ T.concat hskWds
    glossMap <- loadGlossMap
    tatDir <- (</> ("data" </> "tatoeba")) <$> getHomeDirectory
    zhNumSents <- readNumToSents $ tatDir </> "simp-cmn.csv.xz"
    enNumSents <- readNumToSents $ tatDir </> "eng.csv.xz"
    let zhMap = HM.fromList zhNumSents
    let enMap = HM.fromList enNumSents
    zhNumToEnNumsMap <- HM.fromListWith (++) . map (\[a, b] -> (a, [b])) .
        filter (\[a, b] -> a `HM.member` zhMap && b `HM.member` enMap) .
        map (map (readNote "Expecting Int in links.csv.xz" . T.unpack) .
            splitCols) .
        T.lines . T.decodeUtf8 <$>
        run ("xzcat" :: String, [tatDir </> "links.csv.xz" :: String])
    let (wdsToChange, bestSents, enSents) = unzip3 $ catMaybes $ map
            (wdAddBestSentMb hskZiSet zhNumToEnNumsMap zhNumSents enMap) hskWds
    wdPinyins <- getWdPinyins bestSents
    let wdPinyinEnMap = HM.fromList $ zip wdsToChange $ zip wdPinyins enSents
    forM_ notes $ \note -> case HM.lookup (zWord note) wdPinyinEnMap of
        Nothing -> return ()
        Just (wdPinyins, enSent) -> do
            let html = sentHtml enSent $
                    map (wdPinyinAddGloss glossMap) wdPinyins
            updateNote $ note {zHtml = html}
            T.putStrLn $ zWord note
