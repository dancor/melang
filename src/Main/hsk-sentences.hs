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

main :: IO ()
main = do
    args <- getArgs
    saveChanges <- case args of
        [] -> return False
        ["--dry-run"] -> return False
        ["--save-changes"] -> return True
        _ -> fail "Usage"
    notes <- loadZhAnkiNotes
    let hskZiSet = HS.fromList $ T.unpack $ T.concat $ map zWord notes
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
    forM_ notes $ \note -> do
        let wd = zWord note
            contSents = filter ((wd `T.isInfixOf`) . snd) zhNumSents
        unless (null contSents) $ do
            let (n, bestSent) = maximumBy
                    (compare `on` hskSentGoodness hskZiSet zhNumToEnNumsMap)
                    contSents
                enSentNs = fromMaybe [] $ HM.lookup n zhNumToEnNumsMap
                enSent = if null enSentNs then "?" else
                    maximumBy (compare `on` T.length)
                    [ fromJustNote "enSent" (HM.lookup enSentN enMap)
                    | enSentN <- enSentNs]
                html = enSent <> "<br>" <> bestSent
            updateNote $ note {zHtml = html}
            T.putStrLn wd
