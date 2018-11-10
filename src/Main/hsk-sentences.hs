#include <h>

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
    hskWds <- map (T.takeWhile (/= '\t')) . T.lines <$>
        T.readFile "/home/danl/all.csv.txt"
    let hskZiSet = HS.fromList $ T.unpack $ T.concat hskWds
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
    lens <- forM hskWds $ \hskWd -> do
        let contSents = filter ((hskWd `T.isInfixOf`) . snd) zhNumSents
        if null contSents then return Nothing else do
            let (n, bestSent) = maximumBy
                    (compare `on` hskSentGoodness hskZiSet zhNumToEnNumsMap)
                    contSents
                enSentNs = fromMaybe [] $ HM.lookup n zhNumToEnNumsMap
                enSent = if null enSentNs then "?" else
                    maximumBy (compare `on` T.length)
                    [ fromJustNote "enSent" (HM.lookup enSentN enMap)
                    | enSentN <- enSentNs]
                html = enSent ++ "<br>" ++ bestSent
            T.putStrLn hskWd
            T.putStrLn bestSent
            T.putStrLn enSent
            T.putStrLn ""
            return $ Just $ T.length bestSent
    print $ maximum $ catMaybes lens
        
    {-
    let n1s = map fst nums
        zhSents = [fromJust (HM.lookup n1 zhMap) | n1 <- n1s]
        enSents =
            [ maximumBy (compare `on` T.length)
              [fromJust (HMS.lookup n2 enMap) | n2 <- n2s]
            | (_, n2s) <- nums
            ]
    wdPinyinSents <- getPinyins mandarinSentences
    BSLC.writeFile "/home/danl/p/one-off/www/hanyu/tatoeba/sentence.info" $
        serialise $ (n1s, wdPinyinSents, englishSentences)

    -- ?
    let wdPinyinGlossSents =
            map (map (wdPinyinAddGloss glossMap)) wdPinyinSents
        prepEntry count n1 wdPinyinGlosses eSent =
            [ Ae.String n1
            , wdPinyinGlossesToAeson wdPinyinGlosses
            , Ae.String eSent
            ]
        entries = zipWith4 prepEntry [1..] n1s wdPinyinGlossSents eSents
    -}
        
