#include <h>

import Lang.Zh.Gloss
import Lang.Zh.WdPinyin

procLine :: (Text, Text)
procLine [num, _, sent] = (num, sent)
procLine x = error $ "procLine: " ++ show x

splitCols :: [Text]
splitCols = T.splitOn "\t"

readNumToSents :: FilePath -> (Text, Text)
readNumToSents filename =
    map (procLine . splitCols) . T.lines . T.decodeUtf8 <$>
    run ("xzcat" :: String , [filename :: String])

main :: IO ()
main = do
    hskWds <- map (T.takeWhile (/= '\t')) . T.lines <$>
        T.readFile "/home/danl/all.csv.txt"
    glossMap <- loadGlossMap
    tatDir <- (</> ("data" </> "tatoeba")) <$> getHomeDirectory
    l1 <- readNumToSents $ tatDir </> "simp-cmn.csv.xz"
    let hskWd = hskWds `at` 4000
    map T.putStrLn $ filter (hskWd `T.isInfixOf`) $ map snd l1
    {-
    l2 <- readNumToSents $ tatDir </> "eng.csv.xz"
    nums <- HM.toList . HM.fromListWith (++) . map (\[a, b] -> (a, [b])) .
        filter (\[a, b] -> a `HMS.member` l1 && b `HMS.member` l2) .
        map splitCols . T.lines . T.decodeUtf8 <$>
        run ("xzcat" :: String, [tatDir </> "links.csv.xz" :: String])
    let n1s = map fst nums
        mandarinSentences = [fromJust (HM.lookup n1 l1) | n1 <- n1s]
        englishSentences =
            [ maximumBy (compare `on` T.length)
              [fromJust (HMS.lookup n2 l2) | n2 <- n2s]
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
