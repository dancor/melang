#include <h>

-- Each entry has the English sentence and the Mandarin sentence
-- segmented into words. We need to look up pinyin and gloss info
-- for the Mandarin words from CEDICT.

import Codec.Serialise
import Lang.Zh.Cedict

import WdPinyinGloss

{-
procLines :: FilePath -> [String] -> IO [Text]
procLines cmd args = do
    (_, Just hOut, _, _) <-
        createProcess (proc cmd args) {std_out = CreatePipe}
    map TL.toStrict . TL.lines <$> TL.hGetContents hOut

readDataResourceLines :: String -> IO [Text]
readDataResourceLines r = do
    home <- getHomeDirectory
    readResourceLines $ home </> "data" </> r

readResourceLines :: FilePath -> IO [Text]
readResourceLines r = do
    e <- doesFileExist r
    let tryReaders [] = error $ "Could not read resource: " ++ r
        tryReaders ((ext,cmd):readers) = do
            let rExt = r ++ ext
            e <- doesFileExist $ rExt
            if e then procLines cmd [rExt] else tryReaders readers
    if e then map TL.toStrict . TL.lines <$> TL.readFile r
      else tryReaders [(".xz", "xzcat"), (".gz", "zcat")]

procLine :: [Text] -> (Int, Text)
procLine [num, _, sent] = (read $ T.unpack num, sent)
procLine x = error $ "procLine: " ++ show x

splitCols :: Text -> [Text]
splitCols = T.splitOn "\t"

loadTatoeba :: IO [(Int, Text, Text)]
loadTatoeba = do
    l1 <- readNumToSentMap ("tatoeba" </> "simp-cmn.csv")
    l2 <- readNumToSentMap ("tatoeba" </> "eng.csv")
    nums <- HM.toList . HM.fromListWith Set.union .
        map (\(a, b) -> (a, Set.singleton b)) .
        filter (\(a, b) -> a `HM.member` l1 && b `HM.member` l2) .
        map (\[a, b] -> (read $ T.unpack a, read $ T.unpack b)) .
        map splitCols <$> readDataResourceLines ("tatoeba" </> "links.csv")
    let n1s = map fst nums
        zhSentences =
            [fromJust (HM.lookup n1 l1) | n1 <- n1s]
        enSentences =
            [ maximumBy (compare `on` T.length)
              [fromJust (HM.lookup n2 l2) | n2 <- Set.toList n2s]
            | (_, n2s) <- nums
            ]
    return $ zip3 n1s zhSentences enSentences
-}

wdGloss :: Int -> Cedict -> Text -> [WdPinyinGloss]
wdGloss !n !dict !wd = case HM.lookup wd dict of
  Just (CedictEntry p (CedictGloss g)) -> [WdPinyinGloss wd p g]
  Just (CedictEntry p (CedictRef ref)) -> if n > 100
    then error "Depth greater than 100."
    else wdGloss (n + 1) dict ref
  Nothing -> if T.length wd == 1
    then [WdPinyinGloss wd "" ""]
    else concatMap (wdGloss 0 dict . T.singleton) $ T.unpack wd

procTreeLine :: Cedict -> Text -> [WdPinyinGloss]
procTreeLine dict l = wdGloss 0 dict wd where [_, _, _, wd] = T.splitOn "\t" l

getPinyins :: Cedict -> [Text] -> IO [[WdPinyinGloss]]
getPinyins dict sentences = do
    home <- getHomeDirectory
    (Just hIn, Just hOut, _, _) <- createProcess
        (proc (home </> "p/l/melang/zh-sentence-trees.py") [])
        {std_in = CreatePipe, std_out = CreatePipe}
    Conc.forkIO $ mapM_ (T.hPutStrLn hIn) sentences >> hClose hIn
    ls <- TL.lines <$> TL.hGetContents hOut
    return $ map (concatMap (procTreeLine dict . TL.toStrict)) $
        Spl.splitWhen TL.null ls

procTreeLine :: Text -> Text
procTreeLine l = wd where [_, _, _, wd] = T.splitOn "\t" l

getPinyins :: [Text] -> IO [[Text]]
getPinyins sentences = do
    home <- getHomeDirectory
    (Just hIn, Just hOut, _, _) <- createProcess
        (proc (home </> "p/l/melang/zh-sentence-trees.py") [])
        {std_in = CreatePipe, std_out = CreatePipe}
    Conc.forkIO $ mapM_ (T.hPutStrLn hIn) sentences >> hClose hIn
    ls <- TL.lines <$> TL.hGetContents hOut
    return . map (map (procTreeLine . TL.toStrict)) $ Spl.splitWhen TL.null ls

readTmxGz :: String -> IO [(Int, Text, Text)]
readTmxGz filename = do
    let enStart = "      <tuv xml:lang=\"en\"><seg>"
        zhStart = "      <tuv xml:lang=\"zh\"><seg>"
        end = "</seg></tuv>"
        procChunk :: Int -> [TL.Text] -> (Int, Text, Text)
        procChunk i (enL:zhL:_) =
            (i, TL.toStrict zhSentence, TL.toStrict enSentence)
          where
            enSentence = checkDropEnd end $ TL.drop (TL.length enStart) enL
            zhSentence = checkDropEnd end $ checkDropStart zhStart zhL
    (_, Just hOut, _, _) <-
        createProcess (proc "zcat" [filename]) {std_out = CreatePipe}
    ls <- TL.lines <$> TL.hGetContents hOut
    let chunks :: [[TL.Text]]
        chunks = tail $
            Spl.split (Spl.keepDelimsL $ Spl.whenElt (enStart `TL.isPrefixOf`))
            ls
    return $ zipWith procChunk [1..] chunks

loadMultiUN = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    readTmxGz (enZhDir </> "MultiUN.tmx.gz")
    --readTmxGz (enZhDir </> "MultiUNSample.tmx.gz")

loadOpenSubtitles = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    readTmxGz (enZhDir </> "OpenSubtitles.tmx.gz")

spaceCol :: [Text] -> [Text]
spaceCol [] = []
spaceCol rows = zipWith
    (\row width -> row <> T.replicate (maxWidth - width) " ") rows widths
  where
    widths = map (sum . map wcwidth . T.unpack) rows
    maxWidth = maximum widths

lol en wdPinyinGlosses = do
    --when (any (\(WdPinyinGloss w _ _) -> w == "哪怕") wdPinyinGlosses) $ do
        T.putStrLn en
        mapM_ (T.putStrLn . T.intercalate " ") $ transpose
            [spaceCol [w, p, g] | WdPinyinGloss w p g <- wdPinyinGlosses]
        T.putStrLn ""
        hFlush stdout



doSection :: Char -> FilePath -> Int -> [(Int, Text, Text)] -> IO ()
doSection letter _ _ [] = return ()
doSection letter outFilename i entries = do
    e <- doesFileExist (outFilename </> show i)
    if e
      then hPutStrLn stderr $ "Section " ++ show i ++ "/~400 already done."
      else do
        hPutStrLn stderr $ "Doing 100k section number " ++ show i ++ "/~400."
        let (nums, zhSentences, enSentences) = unzip3 curEntries
        t1 <- getCurrentTime
        wdPinyinGlossSentences <- getPinyins zhSentences
        BL.writeFile (outFilename </> show i) $ serialise $
            zip4 (repeat letter) nums wdPinyinGlossSentences enSentences
        t2 <- getCurrentTime
        hPutStrLn stderr $ "Section took " ++ show (t2 `diffUTCTime` t1) ++ "."
    doSection letter outFilename (i + 1) restEntries
  where
    (curEntries, restEntries) = splitAt 10000 entries

doSet :: Char -> (IO [(Int, Text, Text)]) -> FilePath -> IO ()
doSet letter loadFn outFilename = do
    entries <- loadFn
    doSection letter outFilename 1 entries
-}

type SentenceInfo = (Char, Int, [Text], Text)

loadSentenceInfos :: FilePath -> IO [SentenceInfo]
loadSentenceInfos = fmap deserialise . BL.readFile

-- wordTo

main :: IO ()
main = do
    infos <- loadSentenceInfos "MultiUN/1"
    mapM_ print $ take 2 infos
