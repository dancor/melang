module Main where

#include <h>

import Text.HTML.TagSoup
import qualified Data.Text.Lazy.Encoding as DTLE

type Strn = BSL.ByteString
type StrS = BS.ByteString

titleLinePrefix :: Strn
titleLinePrefix = "    <title>"

targetLanguageTag :: Strn
targetLanguageTag = "==Mandarin=="

goodPartsOfSpeech :: [String]
goodPartsOfSpeech = map (filter isLetter) [
  "Adjective",
  "Adverb",
  -- "Article",
  "Conjunction",
  -- "Idiom",
  -- "Interjection",
  -- "Measure word", -- this actually gives the MW for a noun def
  "Noun",
  -- "Particle",
  -- "Phrase",
  -- "Place word",
  "Preposition",
  -- "Prefix",
  -- "Postposition",
  "Pronoun",
  -- "Proverb",
  -- "Suffix",
  "Verb"]

findLangHeading =
  drop 1 .
  dropWhile (not . (\ l -> BSLC.isPrefixOf targetLanguageTag l ||
    BSLC.isPrefixOf ("      <text xml:space=\"preserve\">" `BSLC.append`
      targetLanguageTag) l))

processPage ls =
  if null content || all isAlpha (BSLC.unpack title)
    then Nothing
    else Just (title, content)
  where
  title = BSLC.takeWhile (/= '<') . BSLC.drop (BSLC.length titleLinePrefix) $
    head ls
  content =
    takeWhile (not . (BSLC.isPrefixOf "----")) .
    findLangHeading $
    ls

--showPage :: (StrS, [StrS]) -> (DT.Text, DT.Text)
--showPage :: (StrS, [StrS]) -> DT.Text
showPage :: (StrS, [(StrS, [StrS])]) -> DT.Text
showPage (title, parts) =
  --(DTLE.decodeUtf8 title, DTLE.decodeUtf8 (BSLC.unlines content))
  DTE.decodeUtf8 (BSC.unlines ("%$#@!":title:content))
  where
  content = concatMap (\ (heading, xs) -> (heading `BSC.append` ":"): xs) parts

takeBetween :: BS.ByteString -> BS.ByteString -> BS.ByteString ->
  BS.ByteString
takeBetween leftPart rightPart =
  fst . BS.breakSubstring rightPart . BS.drop (BS.length leftPart) .
  snd . BS.breakSubstring leftPart

pageGetGood :: (StrS, [StrS]) -> Dict
pageGetGood (title, content) =
  if null goodParts
    then []
    else [(DTE.decodeUtf8 title, goodParts)]
  where
  contentParts = partitions (BSC.isPrefixOf "==") content
  procPart (x:xs) =
    if heading `elem` goodPartsOfSpeech
      then
        Just (DTE.decodeUtf8 $ BSC.pack heading, cleanDictEntry xs)
      else Nothing
    where
    heading = filter isLetter (BSC.unpack x)
  cleanDictEntry =
    catMaybes .
    map cleanPinyin .
    filter (not . BS.null)
  cleanPinyin l =
    if BS.isPrefixOf "{{" l
      then
        if BS.isPrefixOf "{{cmn-" l
          -- then Just . DTE.decodeUtf8 $ takeBetween "|pint=" "|" l
          then Just . DTE.decodeUtf8 $ takeBetween "|pin=" "|" l
          else Nothing
      else
        if BS.isPrefixOf "#" l || BS.isPrefixOf "*" l
          then Just $ DTE.decodeUtf8 l
          else Nothing
{-
        if BS.isPrefixOf "[[Category:" l
          then Nothing
          else Just $ DTE.decodeUtf8 l
-}
  goodParts = catMaybes $ map procPart contentParts

loadFreqInfo :: IO [(DT.Text, Int)]
loadFreqInfo = do
  let
    f :: [DT.Text] -> (DT.Text, Int)
    f [w, n] = (w, read $ DT.unpack n)
  map (f . DT.words) . DT.lines <$>
    DTI.readFile "/home/danl/p/l/melang/out/gbRec/freq"

type Dict = [(DictWord, [(POS, RestOfEntry)])]
type DictWord = DT.Text
type POS = DT.Text
type RestOfEntry = [DT.Text]

choosePartOfSpeech :: DT.Text -> Dict -> [(DictWord, RestOfEntry)]
choosePartOfSpeech partOfSpeech =
  map (second (snd . head)) .
  filter (not . null . snd) .
  map (\ (title, parts) ->
    (title, filter ((== partOfSpeech) . fst) parts))

loadDict :: IO Dict
loadDict = do
  concatMap pageGetGood <$> decodeFile "wiktionaryMandarin.bin"

csvLine :: [DT.Text] -> DT.Text
csvLine = DT.intercalate "," .
  map (\ a -> "\"" `DT.append` DT.replace "\"" "\"\"" a `DT.append` "\"")

main = do
  {-
  ls <- BSLC.lines <$> BSL.readFile (
    "/home/danl/Downloads/enwiktionary-20120812-pages-articles.xml")
  let
    pages = catMaybes . map processPage $
      partitions (BSLC.isPrefixOf titleLinePrefix) ls
  --encodeFile "wiktionaryMandarin.bin" $ map showPage pages
  encodeFile "wiktionaryMandarin.bin" pages
  -}

  dict <- loadDict

  freqInfo <- M.fromList <$> loadFreqInfo

  let
    topOf :: [(DictWord, a)] -> [(DictWord, a)]
    topOf wdsAndInfo =
      map fst . reverse . sortBy (comparing snd) $
      map (\ (wd, x) -> ((wd, x), M.lookup wd freqInfo)) wdsAndInfo
    topOfPartOfSpeech partOfSpeech dict =
      topOf $
      choosePartOfSpeech partOfSpeech dict

  forM_ goodPartsOfSpeech $ \ partOfSpeech -> do
    let
      fHm (wd, (pronounce:entry)) =
        Just $ csvLine [wd, pronounce,
          DT.dropWhileEnd isSpace $ DT.unlines entry]
      fHm (wd, []) = Nothing
      result =
        catMaybes . map fHm .
{-
        zipWith (\ n (wd, entry) -> DT.unlines
          (({-DT.pack (show n ++ ": ") `DT.append` -}wd) : entry)) [1..] .
        --take 100000 $
-}
        -- for some wack prepositions:
        filter (not . (DT.isInfixOf ".") . fst) $
        topOfPartOfSpeech (DT.pack partOfSpeech) dict
    DTI.writeFile ("out/wikt/" ++ partOfSpeech ++ ".csv") $ DT.unlines result
    putStrLn $ partOfSpeech ++ ": " ++ show (length result)
  --print $ length $ choosePartOfSpeech "Verb" dict
  --print $ length $ choosePartOfSpeech "Noun" dict

  --DTI.putStr . DT.unlines . map showPage $ goodPages
