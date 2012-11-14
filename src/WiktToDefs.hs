{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import System.Environment
import Text.HTML.TagSoup

import BSUtil

type Str = BS.ByteString

titleLinePrefix :: Str
titleLinePrefix = "    <title>"

goodPartsOfSpeech :: [Str]
goodPartsOfSpeech = map (BSC.pack . filter isLetter)
    [ "Adjective"
    , "Adverb"
    -- , "Article"
    , "Conjunction"
    -- , "Idiom"
    -- , "Interjection"
    -- , "Measure word" -- this actually gives the MW for a noun def
    , "Noun"
    -- , "Particle"
    -- , "Phrase"
    -- , "Place word"
    , "Preposition"
    -- , "Prefix"
    , "Postposition"
    , "Pronoun"
    -- , "Proverb"
    -- , "Suffix"
    , "Verb"
    ]

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f = dropWhile (not . f)

findLangHeading :: Str -> [Str] -> [Str]
findLangHeading targetLanguageTag = drop 1 . dropUntil (\ l ->
    BSC.isPrefixOf targetLanguageTag l ||
    BSC.isPrefixOf ("      <text xml:space=\"preserve\">" `BSC.append`
        targetLanguageTag) l
    )

processPage :: Str -> [Str] -> Maybe (Str, [Str])
processPage targetLanguageTag ls =
    if null content || all isAlpha (BSC.unpack title)
        then Nothing
        else Just (title, content)
  where
    title = BSC.takeWhile (/= '<') . BSC.drop (BSC.length titleLinePrefix) $
        head ls
    content =
        takeWhile (not . (BSC.isPrefixOf "----")) $
        findLangHeading targetLanguageTag ls

showPage :: (Str, [(Str, [Str])]) -> Str
showPage (title, parts) =
    BSC.unlines ("%$#@!":title:content)
  where
    content =
        concatMap (\ (heading, xs) -> (heading `BSC.append` ":"): xs) parts

showPageSimple :: (Str, [Str]) -> Str
showPageSimple (title, content) =
    BSC.unlines ("%$#@!":title:content)

takeBetween :: BS.ByteString
            -> BS.ByteString
            -> BS.ByteString
            -> BS.ByteString
takeBetween leftPart rightPart =
    fst . BS.breakSubstring rightPart . BS.drop (BS.length leftPart) .
    snd . BS.breakSubstring leftPart

{-
pageGetGood :: (Str, [Str]) -> Dict
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
-}

{-
loadFreqInfo :: IO [(DT.Text, Int)]
loadFreqInfo = do
  let
    f :: [DT.Text] -> (DT.Text, Int)
    f [w, n] = (w, read $ DT.unpack n)
  map (f . DT.words) . DT.lines <$>
    DTI.readFile "/home/danl/p/l/melang/out/gbRec/freq"
-}

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

{-
loadDict :: IO Dict
loadDict = do
    concatMap pageGetGood <$> decodeFile "wiktionaryMandarin.bin"
-}

csvLine :: [DT.Text] -> DT.Text
csvLine = DT.intercalate "," .
    map (\ a -> "\"" `DT.append` DT.replace "\"" "\"\"" a `DT.append` "\"")

procLines :: Str -> [Str] -> [Str]
procLines targetLanguageTag =
    map showPageSimple .
    catMaybes . map (processPage targetLanguageTag) .
    partitions (BSC.isPrefixOf titleLinePrefix)

main :: IO ()
main = do
    [lang] <- getArgs
    let targetLanguageTag = BSC.pack . ("==" ++) . (++ "==") $
            case lang of
                "cmn" -> "Mandarin"
                "spa" -> "Spanish"
                a -> a
    bsInteractLErr $ map Right . procLines targetLanguageTag
{-
{-
  let
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
-}
