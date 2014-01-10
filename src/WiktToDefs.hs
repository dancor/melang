{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as DT
import System.Environment
import System.FilePath
import Text.HTML.TagSoup

import Util.BS

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

spPartAbbr :: Str -> Str
spPartAbbr "Adjective" = "ADJ"
spPartAbbr "Adverb" = "ADV"
spPartAbbr "Conjunction" = "CONJ"
spPartAbbr "Noun" = "NOUN"
spPartAbbr "Preposition" = "ADP"
spPartAbbr "Postposition" = "ADP"
spPartAbbr "Pronoun" = "PRON"
spPartAbbr "Verb" = "VERB"
spPartAbbr x = x

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f = dropWhile (not . f)

findLangHeading :: Str -> [Str] -> [Str]
findLangHeading targetLanguageTag = drop 1 . dropUntil (\ l ->
    BSC.isPrefixOf targetLanguageTag l ||
    BSC.isPrefixOf ("      <text xml:space=\"preserve\">" `BSC.append`
        targetLanguageTag) l
    )

processBlock :: [Str] -> Str
processBlock =
    BS.intercalate "; " .
    map head . group .
    map (BSC.pack . processLine . BSC.unpack) .
    map (BS.drop 2) . filter ("# " `BSC.isPrefixOf`) .
    -- The detail of all the sub-sub-headings is too much detail for us.
    takeWhile (not . ("====" `BSC.isPrefixOf`))
  where
    processLine ('[':'[':rest) = toBracketEnd "" rest
    processLine ('{':'{':rest) = toBraceEnd "" rest
    processLine ('\'':'\'':rest) = processLine $ dropWhile (== '\'') rest
    processLine (c:rest) = c : processLine rest
    processLine "" = ""

    toBracketEnd acc (']':']':rest) = bracketMod acc ++ processLine rest
    toBracketEnd acc (c:rest) = toBracketEnd (acc ++ [c]) rest
    -- Shouldn't happen:
    toBracketEnd _ "" = ""

    toBraceEnd acc ('}':'}':rest) = braceMod acc ++ processLine rest
    toBraceEnd acc (c:rest) = toBraceEnd (acc ++ [c]) rest
    -- Shouldn't happen:
    toBraceEnd _ "" = ""

    bracketMod = last . splitWhen (== '|')

    braceMod x = processLine $ case mainPart of
        "," -> ","
        "a" -> "(" ++ parts !! 1 ++ ")"
        "alternative spelling of" -> usualIncl (parts !! 1)
        "apocopic form of" -> usualIncl (parts !! 1)
        "attention" -> ""
        "context" -> "(" ++ parts !! 1 ++ ")"
        "es-demonstrative-accent-usage" -> "(The unaccented form can " ++
            "function as a pronoun if there is no ambiguity as to it " ++
            "being a pronoun in its context.)"
        "es-verb form of" -> "(verb form of " ++ last parts ++ ")"
        "es-verb form of " -> "(verb form of " ++ last parts ++ ")"
        "feminine of" -> usualIncl (parts !! 1)
        "feminine plural of" -> usualIncl (parts !! 1)
        "form of" -> usualIncl $
            last (filter (not . ("lang=" `isPrefixOf`)) parts)
        "gloss" -> theUsual
        "inflection of" -> usualIncl (parts !! 1)
        "label" -> "(" ++ last parts ++ ")"
        "l/en" -> last parts
        "l" -> last parts
        "m" -> "(masculine)"
        "masculine plural of" -> usualIncl (parts !! 1)
        "neuter of" -> usualIncl (parts !! 1)
        "non-gloss definition" -> rest
        "obsolete spelling of" -> usualIncl (parts !! 1)
        "past participle of" -> usualIncl (parts !! 1)
        "plural of" -> usualIncl (parts !! 1)
        "qualifier" -> theUsual
        "rfex" -> ""
        "rfgloss" -> ""
        "sense" -> rest
        "term" -> parts !! 1
        _ -> "<?<" ++ x ++ ">?>"
      where
        parts = splitWhen (== '|') x
        mainPart = head parts
        rest = intercalate "|" $ tail parts
        theUsual = "(" ++ rest ++ ")"
        usualIncl y = "(" ++ mainPart ++ " " ++ y ++ ")"

processContent :: [Str] -> [Str]
processContent =
    map (\(subHead, block) -> spPartAbbr subHead <> ":" <> block) .
    filterGoodBlocks . contentBlocks . dropWhile (not . isSubHead)
  where
    isSubHead x = "===" `BSC.isPrefixOf` x && not ("====" `BSC.isPrefixOf` x)
    extractSubHead x = BS.drop 3 $ BS.take (BS.length x - 3) x
    contentBlocks [] = []
    contentBlocks (subHead:rest) =
        (extractSubHead subHead, processBlock block) :
        contentBlocks rest'
      where
        (block, rest') = break isSubHead rest
    filterGoodBlocks = filter ((`elem` goodPartsOfSpeech) . fst)

processPage :: Set.Set Str -> Str -> [Str] -> Maybe (Str, [Str])
processPage wantedWordSet targetLanguageTag ls =
    if not (null content) && title `Set.member` wantedWordSet
      -- && not (all isAlpha $ BSC.unpack title)
        then Just (title, content)
        else Nothing
  where
    title = BSC.takeWhile (/= '<') . BSC.drop (BSC.length titleLinePrefix) $
        head ls
    content =
        processContent .
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

csvLine :: [DT.Text] -> DT.Text
csvLine = DT.intercalate "," .
    map (\ a -> "\"" `DT.append` DT.replace "\"" "\"\"" a `DT.append` "\"")

procLines :: Set.Set Str -> Str -> [Str] -> [Str]
procLines wantedWordSet targetLanguageTag =
    map showPageSimple .
    catMaybes . map (processPage wantedWordSet targetLanguageTag) .
    partitions (BSC.isPrefixOf titleLinePrefix)

data DictEntry
    = Entry
    { eDef :: Str
    , eStats :: Str
    }

doDefs :: IO ()
doDefs = do
    -- Usage e.g.:
    -- bzless enwiktionary-xxx-pages-articles.xml.bz2 | wikt-to-defs spa
    [lang] <- getArgs
    let targetLanguageTag = BSC.pack $
            case lang of
                "cmn" -> "==Mandarin=="
                "spa" -> "==Spanish=="
                a -> a
    dict <-
        HMS.map (\[def, stats] -> Entry def stats) .
        HMS.fromList . map (uncons . BS.split 9) . BSC.lines <$>
        BS.readFile ("/home/danl/p/l/melang/data" </> lang </> "dict")
    let wantedWordSet = Set.fromList $ map head dict
    bsInteractLErr $ map Right . procLines wantedWordSet targetLanguageTag

main :: IO ()
main = doDefs
