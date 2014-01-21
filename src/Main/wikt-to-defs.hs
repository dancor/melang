{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Function
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.List.Split
import Data.Monoid
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
        "a" -> argsParen
        "alternative spelling of" -> mainParen
        "apocopic form of" -> mainParen
        "attention" -> ""
        "comparative of" -> mainParen
        "context" -> argsParen
        "conjugation of" -> mainParen
        "cx" -> argsParen
        "es-compond of" -> customParen "compound of"
        "es-demonstrative-accent-usage" -> "(The unaccented form can " ++
            "function as a pronoun if there is no ambiguity as to it " ++
            "being a pronoun in its context.)"
        "es-verb form of" -> customParen "verb form of"
        "es-verb form of " -> customParen "verb form of"
        "f" -> "(feminine)"
        "feminine of" -> mainParen
        "feminine plural of" -> mainParen
        "form of" -> mainParen
        "gloss" -> argsParen
        "gloss-stub" -> ""
        "inflection of" -> mainParen
        "label" -> argsParen
        "l/en" -> argsStr
        "l" -> argsStr
        "m" -> "(masculine)"
        "masculine plural of" -> mainParen
        "misspelling of" -> mainParen
        "neuter of" -> mainParen
        "n-g" -> argsStr
        "non-gloss definition" -> argsStr
        "obsolete spelling of" -> mainParen
        "past participle of" -> mainParen
        "plural of" -> mainParen
        "present participle of" -> mainParen
        "qualifier" -> argsParen
        "reflexive of" -> mainParen
        "rfex" -> ""
        "rfgloss" -> ""
        "sense" -> argsStr
        "taxlink" -> argsParen
        "term" -> argsStr
        _ -> "<?<" ++ x ++ ">?>"
      where
        (mainPart:restParts) = splitWhen (== '|') x
        isBadPart part =
            part `elem` ["", "en", "es", "su", "f", "s"] ||
            "=" `isInfixOf` part
        goodParts = filter (not . isBadPart) restParts
        argsStr = intercalate ", " goodParts
        argsParen = "(" ++ argsStr ++ ")"
        mainParen = customParen mainPart
        customParen y = "(" ++ y ++ ": " ++ argsStr ++ ")"

processContent :: [Str] -> Str
processContent content = newDef
  where
    newDef =
        BS.intercalate "; " .
        map (\(subHead, block) -> spPartAbbr subHead <> ":" <> block) .
        filterGoodBlocks . contentBlocks $ dropWhile (not . isSubHead)
        content
    isSubHead x = "===" `BSC.isPrefixOf` x && not ("====" `BSC.isPrefixOf` x)
    extractSubHead x = BS.drop 3 $ BS.take (BS.length x - 3) x
    contentBlocks [] = []
    contentBlocks (subHead:rest) =
        (extractSubHead subHead, processBlock block) :
        contentBlocks rest'
      where
        (block, rest') = break isSubHead rest
    filterGoodBlocks = filter ((`elem` goodPartsOfSpeech) . fst)

processPage :: Str -> Dict -> [Str] -> Dict
processPage targetLanguageTag !dict ls =
    if not (BS.null content) && title `HMS.member` dict
      -- && not (all isAlpha $ BSC.unpack title)
        then HMS.adjust (\e -> e {eDef = content}) title dict
        else dict
  where
    title = BSC.takeWhile (/= '<') . BSC.drop (BSC.length titleLinePrefix) $
        head ls
    content =
        processContent .
        takeWhile (not . (BSC.isPrefixOf "----")) $
        findLangHeading targetLanguageTag ls

procLines :: Str -> Dict -> [Str] -> Dict
procLines targetLanguageTag dict =
    foldl' (processPage targetLanguageTag) dict .
    partitions (BSC.isPrefixOf titleLinePrefix)

type Dict = HMS.HashMap Str DictEntry

data DictEntry
    = Entry
    { eDef :: Str
    , eStats :: Str
    , eN :: Int
    }

doDefs :: IO ()
doDefs = do
    -- Usage e.g.:
    -- bzless enwiktionary-pages-articles.xml.bz2 | wikt-to-defs spa > out
    [lang] <- getArgs
    let targetLanguageTag = BSC.pack $
            case lang of
                "cmn" -> "==Mandarin=="
                "spa" -> "==Spanish=="
                a -> a
    dict <- HMS.fromList .
        zipWith (\n [word, def, stats] -> (word, Entry def stats n)) [1..] .
        map (BS.split 9) . BSC.lines <$>
        BS.readFile ("/home/danl/p/l/melang/data" </> lang </> "dict")
    bsInteractLErr $ map Right .
        map (\(word, e) -> BS.intercalate "\t" [word, eDef e, eStats e]) .
        sortBy (compare `on` (eN . snd)) . HMS.toList .
        procLines targetLanguageTag dict

main :: IO ()
main = doDefs
