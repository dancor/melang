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
import Data.Maybe
import Data.Monoid
import System.Environment
import System.FilePath
import Text.HTML.TagSoup

import Util.BS

type Str = BS.ByteString

type Dict = HMS.HashMap Str DictEntry

type Def = Either Str [(Str, [Str])]

data DictEntry
    = Entry
    { eDef :: Def
    , eStats :: Str
    , eN :: Int
    }

goodPartsOfSpeech :: [Str]
goodPartsOfSpeech = map (BSC.pack . filter isLetter)
    [ "Adjective"
    , "Adverb"
    , "Article"
    , "Cardinal numeral"
    , "Conjunction"
    , "Contraction"
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
spPartAbbr "Article" = "DET"
spPartAbbr "Cardinal numeral" = "NUM"
spPartAbbr "Conjunction" = "CONJ"
spPartAbbr "Contraction" = "ABBR"
spPartAbbr "Noun" = "NOUN"
spPartAbbr "Preposition" = "ADP"
spPartAbbr "Postposition" = "ADP"
spPartAbbr "Pronoun" = "PRON"
spPartAbbr "Verb" = "VERB"
spPartAbbr x = x

processBlock :: Str -> [Str] -> Str
processBlock subSubHeadingPrefix =
    BS.intercalate "; " .
    map head . group .
    map (BSC.pack . processLine . BSC.unpack) .
    map (BS.drop 2) . filter ("# " `BSC.isPrefixOf`) .
    -- The detail of all the sub-sub-headings is too much detail for us.
    takeWhile (not . (subSubHeadingPrefix `BSC.isPrefixOf`))
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

testStr :: [Str]
testStr =
    [ "===Etymology 1==="
    , "From {{etyl|la|es}} {{term|ille|lang=la}}."
    , "====Article===="
    , "'''el''' (plural: [[los]]; feminine: [[la]]; plural feminine: [[las]]; neuter: [[lo]])"
    , "# Masculine singular definite article; [[the]]."
    ]

processContent :: [Str] -> Str
processContent content = newDef
  where
    newDef =
        BS.intercalate "; " .
        map (\(subHead, block) ->
            spPartAbbr subHead <> ":" <> block) .
        filterGoodBlocks . procHeadings $ dropWhile (not . isHeading)
        content
    filterGoodBlocks = filter ((`elem` goodPartsOfSpeech) . fst)
    isHeading x = "===" `BSC.isPrefixOf` x
    isHeading4Plus x = "====" `BSC.isPrefixOf` x
    isHeading5Plus x = "=====" `BSC.isPrefixOf` x
    extractHeading n x = BS.drop n $ BS.take (BS.length x - n) x

    procHeadings [] = []
    procHeadings (heading:rest) =
        if "===Etymology" `BSC.isPrefixOf` heading
          then proc4Headings rest'
          else
            if isHeading4Plus heading
              then procHeadings rest'
              else
                (extractHeading 3 heading, processBlock "====" block) :
                procHeadings rest'
      where
        (block, rest') = break isHeading rest

    proc4Headings [] = []
    proc4Headings x@(heading:rest) =
        if isHeading5Plus heading
          then proc4Headings rest'
          else
            if isHeading4Plus heading
              then
                (extractHeading 4 heading, processBlock "=====" block) :
                proc4Headings rest'
              -- else: It's a === heading, so leave proc4Headings.
              else procHeadings x
      where
        (block, rest') = break isHeading rest

processPage :: Dict -> [Str] -> Dict
processPage !dict [] = dict
processPage !dict (magicTitle:ls) =
    if not (BS.null content) && title `HMS.member` dict
      -- && not (all isAlpha $ BSC.unpack title)
        then HMS.adjust (\e -> e {eDef = readDef content}) title dict
        else dict
  where
    title = BSC.drop 1 magicTitle
    content = processContent ls

procLines :: Dict -> [Str] -> Dict
procLines dict =
    foldl' processPage dict .
    partitions ("^" `BSC.isPrefixOf`)

-- break to a Maybe can be more natural.
breakSubstr :: Str -> Str -> Maybe (Str, Str)
breakSubstr needle haystack =
  let (pre, needlePost) = BS.breakSubstring needle haystack
      post = BS.drop (BS.length needle) needlePost
  in if BS.null needlePost then Nothing else Just (pre, post)

breaksSubstr :: Str -> Str -> [Str]
breaksSubstr needle haystack =
    if BS.null needlePost then [haystack] else pre : breaksSubstr needle post
  where
    (pre, needlePost) = BS.breakSubstring needle haystack
    post = BS.drop (BS.length needle) needlePost

doDeref :: Dict -> Str -> Str -> Str -> Str
doDeref dict spPart needle s =
  case breakSubstr needle s of
    Nothing -> s
    Just (pre, post) ->
        if ":" `BS.isInfixOf` ref
          -- Already has a refDef.
          then pre <> needle <> ref <> derefVerb dict rest
          else
            pre <> needle <> ref <> ": " <> refDef <> derefVerb dict rest
      where
        (ref, rest) = BSC.break (== ')') post
        refDef = maybe "???" (modDef . eDef) (HMS.lookup ref dict)
        modDef (Left x) = x
        modDef (Right x) = maybe "???" head $ lookup spPart x

derefVerb :: Dict -> Str -> Str
derefVerb dict =
    doDeref dict "VERB" "(verb form of: " .
    doDeref dict "NOUN" "(plural of: "

readDef :: Str -> Def
readDef s =
    if startsSpPart s
      then
        Right . map headPullSpPart . partitions startsSpPart $
        breaksSubstr "; " s
      else Left s
  where
    startsSpPart x = ":" `BS.isPrefixOf` BSC.dropWhile isUpper x
    breakSpPart = fromJust . breakSubstr ":"
    headPullSpPart (x:xs) = (spPart, def1:xs)
      where (spPart, def1) = breakSpPart x
    headPullSpPart _ = error "headPullSpPart: empty list"

showDef :: Def -> Str
showDef (Left s) = s
showDef (Right xs) = BS.intercalate "; "
    [spPart <> ":" <> BS.intercalate "; " def | (spPart, def) <- xs]

onEachDefLine :: (Str -> Str) -> Def -> Def
onEachDefLine f (Left x) = Left $ f x
onEachDefLine f (Right xs) = Right [(spPart, map f ls) | (spPart, ls) <- xs]

main :: IO ()
main = do
    -- Usage e.g.:
    -- /usr/bin/time < ~/data/wikt/spa ./wikt-to-defs spa > out
    [lang] <- getArgs
    dict <- HMS.fromList .
        zipWith (\n [word, def, stats] ->
            (word, Entry (readDef def) stats n)) [1..] .
        map (BS.split 9) . BSC.lines <$>
        BS.readFile ("/home/danl/p/l/melang/data" </> lang </> "dict")
    bsInteractLErr $ map Right .
        map (\(word, e) -> BS.intercalate "\t" [word,
            showDef . onEachDefLine (derefVerb dict) $ eDef e, eStats e]) .
        sortBy (compare `on` (eN . snd)) . HMS.toList .
        procLines dict
