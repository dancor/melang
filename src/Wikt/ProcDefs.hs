{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Wikt.ProcDefs where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Text.HTML.TagSoup

type Str = BS.ByteString

type Dict = HMS.HashMap Str DictEntry

type Def = Either Str [(SpPart, [Str])]

type SpPart = Str

data DictEntry
    = Entry
    { eWd :: Str
    , eDef :: Def
    , eStats :: Str
    , eN :: Int
    }

type Lang = String

goodPartsOfSpeech :: [Str]
goodPartsOfSpeech = map BSC.pack
    [ "Adjective"
    , "Adverb"
    , "Article"
    , "Cardinal numeral"
    , "Conjunction"
    , "Contraction"
    , "Determiner"
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
spPartAbbr "Determiner" = "DET"
spPartAbbr "Noun" = "NOUN"
spPartAbbr "Preposition" = "ADP"
spPartAbbr "Postposition" = "ADP"
spPartAbbr "Pronoun" = "PRON"
spPartAbbr "Verb" = "VERB"
spPartAbbr x = x

processBlock :: Int -> [Str] -> Str
processBlock blockDepth =
    BS.intercalate "; " .
    map head . group .
    map (BSC.pack . processLine . BSC.unpack) .
    map (\x -> if "# " `BSC.isPrefixOf` x then BS.drop 2 x else x) .
    filter (\x -> "# " `BSC.isPrefixOf` x || "{{de-noun|" `BSC.isPrefixOf` x) .
    -- The detail of all the sub-sub-headings is too much detail for us.
    takeWhile (not . (subSubHeadingPrefix `BSC.isPrefixOf`))
  where
    subSubHeadingPrefix = BSC.replicate (blockDepth + 1) '='

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
        "de-noun" -> intercalate "|" restParts
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
processContent = procGoodSects . langSectToGoodSects

langSectToGoodSects :: [Str] -> [(Str, (Int, [Str]))]
langSectToGoodSects =
    filterGoodBlocks . procHeadings . dropWhile (not . isHeading)
  where
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
                (extractHeading 3 heading, (3, block)) :
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
                (extractHeading 4 heading, (4, block)) :
                proc4Headings rest'
              -- else: It's a === heading, so leave proc4Headings.
              else procHeadings x
      where
        (block, rest') = break isHeading rest

procGoodSects :: [(Str, (Int, [Str]))] -> Str
procGoodSects =
    BS.intercalate "; " .
    map (\(subHead, (depth, block)) ->
        spPartAbbr subHead <> ":" <> processBlock depth block)

-- If a wiktionary entry is a case-sensitive match for a dictionary word,
-- always add the wiktionary definition to the dictionary.
-- Only add a case-insensitive match when the dictionary has no definition.
processPage :: Dict -> [Str] -> Dict
processPage !dict [] = dict
processPage !dict (magicTitle:ls) =
    if BS.null content then dict else
    case HMS.lookup key dict of
      Nothing -> dict
      Just e -> if eDef e == Left "???" || eWd e == title
        then HMS.insert key (e {eDef = readDef content}) dict
        else dict
  where
    title = BSC.drop 1 magicTitle
    key = BSC.map toLower title
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

doDeref :: Dict -> SpPart -> Str -> Str -> Str
doDeref dict spPart needle s =
  case breakSubstr needle s of
    Nothing -> s
    Just (pre, post) -> pre <> needle <> refRepl <> derefVerb dict spPart rest
      where
        (refWithPossibleOldDef, rest) = BSC.break (== ')') post
        (ref, possibleOldDef) = BSC.break (== ':') refWithPossibleOldDef
        refDef =
            maybe "???" (modDef . eDef) (HMS.lookup (BSC.map toLower ref) dict)
        modDef (Left x) = x
        modDef (Right x) = maybe "???" head $ lookup spPart x
        refRepl = if ")" `BS.isPrefixOf`
            BSC.dropWhile (== '?') (BSC.dropWhile (== ' ') possibleOldDef)
          then ref <> ": " <> refDef
          else refWithPossibleOldDef

derefVerb :: Dict -> SpPart -> Str -> Str
derefVerb dict spPart =
    doDeref dict spPart "(apocopic form of: "     .
    doDeref dict spPart "(feminine of: "          .
    doDeref dict spPart "(feminine plural of: "   .
    doDeref dict spPart "(form of: "              .
    doDeref dict spPart "(masculine plural of: "  .
    doDeref dict spPart "(obsolete spelling of: " .
    doDeref dict spPart "(plural of: "            .
    doDeref dict spPart "(verb form of: "

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

onEachDefLine :: (SpPart -> Str -> Str) -> Def -> Def
onEachDefLine f (Left x) = Left $ f "??" x
onEachDefLine f (Right xs) =
    Right [(spPart, map (f spPart) ls) | (spPart, ls) <- xs]
