{-# LANGUAGE OverloadedStrings #-}

module Lang.De.DewiktNouns where

import Control.Applicative
import Data.Char
import qualified Data.Set as Set
import qualified Data.Text.Lazy as DT
import qualified Data.Text.Lazy.IO as DT
import Data.List
import Text.HTML.TagSoup

-- A list of 8 forms (nom s, nom p, gen s, gen p, dat s, dat p, akk s, akk p)
type NounForm = [DT.Text]

killDer :: DT.Text -> DT.Text
killDer text = if
    "der " `DT.isPrefixOf` text ||
    "die " `DT.isPrefixOf` text ||
    "das " `DT.isPrefixOf` text ||
    "den " `DT.isPrefixOf` text ||
    "dem " `DT.isPrefixOf` text ||
    "des " `DT.isPrefixOf` text
  then DT.drop 4 text
  else text

readNoun :: [DT.Text] -> NounForm
readNoun [] = error "procForms: empty list"
readNoun (_:ls) = map
    (DT.intercalate " \\ " . map (repls . DT.drop 1 . DT.dropWhile (/= '=')))
    [l1, l3, l5, l7, l2, l4, l6, l8]
  where
    (l1, rest1) = span ("|Nominativ Singular" `DT.isPrefixOf`) ls
    (l2, rest2) = span ("|Nominativ Plural" `DT.isPrefixOf`) rest1
    (l3, rest3) = span ("|Genitiv Singular" `DT.isPrefixOf`) rest2
    (l4, rest4) = span ("|Genitiv Plural" `DT.isPrefixOf`) rest3
    (l5, rest5) = span ("|Dativ Singular" `DT.isPrefixOf`) rest4
    (l6, rest6) = span ("|Dativ Plural" `DT.isPrefixOf`) rest5
    (l7, rest7) = span ("|Akkusativ Singular" `DT.isPrefixOf`) rest6
    (l8, _)     = span ("|Akkusativ Plural" `DT.isPrefixOf`) rest7
    repls =
        DT.replace "; " " \\ " .
        DT.dropAround (== ' ') .
        -- DT.replace "/" "" .
        DT.replace "&lt;small&gt;" "" .
        DT.replace "&lt;/small&gt;" "" .
        DT.replace "&lt;br&gt;" "; " .
        DT.replace "&lt;br/&gt;" "; " .
        DT.replace "&lt;br &gt;" "; " .
        DT.replace "&lt;br /&gt;" "; " .
        DT.replace "&lt;/br&gt;" "; " .
        DT.replace "&lt;/br/&gt;" "; " .
        DT.replace "(" "" .
        DT.replace ")" ""
        -- &lt;!-- von oben der Tag übernommen --&gt;
        -- &lt;ref&gt;Dativ und Akkusativ  ...

readNouns :: DT.Text -> [NounForm]
readNouns = map readNoun .
    partitions (== "{{Deutsch Substantiv Übersicht") .
    DT.lines

showNouns :: [NounForm] -> DT.Text
showNouns = DT.unlines . intercalate [""]

readDewiktLazy :: IO DT.Text
readDewiktLazy = DT.readFile "/home/danl/data/wikt/de/cur"

pullWds :: DT.Text -> [DT.Text]
pullWds text = case DT.dropWhile (not . isAlpha) text of
  "" -> []
  wdRest -> wd : pullWds rest where (wd, rest) = DT.span isAlpha wdRest

readStoryWds :: IO (Set.Set DT.Text)
readStoryWds =
    Set.fromList . pullWds <$> DT.readFile
    "/home/danl/l/l/de/audio/book/Herrn-Arnes-Schatz/Kapitel-01.txt"

{-
wdsToNounForms :: DT.Text -> [DT.Text] -> [Maybe NounForm]
wdsToNounForms wikt wds =

wdsToNounFormsUnord :: DT.Text -> Set.Set DT.Text -> Map DT.Text NounForm
wdsToNounFormsUnord wikt wds =
    filter (Set.member wds) .
    partitions (== "{{Deutsch Substantiv Übersicht") $
    DT.lines wikt
    -}
