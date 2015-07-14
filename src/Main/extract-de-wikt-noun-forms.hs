{-# LANGUAGE OverloadedStrings #-}

#include <h>

import Data.Monoid

import Util.BS

pullForms :: [BS.ByteString] -> BS.ByteString
pullForms ls = BS.intercalate "|" $ map cleanUp
    [nomS, nomP, genS, genP, datS, datP, akkS, akkP]
  where
    nomS:_ = takeWhile ("|Nominativ Singular" `BS.isPrefixOf`) ls
    nomP:_ = takeWhile ("|Nominativ Plural" `BS.isPrefixOf`) ls
    genS:_ = takeWhile ("|Genitiv Singular" `BS.isPrefixOf`) ls
    genP:_ = takeWhile ("|Genitiv Plural" `BS.isPrefixOf`) ls
    datS:_ = takeWhile ("|Dativ Singular" `BS.isPrefixOf`) ls
    datP:_ = takeWhile ("|Dativ Plural" `BS.isPrefixOf`) ls
    akkS:_ = takeWhile ("|Akkusativ Singular" `BS.isPrefixOf`) ls
    akkP:_ = takeWhile ("|Akkusativ Plural" `BS.isPrefixOf`) ls
    cleanUp =
        bsReplace "&lt;small&gt;" "" . bsReplace "&lt;/small&gt;" "" .
        BSC.dropWhile (/= '=')

extractNounForms :: [BS.ByteString] -> [BS.ByteString]
extractNounForms [] = []
extractNounForms (l:ls) =
    if "{{Deutsch Substantiv Übersicht" `BS.isPrefixOf` l
      then
        let (formLines, rest) = span ("|" `BS.isPrefixOf`) ls
        in pullForms formLines : extractNounForms rest
      else extractNounForms ls

main :: IO ()
main = bsInteractL extractNounForms
