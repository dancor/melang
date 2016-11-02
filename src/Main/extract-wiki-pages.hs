{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import Debug.Trace

import Util.BS

type Str = BS.ByteString

inTitlePrefix :: Str
inTitlePrefix = "    <title>"

inTitlePrefixLen :: Int
inTitlePrefixLen = BS.length inTitlePrefix

textPrefix :: Str
textPrefix = "      <text xml:space=\"preserve\">"

textPrefixLen :: Int
textPrefixLen = BS.length textPrefix

textOverPrefix :: Str
textOverPrefix = "      <sha1>"

outTitlePrefix :: Str
outTitlePrefix = "^"

chopN :: Int -> Str -> Str
chopN !n !s = BS.take (BS.length s - n) s

wikiPullTitleTexts :: [Str] -> [Str]
wikiPullTitleTexts ls =
    case ls2 of
      [] -> []
      _ -> 
        (outTitlePrefix <> chopN 8 (BS.drop inTitlePrefixLen titleLine)) :
        killIfFinalEndTextTag (BS.drop textPrefixLen textLine) :
        map scanForOutTitlePrefix (killFinalEndTextTag pageContentLines) ++
        wikiPullTitleTexts (drop 1 rest)
  where
    ls2 = dropWhile (not . (inTitlePrefix `BS.isPrefixOf`)) ls
    titleLine:ls3 = ls2
    ls4 = dropWhile (not . (textPrefix `BS.isPrefixOf`)) ls3
    textLine:ls5 = ls4
    (pageContentLines, rest) = break (textOverPrefix `BS.isPrefixOf`) ls5

killIfFinalEndTextTag :: Str -> Str
killIfFinalEndTextTag a = if "</text>" `BS.isSuffixOf` a then chopN 7 a else a

killFinalEndTextTag :: [Str] -> [Str]
killFinalEndTextTag [a] =
    if "</text>" `BS.isSuffixOf` a
      then [chopN 7 a]
      else trace "Warning: page text had no </text>" [a]
killFinalEndTextTag (a:rest) = a : killFinalEndTextTag rest
killFinalEndTextTag [] = []

scanForOutTitlePrefix :: Str -> Str
scanForOutTitlePrefix a = if "^" `BS.isPrefixOf` a then BSC.cons ' ' a else a

main :: IO ()
main = bsInteractL wikiPullTitleTexts
