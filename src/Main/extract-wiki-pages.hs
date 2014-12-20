{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BSC
import Data.Monoid

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

-- Chosen randomly with odd characters.
outTitlePrefix :: Str
outTitlePrefix = "^$#`"

chopN :: Int -> Str -> Str
chopN !n !s = BS.take (BS.length s - n) s

{-
breakAndSepLast :: (a -> Bool) -> [a] -> (([a], a), [a])
-}

wikiPullTitleTexts :: [Str] -> [Str]
wikiPullTitleTexts ls =
    case ls2 of
      [] -> []
      _ -> 
        (outTitlePrefix <> chopN 8 (BS.drop inTitlePrefixLen titleLine)) :
        BS.drop textPrefixLen textLine :
        ls6 ++
        wikiPullTitleTexts (drop 1 rest)
  where
    ls2 = dropWhile (not . (inTitlePrefix `BS.isPrefixOf`)) ls
    titleLine:ls3 = ls2
    ls4 = dropWhile (not . (textPrefix `BS.isPrefixOf`)) ls3
    textLine:ls5 = ls4
    (ls6, rest) = break (textOverPrefix `BS.isPrefixOf`) ls5

{-
killEndText :: [Str] -> [Str]
killEndText (a:b:rest) =
    if "</text>" `BS.isSuffixOf` a &&
        outTitlePrefix `BS.isPrefixOf` b
      then chopN 7 a : killEndText (b:rest)
      else a : killEndText (b:rest)
killEndText [a] =
    if "</text>" `BS.isSuffixOf` a
      then [chopN 7 a]
      else [a]
killEndText [] = []
-}

main :: IO ()
main = bsInteractL wikiPullTitleTexts
--main = bsInteractL killEndText
