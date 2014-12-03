{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.Monoid
import System.Environment
import Text.HTML.TagSoup

import Util.BS

type Str = BS.ByteString

titleLinePrefix :: Str
titleLinePrefix = "    <title>"

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f = dropWhile (not . f)

findLangHeading :: Str -> [Str] -> [Str]
findLangHeading targetLanguageTag = drop 1 . dropUntil (\ l ->
    BSC.isPrefixOf targetLanguageTag l ||
    BSC.isPrefixOf ("      <text xml:space=\"preserve\">" `BSC.append`
        targetLanguageTag) l
    )

-- Oddly, ^ never appears at the beginning of a line in the Spanish
-- wiktionary at least.
titleMagic :: BS.ByteString
titleMagic = "^"

processPage :: Str -> [Str] -> Maybe [Str]
processPage targetLanguageTag ls =
    if null content
        then Nothing
        else Just (title : content)
  where
    title = titleMagic <> BSC.takeWhile (/= '<')
        (BSC.drop (BSC.length titleLinePrefix) $ head ls)
    content =
        filter (not . BS.null) .
        takeWhile (not . (BS.isPrefixOf "----")) $
        findLangHeading targetLanguageTag ls

processLines :: Str -> [Str] -> [Str]
processLines targetLanguageTag =
    concat .
    catMaybes .
    map (processPage targetLanguageTag) .
    partitions (BSC.isPrefixOf titleLinePrefix)

main :: IO ()
main = do
    args <- getArgs
    targetLanguageTag <- BSC.pack <$> case args of
      ["cmn"] -> return "==Mandarin=="
      ["ger"] -> return "==German=="
      ["spa"] -> return "==Spanish=="
      [a] -> return a
      _ -> error $ concat
        [ "usage e.g.: /usr/bin/time bzcat "
        , "~/data/wikt/enwiktionary-pages-articles.xml.bz2 | "
        , "./wikt-pull-lang ==Spanish== > ~/data/wikt/spa"
        ]
    bsInteractLErr $ map Right . processLines targetLanguageTag
