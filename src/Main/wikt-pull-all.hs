{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.Monoid
import Text.HTML.TagSoup

import Util.BS

type Str = BS.ByteString

titleMagic :: Str
titleMagic = "^@# "

titleLinePrefix :: Str
titleLinePrefix = "    <title"

bodyLinePrefix :: Str
bodyLinePrefix = "      <text xml:space=\"preserve\""

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f = dropWhile (not . f)

dropTag :: Str -> Str -> Str
dropTag pre =
    BS.drop 1 . BSC.dropWhile (/= '>') . BS.drop (BS.length pre)

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x : xs

onLast :: (a -> a) -> [a] -> [a]
onLast _ [] = []
onLast f [x] = [f x]
onLast f (x:xs) = x : onLast f xs

dropLastN :: Int -> Str -> Str
dropLastN n s = BS.take (BS.length s - n) s

procPage :: [Str] -> Maybe [Str]
procPage ls =
    if null content
      then Nothing
      else Just (title : content)
  where
    title = titleMagic <>
        BSC.takeWhile (/= '<') (dropTag titleLinePrefix $ head ls)
    content =
        onLast (dropLastN 7) .
        takeWhile (not . ("      <sha1" `BS.isPrefixOf`)) .
        onHead (dropTag bodyLinePrefix) $
        dropUntil (bodyLinePrefix `BS.isPrefixOf`) ls

procLines :: [Str] -> [Str]
procLines = concat . catMaybes . map procPage .
    partitions (titleLinePrefix `BS.isPrefixOf`)

main :: IO ()
main = bsInteractL procLines
