{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

-- | Parsing Mandarin sentences and fragments into individual words.

module Cmn.TextWds where

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as DT

import Cmn.Dict
import Lang
import Util.SciSigFig

textWds :: Lang -> WdDict -> Int -> DT.Text -> [Either DT.Text DictEntry]
textWds Cmn dict maxWdLen = improvePass . textWds2 dict maxWdLen
  where
    -- Look for improvements of the form (A B) C -> A (B C).
    improvePass [] = []
    improvePass (x1 : rest1@(x2 : rest2))
      | Right entry1 <- x1
      , Right entry2 <- x2
      , word1 <- eWord entry1
      , DT.length word1 == 2
      , word2 <- eWord entry2
      , DT.length word2 == 1
      , word1' <- DT.take 1 word1
      , word2' <- DT.drop 1 word1 <> word2
      , Just entry1' <- HMS.lookup word1' dict
      , Just entry2' <- HMS.lookup word2' dict
      , eN entry1' + eN entry2' < eN entry1 + eN entry2 =
        Right entry1' : Right entry2' : improvePass rest2
      | otherwise = x1 : improvePass rest1
    improvePass (x : rest) = x : improvePass rest
textWds _ dict _ = map textWd . DT.words

textWds2 :: WdDict -> Int -> DT.Text -> [Either DT.Text DictEntry]
textWds2 dict maxWdLen text
  | DT.null text = []
  | isPunctuation text1 || isSpace text1 =
    Left (DT.pack [text1]) : textWds dict maxWdLen textRest
  | otherwise = res : textWds dict maxWdLen resRest
  where
    Just (text1, textRest) = DT.uncons text
    (res, resRest) =
        maybe (Left $ DT.singleton text1, textRest) (first Right) .
        listToMaybe . catMaybes $ map tryWdAndRest wdAndRests
    tryWdAndRest :: (DT.Text, DT.Text) -> Maybe (DictEntry, DT.Text)
    tryWdAndRest (wd, rest) =
        (\entry -> (entry, rest)) <$> HMS.lookup wd dict
    wdAndRests =
        map (\n -> (DT.take n text, DT.drop n text)) $ reverse [1 .. maxWdLen]

textWdsHtml :: WdDict -> Int -> DT.Text -> DT.Text
textWdsHtml dict maxWdLen text =
    "<html><head>" <>
    "<meta http-equiv=\"Content-Type\" " <>
    "content=\"text/html; charset=UTF-8\" />" <>
    "<title>Text Words</title></head>" <>
    "<body style=\"background:black;color:white;" <>
    "font-family:AR PL UMing CN;font-size:24px\">" <>
    DT.unlines (map doWd $ textWds dict maxWdLen text) <>
    "</body></html>"
  where
    doWd (Left wd) = DT.replace "\n" "<br>" wd
    doWd (Right entry) =
        "<a href=\"#\" style=\"text-decoration:none;color:" <>
        colorFor (eN entry) <>
        "\" title=\"" <> htmlEsc (showEntry entry) <> "\"><nobr>" <>
        (eWord entry) <> "</nobr></a>"
      where
        colorFor n
          | n <= 1000 = "green"
          | n <= 2000 = "yellow"
          | n <= 10000 = "orange"
          | otherwise = "red"
    -- XXX
    htmlEsc = id
