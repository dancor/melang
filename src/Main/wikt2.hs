{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Directory
import System.FilePath

sectionBy :: (a -> Maybe b) -> [a] -> [(b, [a])]
sectionBy _ [] = []
sectionBy f (x:xs) =
    case f x of
      Just y -> growSection y [] f xs
      _ -> sectionBy f xs

growSection :: b -> [a] -> (a -> Maybe b) -> [a] -> [(b, [a])]
growSection sectionName sectionSoFar _ [] = [(sectionName, sectionSoFar)]
growSection sectionName sectionSoFar f (x:xs) =
    case f x of
      Just y -> (sectionName, sectionSoFar) : growSection y [] f xs
      _ -> growSection sectionName (sectionSoFar ++ [x]) f xs

wiktSections :: DT.Text -> [(DT.Text, [DT.Text])]
wiktSections = sectionBy f . DT.lines
  where
    f x = case DT.unpack x of
       '=':'=':'=':c:_ ->
         if isAlpha c
           then Just . DT.takeWhile (/= '=') $ DT.drop 3 x
           else Nothing
       _ -> Nothing

dirProc :: (DT.Text -> Maybe DT.Text) -> FilePath -> FilePath -> IO ()
dirProc f inDir outDir = do
    createDirectoryIfMissing False outDir
    files <- filter ((/= '.') . head) <$> getDirectoryContents inDir
    forM_ files $ \file -> do
        c <- DTI.readFile $ inDir </> file
        case f c of
          Just res -> DTI.writeFile (outDir </> file) res
          _ -> return ()

-- Note a data inconsistency I found while making this list:
-- This should be applied: pronunciation -> Pronunciation
-- (More generally, could check for lowercase fields.)
goodFields :: [DT.Text]
goodFields =
    [ "Abbreviation"
    , "Adjective"
    , "Adverb"
    , "Affix"
    , "Article"
    , "Conjunction"
    , "Determiner"
    , "Idiom"
    , "Interjection"
    , "Measure word"
    , "Noun"
    , "Number"
    , "Numeral"
    , "Particle"
    , "Phrase"
    , "Postposition"
    , "Prefix"
    , "Preposition"
    , "Pronoun"
    , "Proper noun"
    , "Suffix"
    , "Syllable"
    , "Verb"
    ]

procF :: DT.Text -> Maybe DT.Text
procF c =
    if DT.null res then Nothing else Just res
    --Just $ DT.unlines $ map fst $ wiktSections c
  where
    sections =
        filter ((`elem` goodFields) . fst) $
        wiktSections c
    res = DT.concat $ map (\(section, content) ->
        DT.unlines $ section : map ("- " `DT.append`) content) sections

main :: IO ()
main = do
    let inDir = "/home/danl/data/wikt/10k"
        outDir = "/home/danl/data/wikt/out"
    dirProc procF inDir outDir
