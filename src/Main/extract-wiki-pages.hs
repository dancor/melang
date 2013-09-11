{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import ExtractWikiPages
import GB1

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

mandarinSection :: BS.ByteString
mandarinSection = "Mandarin"

translingualSection :: BS.ByteString
translingualSection = "Translingual"

wiktSections :: BS.ByteString -> [(BS.ByteString, [BS.ByteString])]
wiktSections = sectionBy f . BSC.lines
  where
    f x = case BSC.unpack $ BS.take 3 x of
       '=':'=':[c] ->
         if isAlpha c
           then Just . BSC.takeWhile (/= '=') $ BSC.drop 2 x
           else Nothing
       _ -> Nothing

procBody :: BS.ByteString -> Maybe BS.ByteString
procBody body =
    if any (== mandarinSection) $ map fst res
      then Just $ collect res
      else Nothing
  where
    res = filter ((`elem` [translingualSection, mandarinSection]) . fst) $
        wiktSections body
    collect = BSC.unlines . map (BSC.unlines . snd)

seqSnd :: (a, Maybe b) -> Maybe (a, b)
seqSnd (a, Just b) = Just (a, b)
seqSnd _ = Nothing

main :: IO ()
main = do
    dictWds <- take 10000 . map dlWord .
        zipWith readDictline [1..] . DT.lines <$> DTI.readFile
        "/home/danl/p/l/melang/data/cmn/dict"

    xmlStr <- BSL.getContents
    mapM_ (\(title, text) -> BS.writeFile (DT.unpack title) text) .
        -- take 10000 . 
        catMaybes .
        map (seqSnd . second procBody) .
        filter ((`elem` dictWds) . fst) $
        extractPages xmlStr
