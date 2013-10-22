{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.GoogBk

type WdInfo = (GWdInfo, Int)

type GInfos = (Map.Map DT.Text WdInfo, Int)

data SentScoreInfo = SentScoreInfo
    { sObsWdCount :: Int
    , s10kWdCount :: Int
    , s2kWdCount :: Int
    , s1kWdCount :: Int
    } deriving (Eq, Ord)

showScore :: SentScoreInfo -> String
showScore (SentScoreInfo a b c d) =
    show a ++ "x " ++ show b ++ "h " ++ show c ++ "m " ++ show d ++ "e"

findWords :: GInfos -> DT.Text -> [Either DT.Text WdInfo]
findWords gInfos@(gWdMap, gMaxWdLen) x
  | DT.null x = []
  | otherwise =
    maybe
        (Left x0 : findWords gInfos xRest)
        (\(wdInfo, xAfterWd) ->
            Right wdInfo : findWords gInfos xAfterWd)
        grabWordMb
  where
    grabWordMb :: Maybe (WdInfo, DT.Text)
    grabWordMb =
        listToMaybe $ catMaybes [doTry i | i <- reverse [1 .. maxLenTry]]
    maxLenTry = min gMaxWdLen $ DT.length x
    doTry i =
        case Map.lookup wd gWdMap of
          Nothing -> Nothing
          Just y -> Just (y, xAfterWd)
      where
        (wd, xAfterWd) = DT.splitAt i x
    (x0, xRest) = DT.splitAt 1 x

collapseLefts :: (a -> a -> a) -> [Either a b] -> [Either a b]
collapseLefts _ [] = []
collapseLefts f (Left a : Left b : xs) = Left (f a b) : collapseLefts f xs
collapseLefts f (x : xs) = x : collapseLefts f xs

sentGetScore :: GInfos -> Sent -> SentScoreInfo
sentGetScore gInfos =
    scoreSent . collapseLefts DT.append . findWords gInfos . sSent
  where
    scoreSent = foldl' wdScore (SentScoreInfo 0 0 0 0)
    wdScore !s (Left wd) =
        if DT.all (\c -> isAscii c || isPunctuation c) wd
          then s
          else s {sObsWdCount = sObsWdCount s + 1}
    wdScore !s (Right (_, i))
      | i <= 1000 = s {s1kWdCount = s1kWdCount s + 1}
      | i <= 2000 = s {s2kWdCount = s2kWdCount s + 1}
      | otherwise = s {s10kWdCount = s10kWdCount s + 1}
        -- i `div` 1000

data Sent = Sent
    { sNum :: Int
    , sLang :: DT.Text
    , sSent :: DT.Text
    } deriving (Eq, Ord)

readSent :: DT.Text -> Sent
readSent s = Sent (read $ DT.unpack num) lang sent
  where
    [num, lang, sent] = DT.split (== '\t') s

readLink :: DT.Text -> (Int, Int)
readLink s = (read $ DT.unpack a, read $ DT.unpack b)
  where
    [a, b] = DT.split (== '\t') s

main :: IO ()
main = do
    gLines <- take 100000 <$> loadGoogBk
    cmnSents <- map readSent . DT.lines <$> DTI.readFile "cmn.csv"
    engSents <- map readSent . DT.lines <$> DTI.readFile "eng.csv"
    let cmnNums = Set.fromList $ map sNum cmnSents
        engNums = Set.fromList $ map sNum engSents
    links <- Map.fromListWith (++) . map (\(a, b) -> (a, [b])) .
        filter (\(a, b) -> a `Set.member` cmnNums && b `Set.member` engNums) .
        map readLink . DT.lines <$> DTI.readFile "links.csv"
    let engSentMap = Map.fromList $ map (\s -> (sNum s, s)) engSents
        gMaxWdLen = maximum $ map (DT.length . gWd) gLines
        --gTotOccurs = sum $ map gOccurs gLines
        gWdMap = Map.fromList $ zipWith (\n i -> (gWd i, (i, n))) [1..] gLines
    mapM_ (\((score, s), engs) -> DTI.putStr . DT.unlines $
            DT.pack (showScore score ++ "\t") `DT.append` sSent s : engs
        ) .
        sort .
        filter ((> 0) . length . snd) .
        map (\(score, s) ->
            ( (score, s)
            , map (\eNum -> "- " `DT.append`
              sSent (fromJust $ Map.lookup eNum engSentMap)) .
              fromMaybe [] $ Map.lookup (sNum s) links
            )
        ) .
        filter ((== 0) . sObsWdCount . fst) .
        filter ((> 0) . s1kWdCount . fst) $
        map (\s -> (sentGetScore (gWdMap, gMaxWdLen) s, s)) cmnSents
