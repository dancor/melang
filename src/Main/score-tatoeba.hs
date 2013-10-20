{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
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
    }

showScore :: SentScoreInfo -> String
showScore (SentScoreInfo a b c) = intercalate "\t" $ map show [a, b, c]

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

sentGetScore :: GInfos -> DT.Text -> SentScoreInfo
sentGetScore gInfos =
    scoreSent . collapseLefts DT.append . findWords gInfos
  where
    scoreSent = foldl' wdScore (SentScoreInfo 0 0 0)
    wdScore !s (Left wd) =
        if DT.all (\c -> isAscii c || isPunctuation c) wd
          then s
          else s {sObsWdCount = sObsWdCount s + 1}
    wdScore !s (Right (_, i))
      | i <= 2000 = s {s2kWdCount = s2kWdCount s + 1}
      | otherwise = s {s10kWdCount = s10kWdCount s + 1}
        -- i `div` 1000

main :: IO ()
main = do
    gLines <- take 10000 <$> loadGoogBk
    sents <- map (DT.dropWhile (/= '\t') . DT.dropWhile (/= '\t')) .
        DT.lines <$> DTI.getContents
    let gMaxWdLen = maximum $ map (DT.length . gWd) gLines
        --gTotOccurs = sum $ map gOccurs gLines
        gWdMap = Map.fromList $ zipWith (\n i -> (gWd i, (i, n))) [1..] gLines
    mapM_ (\(d, s) ->
            DTI.putStrLn $ DT.pack (showScore d ++ "\t") `DT.append` s) $
        -- sort $
        map (\s -> (sentGetScore (gWdMap, gMaxWdLen) s, s)) sents
