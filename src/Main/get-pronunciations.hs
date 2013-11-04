{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Cedict
import Cmn.GoogBk
import Cmn.KiloDeck

main :: IO ()
main = do
    goog <- take 10000 <$> loadGoogBk
    deckPronMap <- Map.fromList .
        map (\x -> (kLWord x, Set.singleton $ kLPinyin x)) <$>
        loadKiloDecks kiloDeckDir
    dictPronMap <- Map.fromListWith Set.union .
        map (\x -> (cSimp x, Set.singleton $ cPron x)) <$> loadCedict
    let pronMap = Map.union deckPronMap dictPronMap
        doGoogWd x = DT.concat [gWd x, "\t", pronStr]
          where
            pronStr = maybe pronByChr showAllPoss $ Map.lookup (gWd x) pronMap
            pronByChr = maybe "?" (DT.intercalate " | " . map showAllPoss) $
                sequence
                [Map.lookup (DT.pack [c]) pronMap | c <- DT.unpack $ gWd x]
            showAllPoss = DT.intercalate " \\ " . Set.toList
    DTI.putStr . DT.unlines $ map doGoogWd goog
