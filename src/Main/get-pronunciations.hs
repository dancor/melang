{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Cedict
import Cmn.GoogBk
import Cmn.KiloDeck
import Util.DT

main :: IO ()
main = do
    goog <- take 10000 <$> loadGoogBk
    deckPronMap <- Map.fromList .
        map (\x -> (kLWord x, Set.singleton $ kLPinyin x)) <$>
        loadKiloDecks kiloDeckDir
    let _ = rnf deckPronMap
    let pronF = "/home/danl/p/l/melang/data/cmn/pinyin"
    prevPronMap <- Map.fromList . map (second Set.singleton) .
        map (breakOnCh '\t') . DT.lines <$> DTI.readFile pronF
    let _ = rnf prevPronMap
    dictPronMap <- Map.fromListWith Set.union .
        map (\x -> (cSimp x, Set.singleton $ cPron x)) <$> loadCedict
    let _ = rnf dictPronMap
    let pronMap = Map.union deckPronMap $ Map.union prevPronMap dictPronMap
        doGoogWd x = DT.concat [gWd x, "\t", pronStr]
          where
            pronStr = maybe pronByChr showAllPoss $ Map.lookup (gWd x) pronMap
            pronByChr = maybe "?" (DT.intercalate " | " . map showAllPoss) $
                sequence
                [Map.lookup (DT.pack [c]) pronMap | c <- DT.unpack $ gWd x]
            showAllPoss = DT.intercalate " \\ " . Set.toList
    DTI.writeFile pronF . DT.unlines $ map doGoogWd goog
