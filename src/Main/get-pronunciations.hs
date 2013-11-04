{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Cedict
import Cmn.GoogBk
import Cmn.KiloDeck
import Util.DT

type PronMap = Map.Map DT.Text (Set.Set DT.Text)

-- Choose lowercase for: a single character with just a lowercase and
-- capitalized version of the same pinyin.
dedupeCaps :: PronMap -> PronMap
dedupeCaps = Map.map f
  where
    f x
      | [xUp, xLow] <- Set.toList x
      , xUp1:xUps <- DT.unpack xUp
      , toLower xUp1 : xUps == DT.unpack xLow =
        Set.singleton xLow
    f x = x

main :: IO ()
main = do
    goog <- take 10000 <$> loadGoogBk
    deckPronMap <- Map.fromList .
        map (\x -> (kLWord x, Set.singleton $ kLPinyin x)) <$>
        loadKiloDecks kiloDeckDir
    let _ = rnf deckPronMap
    let pronF = "/home/danl/p/l/melang/data/cmn/pinyin"
    prevPronMap <- Map.fromList . map (second Set.singleton) .
        map (breakOnCh '\t') .
        filter (not . ("\\" `DT.isInfixOf`)) . DT.lines <$> DTI.readFile pronF
    let _ = rnf prevPronMap
    dictPronMap <- Map.fromListWith Set.union .
        map (\x -> (cSimp x, Set.singleton $ cPron x)) <$> loadCedict
    let _ = rnf dictPronMap
    let pronMap = dedupeCaps .
            Map.union deckPronMap $ Map.union prevPronMap dictPronMap
        doGoogWd x = DT.concat [gWd x, "\t", pronStr]
          where
            pronStr = maybe pronByChr showAllPoss $ Map.lookup (gWd x) pronMap
            pronByChr = maybe "?" (DT.intercalate " | " . map showAllPoss) $
                sequence
                [Map.lookup (DT.pack [c]) pronMap | c <- DT.unpack $ gWd x]
            showAllPoss = DT.intercalate " \\ " . Set.toList
    DTI.writeFile pronF . DT.unlines $ map doGoogWd goog
