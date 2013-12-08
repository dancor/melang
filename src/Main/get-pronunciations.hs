{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import Control.Applicative
import Control.DeepSeq
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as DT

import Cmn.Cedict
import Cmn.Dict

-- Choose lowercase for: a single character with just a lowercase and
-- capitalized version of the same pinyin.
dedupeCaps :: Set.Set DT.Text -> Set.Set DT.Text
dedupeCaps x
  | [xUp, xLow] <- Set.toList x
  , xUp1:xUps <- DT.unpack xUp
  , toLower xUp1 : xUps == DT.unpack xLow =
    Set.singleton xLow
  | otherwise = x

main :: IO ()
main = do
    dict <- loadDict
    pinyinMap <- Map.map dedupeCaps . Map.fromListWith Set.union .
        map (\x -> (cSimp x, Set.singleton $ cPron x)) <$> loadCedict
    let _ = deepseq pinyinMap
    let doEntry entry
          | ePinyin entry == "?"
          , Just pinyins <- Map.lookup (eWord entry) pinyinMap
          , Set.size pinyins == 1
          , pinyin <- DT.intercalate "|" (Set.toList pinyins) =
            entry {ePinyin = pinyin}
          | otherwise = entry
    writeDict $! map doEntry dict
