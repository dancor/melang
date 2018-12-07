{-# LANGUAGE OverloadedStrings #-}

module Freedict where

import qualified Data.HashMap.Strict as HM
import Data.List.Split
import qualified Data.Text as T

data FreedictEntry
    = FreedictEntry
    { fWord :: T.Text
    , fDef  :: T.Text
    }

type Freedict = [FreedictEntry]

type FreedictMap = HM.HashMap DT.Text FreedictEntry

readFreedict :: T.Text -> Freedict
readFreedict c =
    map (\(x:xs) ->
        FreedictEntry x (T.intercalate "; " $ map (T.drop 3) xs)) parts
  where
    parts = filter (not . null) $ splitWhen T.null ls
    ls = dropWhile (\l -> DT.null l || " " `T.isPrefixOf` l ||
        "0" `T.isPrefixOf` l) $ T.lines c

makeFreedictMap :: Freedict -> FreedictMap
makeFreedictMap = HM.fromList . map (\entry -> (fWord entry, entry))
