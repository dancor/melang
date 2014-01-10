{-# LANGUAGE OverloadedStrings #-}

module Freedict where

import qualified Data.HashMap.Strict as HMS
import Data.List.Split
import qualified Data.Text as DT

data FreedictEntry
    = FreedictEntry
    { fWord :: DT.Text
    , fDef  :: DT.Text
    }

type Freedict = [FreedictEntry]

type FreedictMap = HMS.HashMap DT.Text FreedictEntry

readFreedict :: DT.Text -> Freedict
readFreedict c =
    map (\(x:xs) ->
        FreedictEntry x (DT.intercalate "; " $ map (DT.drop 3) xs)) parts
  where
    parts = filter (not . null) $ splitWhen DT.null ls
    ls = dropWhile (\l -> DT.null l || " " `DT.isPrefixOf` l ||
        "0" `DT.isPrefixOf` l) $ DT.lines c

makeFreedictMap :: Freedict -> FreedictMap
makeFreedictMap = HMS.fromList . map (\entry -> (fWord entry, entry))
