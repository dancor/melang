{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Freedict

main :: IO ()
main = do
    dictMap <- makeFreedictMap .
        readFreedict <$> DTI.readFile "data/spa/freedict-spa-eng.dict"
    topWds <- map DT.words . DT.lines <$> DTI.readFile "data/spa/top-wds"
    let defMb wd = listToMaybe $ catMaybes
            [ HMS.lookup wd dictMap
            , HMS.lookup (DT.toLower wd) dictMap
            ]
        def wd = case defMb wd of
          Nothing -> "???"
          Just (FreedictEntry _ dictDef) -> dictDef
        treatWd [wd, _, spPart, spPartFreq] =
            DT.intercalate "\t" [wd, def wd, spPart <> " " <> spPartFreq]
        treatWd _ = error "top-wds format is incorrect."
    DTI.writeFile "out" $ DT.unlines $ map treatWd topWds
