{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
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
    let treatWd [wd, _, spPart, spPartFreq] =
            DT.intercalate "\t" [wd, def, spPart <> " " <> spPartFreq]
          where
            def = case HMS.lookup wd dictMap of
                    Nothing -> "???"
                    Just (FreedictEntry _ dictDef) -> dictDef
    DTI.writeFile "out" $ DT.unlines $ map treatWd topWds
