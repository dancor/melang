{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.IO as DTL
import qualified Data.Set as Set

import Lang.De.DewiktNouns

main :: IO ()
main = do
    d <- readDewiktLazy
    storyWdsSet <- readStoryWds
    let nounForms = readNouns d
        nounWds =
            [ (killDer nFPart, nF)
            | nF <- nounForms
            , nFPart <- nF
            ]
        storyNouns = nub . sort . map snd $
            filter ((`Set.member` storyWdsSet) . fst) nounWds
    DTL.putStr $ DTL.unlines $ map (DTL.intercalate "; ") storyNouns
