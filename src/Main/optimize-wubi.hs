module Main where

import Lang.Cmn.Dict

main :: IO ()
main = do
    dictEntries <- loadDict
    let wdToEntry = dictToWdDict dictEntries
    print $ length dictEntries
