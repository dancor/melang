{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Map as Map

main :: IO ()
main = do
    ls <- zip [1..] . map (DT.split (== '\t')) . DT.lines <$>
        DTI.readFile "/home/danl/p/l/melang/data/spa/dict-10k.txt"
    let spPartToLs = Map.toList . Map.fromListWith (++) $ map (\(num, l) ->
            (DT.takeWhile (/= ' ') . DT.takeWhile (/= '/') $ l !! 2,
            [DT.pack (show num) : l])) ls
    forM_ spPartToLs $ \(spPart, myLs) -> do
        DTI.writeFile ("out/" ++ DT.unpack spPart) $
            DT.unlines $ map (DT.intercalate "\t") $ reverse myLs
