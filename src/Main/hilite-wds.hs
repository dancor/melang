import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.List
import qualified Data.Text.IO as DTI
import qualified Data.Text as DT
import qualified Data.HashMap.Strict as HMS
import System.Environment

import Lang
import Lang.TextWds

dropIfEnds :: String -> String -> String
dropIfEnds suff x
  | suff `isSuffixOf` x = take (length x - length suff) x
  | otherwise = x

main :: IO ()
main = do
    let maxWdLen = 7
    args <- getArgs
    let (lang, files) = case args of
          "z":rest -> (Cmn, rest)
          "s":rest -> (Spa, rest)
          _ -> error "usage (Spanish or Mandarin): hilite-wds [s|z] <files>"
    let dictFile = case lang of
          Cmn -> "/home/danl/p/l/melang/lang/cmn/dict"
          Spa -> "/home/danl/p/l/melang/lang/spa/dict-100k.txt"
    dict <- HMS.fromList . flip zip [1..] .
        map (DT.toLower . DT.takeWhile (/= '\t')) .
        DT.lines <$> DTI.readFile dictFile
    forM_ files $ \file -> do
        c <- DTI.readFile file
        DTI.writeFile (dropIfEnds ".txt" file ++ ".html") $!!
            textWdsHtml lang dict maxWdLen $!! c
