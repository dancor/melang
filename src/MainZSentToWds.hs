import qualified Data.Map as Map
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment

import Cmn.Lang
import Cmn.SentenceWords

mainLoop :: Map.Map Wd WdInfo -> IO ()
mainLoop d = do
    l <- DTI.getLine
    doSent d l
    mainLoop d

main :: IO ()
main = do
    args <- getArgs
    d <- loadDict
    case args of
      [] -> mainLoop d
      _ -> doSent d . DT.pack $ concat args
