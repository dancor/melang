import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Environment

import Cmn.SentenceWords2
import Cmn.WdInfo2

mainLoop :: HMS.HashMap Wd WdInfo -> IO ()
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
