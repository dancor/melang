import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.List
import qualified Data.Text.IO as DTI
import System.Environment

import Cmn.Dict
import Cmn.TextWds

dropIfEnds :: String -> String -> String
dropIfEnds suff x
  | suff `isSuffixOf` x = take (length x - length suff) x
  | otherwise = x

main :: IO ()
main = do
    dict <- dictToWdDict <$> loadDict
    let maxWdLen = 7
    args <- getArgs
    forM_ args $ \arg -> do
        c <- DTI.readFile arg
        DTI.writeFile (dropIfEnds ".txt" arg ++ ".html") $!!
            textWdsHtml dict maxWdLen $!! c
