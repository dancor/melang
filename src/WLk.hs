import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment
import System.FilePath
import System.IO
import qualified Data.Map as M

import PairUtil
import Wubi

main = do
  home <- getEnv "HOME"
  wubiToCh <- map (listToPair . take 2 . words) . lines <$> 
    readFile (home </> "l" </> "l" </> "z" </> "wubi1.txt")
  let
    w2c = M.fromList wubiToCh
    c2w = M.fromListWith (++) $ map (second (:[]) . swap) wubiToCh
  forever $ do
    putStr "> "
    hFlush stdout
    l <- getLine
    let
      r = maybeToList (M.lookup l w2c) ++
        concat (maybeToList $ M.lookup l c2w)
    putStrLn $ unlines $ sortBy (comparing length `mappend` compare) $ 
      map (concatMap (showWK . qToWK)) r
