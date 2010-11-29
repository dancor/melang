import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment
import System.FilePath
import qualified Data.Map as M

import PairUtil
import Wubi

io = liftIO

main = runInputT defaultSettings $ do
  home <- io $ getEnv "HOME"
  wubiToCh <- io $ map (listToPair . take 2 . words) . lines <$> 
    readFile (home </> "l" </> "l" </> "z" </> "wubi1.txt")
  let
    w2c = M.fromList wubiToCh
    c2w = M.fromListWith (++) $ map (second (:[]) . swap) wubiToCh
  forever $ do
    l <- fromMaybe "" <$> getInputLine "> "
    let
      r = maybeToList (M.lookup l w2c) ++
        concat (maybeToList $ M.lookup l c2w)
    io . putStrLn $ if l == "" then "" else
      unlines . sortBy (comparing length `mappend` compare) $
        map (concatMap (showWK . qToWK)) r
