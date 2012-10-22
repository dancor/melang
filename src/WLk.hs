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

io :: IO a -> InputT IO a
io = liftIO

wLk :: Ord k => M.Map k [[Char]] -> M.Map k [[Char]] -> k -> String
wLk w2c c2w l =
  unlines . sortBy (comparing length `mappend` compare) $ map showRes r
  where
  r = fromMaybe [] (M.lookup l w2c) ++
    concat (maybeToList $ M.lookup l c2w)
  showRes x = case sequence $ map qToWK x of
    Just y -> concatMap showWK y
    _ -> x

main :: IO ()
main = runInputT defaultSettings $ do
  home <- io $ getEnv "HOME"
  wubiToCh <- io $ map (listToPair . take 2 . words) . lines <$>
    readFile (home </> "l" </> "l" </> "z" </> "wubi.txt")
  let
    w2c = M.fromListWith (++) $ map (second (:[])) wubiToCh
    c2w = M.fromListWith (++) $ map (swap . first (:[])) wubiToCh
  args <- io getArgs
  case args of
    [] -> forever $ do
      l <- fromMaybe "" <$> getInputLine "> "
      io . putStrLn $ if l == "" then "" else wLk w2c c2w l
    _ -> mapM_ (io . putStrLn . wLk w2c c2w) args
