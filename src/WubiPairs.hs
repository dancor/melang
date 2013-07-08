import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment
import System.FilePath
import qualified System.IO.UTF8 as U
import qualified Data.Map as M

import Util.Pair
import Wubi

shortOrd :: (Ord a) => [a] -> [a] -> Ordering
shortOrd = comparing length `mappend` compare

main = do
  home <- getEnv "HOME"
  wubiToCh <- map (first (map qToWK) .
    listToPair . take 2 . words) . lines <$>
    readFile (home </> "l" </> "l" </> "z" </> "wubi1.txt")
  let
    wubiToCh2 = filter ((<= 2) . length . fst)
      wubiToCh
    cToW :: M.Map String [Wubi]
    cToW = M.fromListWith (++) $
      map (second (:[]) . swap) wubiToCh2
    cToMinW :: M.Map String Wubi
    cToMinW = M.map (minimumBy shortOrd) cToW
    minWToC :: M.Map Wubi String
    minWToC = M.fromList . map swap $
      M.toList cToMinW
    allWK =
      [WK s n | s <- [P .. G], n <- [1 .. 5]]
    wubiLk = fromMaybe "。" . flip M.lookup minWToC
  --putStr . unlines . map (uncurry (++)) . map (first (concatMap dStr)) . sortBy (shortOrd `on` fst) . map (first (map d) . swap) $ M.toList c2w
  putStr . unlines $ [
    "  " ++ concatMap showWK allWK,
    "  " ++ concatMap
      (wubiLk . (:[])) allWK] ++
    map (\ x -> showWK x ++ concatMap (wubiLk . (x:) . (:[])) allWK) allWK

