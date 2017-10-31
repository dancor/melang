import Data.Char
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
import HSH

import Util.Pair
import Wubi

wLk :: Ord k => M.Map k [[Char]] -> M.Map k [[Char]] -> k -> String
wLk w2c c2w l =
    unlines . sortBy (comparing length `mappend` compare) $ map showRes r
  where
    r = fromMaybe [] (M.lookup l w2c) ++ concat (maybeToList $ M.lookup l c2w)
    showRes x = case sequence $ map qToWK x of
      Just y -> concatMap showWK y
      _ -> x

descToQwerty [] = ""
descToQwerty (area:num:rest) = q : descToQwerty rest
  where
    q = case [area, num] of
        "P1" -> 'q'
        "P2" -> 'w'
        "P3" -> 'e'
        "P4" -> 'r'
        "P5" -> 't'
        "N1" -> 'p'
        "N2" -> 'o'
        "N3" -> 'i'
        "N4" -> 'u'
        "N5" -> 'y'
        "H1" -> 'a'
        "H2" -> 's'
        "H3" -> 'd'
        "H4" -> 'f'
        "H5" -> 'g'
        "S1" -> 'h'
        "S2" -> 'j'
        "S3" -> 'k'
        "S4" -> 'l'
        "S5" -> 'm'
        "G1" -> 'n'
        "G2" -> 'b'
        "G3" -> 'v'
        "G4" -> 'c'
        "G5" -> 'x'
        "ZZ" -> 'z'
        a -> error $ "descToQwerty: bad code: " ++ show a

descToQwerty a = error $ "descToQwerty: bad end: " ++ show a

convArg :: String -> String
convArg a
  | any isUpper a =
    descToQwerty a
  | otherwise     = a

main :: IO ()
main = runInputT defaultSettings $ do
    home <- liftIO $ getEnv "HOME"
    wubiToCh <- liftIO $ map (listToPair . take 2 . words) .
      init . drop 1 . dropWhile (/= "[Data]") . lines <$> readFile
      (home </> "p/one-off/fcitx-table-translate/original/wubi2008.txt")
    let w2c = M.fromListWith (++) $ map (second (:[])) wubiToCh
        c2w = M.fromListWith (++) $ map (swap . first (:[])) wubiToCh
        doL l = liftIO $ do
            putStr $ wLk w2c c2w l
            runIO ("/home/danl/bin/zh-char-parts", [l])
            putStrLn ""
    args <- liftIO getArgs
    case args of
      [] -> forever $ do
        l <- fromMaybe "" <$> getInputLine "> "
        unless (null l) $ doL l
      _ -> mapM_ (doL . convArg) args
