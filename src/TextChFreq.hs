import System.Environment
import System.FilePath
import Control.Applicative
import Data.Ratio
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow
import Data.List
import Data.Ord
import Data.Char

punc :: [Char]
punc = "\
\＄＆－｛｝（］＊）＋＝！＃～％｀［＠、\
\｛…｜：“；‘，＜。＞／？─”　《》"

main = do
  home <- getEnv "HOME"
  zCounts <- sortBy (flip $ comparing snd) .
    map (second (read . drop 1) .
    break (== '\t') . drop 1 .
    dropWhile (/= '\t')) . lines <$> readFile
    (home </> "l" </> "l" </> "z" </>
    "chars" </> "simp-list")
  let
    zCountMap = M.fromList zCounts
    total = sum $ map snd zCounts
    sums = scanl1 (+) $ map snd zCounts
    zFreqStrs = zipWith3 (\ i (z, n) s -> (z,
      show i ++ "\t" ++
      show (floor $ s * 100 % total) ++ "%"))
      [1..] zCounts sums
  zs <- sortBy (flip $ comparing (fromMaybe 0 .
    flip M.lookup zCountMap . (:[]))) . nub .
    filter (\ c -> c `notElem` punc &&
    ord c > 127) <$> getContents
  putStr . unlines $ map (\ z -> [z] ++ "\t" ++
    fromMaybe "?" (lookup [z] zFreqStrs)) zs
