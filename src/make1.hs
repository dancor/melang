import Data.List
import Control.Applicative
import qualified Data.Map as M
import System.Environment
import System.FilePath

listToPair [a,b] = (a,b)

highSndOrBKey (a,n) (b,m) = if n > m then (a,n) else (b,m)

main = do
  home <- getEnv "HOME"
  a <- unlines . map (\ (a,b) -> a ++ " " ++ b) . M.toList . M.map fst . 
    M.fromListWith highSndOrBKey . 
    map ((\ [a,b,c] -> (a,(b,read c :: Int))) . words) . 
    lines <$> readFile (home </> "l" </> "l" </> "z" </> "wubi.txt")
  writeFile (home </> "l" </> "l" </> "z" </> "wubi1.txt") a
