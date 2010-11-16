import Data.List
import Control.Applicative
import qualified Data.Map as M

listToPair [a,b] = (a,b)

highSndOrBKey (a,n) (b,m) = if n > m then (a,n) else (b,m)

main = do
  a <- unlines . map (\ (a,b) -> a ++ " " ++ b) . M.toList . M.map fst . M.fromListWith highSndOrBKey . map ((\ [a,b,c] -> (a,(b,c))) . words) . lines <$> readFile "wubi.txt"
  writeFile "wubi1.txt" a
