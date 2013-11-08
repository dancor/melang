import Data.Char
import Data.Function
import Data.List
import Data.List.Split

main = interact $ unlines . f . lines where
    f = map show .
          sort . map (\x -> (length x, fst $ head x)) .
          groupBy ((==) `on` snd) .
          sortBy (compare `on` snd) .
          map (\x -> (x, filter (not . isDigit) x)) .
          --filter ((== 1) . length . filter isDigit) .
          filter (not . null) . concatMap (splitWhen isDigit) .
          map (killNum . (!! 1) . words)
    killNum x = if ":" `isInfixOf` x
      then drop 1 $ dropWhile (/= ':') x
      else x
