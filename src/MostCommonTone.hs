#include <h>

procLine :: String -> [Int]
procLine l = 
    map (read . (:[])) $ filter isDigit pinyin
  where
    pinyin = words l !! 1

main = do
    [n] <- map read <$> getArgs
    ls <- take (n :: Int) . lines <$>
        readFile "/home/danl/p/l/melang/out/gbRec/mem"
    let tonesSeen = concatMap procLine ls
        numTones = length tonesSeen
    putStrLn .
        intercalate "\t" . 
        map (\ (k, v) -> show k ++ ": " ++ show v ++ "%") .
        M.toList . 
        --M.map (\ x -> showFFloat (Just 3) x "") . 
        M.map (\ x -> x * 100 `div` numTones) .
        M.fromListWith (+) $ zip tonesSeen (repeat 1)
