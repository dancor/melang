#include <h>

lineToZiPinyins :: String -> [(Char, String)]
lineToZiPinyins l = zip zis pinyins
  where
    zis:pinyin:_ = words l
    pinyins = pronToSyls $ filter (/= '-') $ map toLower pinyin

pronToSyls :: String -> [String]
pronToSyls = doSpl . dropWhile (\c -> c == '#' || c == ':' || isDigit c)
  where
    doSpl "?" = []
    doSpl x = Spl.split (Spl.keepDelimsR $ Spl.whenElt isDigit) x

main = do
    ziPinyins <- concatMap lineToZiPinyins . take 10000 . lines <$>
        readFile "/home/danl/p/l/melang/lang/zh/dict"
    let pinyinsToInfo = sortBy (comparing (\(_,_,l) -> l)) .
            map (\(p, zs) -> (p, zs, length zs)) .
            Map.toList . Map.fromListWith (++) .
            concatMap (\(z, ps) -> [(p, [z]) | p <- Set.toList ps]) .
            Map.toList . Map.fromListWith Set.union $
            map (\(z, p) -> (z, Set.singleton p)) ziPinyins
    mapM_
        (\(p,zs,l) -> putStrLn $ show l ++ " " ++ p ++ "\t" ++ zs)
        pinyinsToInfo
