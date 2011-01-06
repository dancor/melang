#include <h>

filterIfAny :: (a -> Bool) -> [a] -> [a]
filterIfAny f xs = if any f xs then filter f xs else xs

-- assignments earlier in wubi.txt are preferred but getting new (esp. single) 
-- chars is preferred more
doAssigns :: [(String, [(String, Int)])] -> S.Set String -> 
  M.Map String String -> M.Map String String
doAssigns [] zAss wToZ = wToZ
doAssigns ((w,poss):ws) zAss wToZ = doAssigns ws zAss' wToZ' where
  ass = fst . head . sortBy (flip $ comparing snd) .
    filterIfAny ((== 1) . length . fst) $ 
    filterIfAny ((`S.notMember` zAss) . fst) poss
  zAss' = S.insert ass zAss
  wToZ' = M.insert w ass wToZ

main = do
  home <- getEnv "HOME"
  wubiPoss <- map (first head . unzip) . groupBy ((==) `on` fst) . 
    map ((\ [w,c,n] -> (w,(c,read n))) . words) . lines <$> 
    readFile (home </> "l" </> "l" </> "z" </> "wubi.txt")
  writeFile (home </> "l" </> "l" </> "z" </> "wubi1.txt") . unlines .
    map (\ (a,b) -> a ++ " " ++ b) . 
    sortBy (comparing (length . fst) `mappend` compare) . M.toList $ 
    doAssigns wubiPoss S.empty M.empty
