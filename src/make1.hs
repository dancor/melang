#include <h>

type Wubi = String
type Wd = String
type WubiPoss = [(Wubi, [(Wd, Int)])]

filterIfAny :: (a -> Bool) -> [a] -> [a]
filterIfAny f xs = if any f xs then filter f xs else xs

-- assignments earlier in wubi.txt are preferred but getting new (esp. single) 
-- chars is preferred more
doAssigns :: WubiPoss -> S.Set String -> M.Map String String -> 
  (S.Set String, M.Map String String)
doAssigns [] zAss wToZ = (zAss, wToZ)
doAssigns ((w,poss):ws) zAss wToZ = doAssigns ws zAss' wToZ' where
  ass = fst . head . sortBy (flip $ comparing snd) .
    filterIfAny ((== 1) . length . fst) $ 
    filterIfAny ((`S.notMember` zAss) . fst) poss
  zAss' = S.insert ass zAss
  wToZ' = M.insert w ass wToZ

main :: IO ()
main = do
  home <- getEnv "HOME"
  wubiToZPoss <- map (first head . unzip) . groupBy ((==) `on` fst) . 
    map ((\ [w,c,n] -> (w,(c,read n))) . words) . lines <$> 
    readFile (home </> "l" </> "l" </> "z" </> "wubi.txt")
  let
    zToWubiPoss :: M.Map Wd [Wubi]
    zToWubiPoss = M.fromListWith (++) . map (swap . first (:[])) $ 
      concatMap (uncurry zip . first repeat . second (map fst)) wubiToZPoss
    (zAss, wToZ) = doAssigns wubiToZPoss S.empty M.empty
    z1sUnass = filter (\ z -> length z == 1 && z `S.notMember` zAss && 
        generalCategory (head z) == OtherLetter) $ M.keys zToWubiPoss
    z1Wubis = filter (not . null . snd) $
      map (\ z -> 
        (z, filter ((== 4) . length) . fromJust $ M.lookup z zToWubiPoss)) 
      z1sUnass
    wToZ' = foldr (\ (z, w:_) m -> M.insert (wubiLol w) z m) wToZ z1Wubis
    wubiLol x = take 3 x ++ "z" ++ drop 3 x
  {-
  print $ length z1Wubis
  putStr . unlines . map (\ (x, y) -> x ++ " " ++ show y) $ take 20 z1Wubis
  -}
  writeFile (home </> "l" </> "l" </> "z" </> "wubi1.txt") . unlines .
    map (\ (w,z) -> w ++ " " ++ z) . 
    sortBy (comparing (length . fst) `mappend` compare) $ M.toList wToZ'
