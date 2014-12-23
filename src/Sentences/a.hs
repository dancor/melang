
#include <h>

decSort :: (a -> b) -> (b -> b -> Ordering) -> [a] -> [a]
decSort f c = map fst . sortBy (c `on` snd) . map (\x -> (x, f x))

main :: IO ()
main = do
    ls <-
        map (\[n, l, s] -> (l, (read $ BSC.unpack n, s))) .
        map (BSC.split '\t') .
        BSC.lines <$> BS.readFile "/home/danl/data/tatoeba/sentences.csv"
    let eNToSent = IntMap.fromList . map snd $ filter ((== "eng") . fst) ls
        fNToSent = IntMap.fromList . map snd $ filter ((== "deu") . fst) ls
    conns <-
        decSort
            (length . BSC.words . fromJust . flip IntMap.lookup fNToSent . fst)
            compare .
        map (\asbs -> (fst $ head asbs, map snd asbs)) .
        groupBy ((==) `on` fst) .
        sort .
        filter (\(a, b) ->
            a `IntMap.member` fNToSent &&
            b `IntMap.member` eNToSent) .
        map (\[a, b] -> (read $ BSC.unpack a, read $ BSC.unpack b)) .
        map (BSC.split '\t') .
        BSC.lines <$> BS.readFile "/home/danl/data/tatoeba/links.csv"
    forM_ conns $ \(a, bs) -> do
        let Just s1 = IntMap.lookup a fNToSent
        BSC.putStrLn s1
        forM_ bs $ \b -> do
            let Just s2 = IntMap.lookup b eNToSent
            BSC.putStrLn $ "- " <> s2
        BSC.putStrLn ""
