-- import Control.Applicative
-- import Control.DeepSeq
-- import Data.Time

import Cmn.Lcmc

main :: IO ()
main = do
    myMain
{-
    putStrLn "Reading corpus words."
    time0 <- getCurrentTime
    wds <- flattenCorpus <$> readLcmcCorpus
    wds `deepseq` do
        time1 <- getCurrentTime
        putStrLn $ "Read corpus words: " ++ show (diffUTCTime time1 time0)
        print $ length wds
        time2 <- getCurrentTime
        putStrLn $ "Counted corpus words: " ++ show (diffUTCTime time2 time1)
-}
    --res <- wordStats . flattenCorpus <$> readLcmcCorpus
    --mapM_ putStrLn res
