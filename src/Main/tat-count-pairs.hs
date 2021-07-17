{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Run as: pv cur/sentences.tsv | ./countPairs

import Control.Exception (bracket)
import Control.Monad (liftM2)
--import qualified Data.Text as T
import qualified Data.Text.Lazy as T
--import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as T
import Data.Hashable (Hashable, hashWithSalt)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hClose, hGetLine, hIsEOF, openFile, readFile, stdin, Handle, IOMode(ReadMode))

type I = Int
type L = T -- Lang abbrs are just used directly.
type T = T.Text
data P2 a = P2 {i1 :: !a, i2 :: !a} deriving (Eq, Ord, Show)
instance Hashable a => Hashable (P2 a) where
    hashWithSalt s (P2 a b) = hashWithSalt s (a, b)
--type Count = HashMap (P2 L) [P2 I]
--type Count = HashMap (P2 L) I
type Count = I

{-# INLINE test #-}
test = take 1000
--test = id

tInt :: T -> Int
tInt = read . T.unpack

accumCount :: IntMap L -> Count -> P2 I -> Count
accumCount idToLang !c (P2 id1 id2) =
    --HM.insertWith (++) (P2 l1 l2) [P2 id1 id2] c
    --HM.insertWith (+) (P2 l1 l2) 1 c
    c + 1
  where
    l1 = fromMaybe "err" $ IM.lookup id1 idToLang
    l2 = fromMaybe "err" $ IM.lookup id2 idToLang

{-
hReadLs :: Handle -> Int -> (T -> a) -> IO [a]
hReadLs _ 0 _ = return []
hReadLs h i f = do
    e <- hIsEOF h
    if e then return [] else liftM2 (:) (f <$> T.hGetLine h) (hReadLs h (i - 1))

fReadLs f = bracket (openFile f ReadMode) hClose hReadLs
-}

main :: IO ()
main = do
    home <- getHomeDirectory
    let curTatDir = home </> "data" </> "tatoeba" </> "cur"
    idToLang <- IM.fromList .
        map ((\(id:lang:_) -> (tInt id, lang)) . T.split (== '\t')) . 
        test . T.lines <$> T.readFile (curTatDir </> "sentences.tsv")
        -- test <$> fReadLs "cur/sentences.tsv"
        -- test <$> hReadLs stdin
    putStrLn $ "Number of sentences: " ++ show (IM.size idToLang)
    -- c <- foldl' (accumCount idToLang) HM.empty .
    c <- foldl' (accumCount idToLang) 0 .
        map ((\(id1:id2:_) -> P2 (tInt id1) (tInt id2)) . T.split (== '\t')) . 
        -- test <$> fReadLs "cur/links.tsv"
        -- test <$> hReadLs stdin
        test . T.lines <$> T.getContents
    putStrLn $ "Number of links: " ++ show c
    {-
    mapM_ (\(P2 l1 l2, i) ->
        T.putStrLn $ l1 <> " " <> l2 <> " " <> T.pack (show i)
        ) . sortBy (comparing snd) $ HM.toList c
    -}
