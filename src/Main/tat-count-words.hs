{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Run as: pv cur/sentences.lang.tsv | ./tat-count-words
-- Run as: pv cur/sentences.tsv | ./tat-count-words

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
type Count = HashMap (P2 L) I
--type Count = I

{-# INLINE test #-}
test = take 1000
--test = id

sentToWords :: T -> [T]
sentToWords = filter (not . T.null) .
    map (T.toLower . T.filter isAlpha) . T.words

main :: IO ()
main = do
    wordCounts <- HM.fromListWith (+) .
        concatMap ((\(_id:_lang:sent:_) -> (sentToWords sent, 1)) . T.split (== '\t')) . 
        test . T.lines <$> T.getContents
    mapM_ (\(w, c) ->
        T.putStrLn $ T.pack (show c) <> " " <> 
        ) $ sortBy (comparing snd) wordCounts
