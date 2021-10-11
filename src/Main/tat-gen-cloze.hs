{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Run as: pv cur/sentences.tsv | ./this

import Control.Exception (bracket)
import Control.Monad (liftM2)
import Data.Char (isAlpha)
import Data.Either (partitionEithers)
--import qualified Data.Text as T
import qualified Data.Text.Lazy as T
--import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as T
import Data.Hashable (Hashable, hashWithSalt)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', minimumBy, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hClose, hGetLine, hIsEOF, hPutStrLn, openFile, readFile,
  stderr, stdin, Handle, IOMode(ReadMode))

import Debug.Trace

type I = Int
type L = T -- Lang abbrs are just used directly.
type T = T.Text
data P2 a = P2 {i1 :: !a, i2 :: !a} deriving (Eq, Ord, Show)
instance Hashable a => Hashable (P2 a) where
  hashWithSalt s (P2 a b) = hashWithSalt s (a, b)
type Accum = HashMap (P2 L) [P2 I]

{-# INLINE test #-}
--test = take 10000000
test = id

data Tat = Tat
  { tLang :: !L
  , tSent :: !T
  } deriving Show

untab :: T -> [T]
untab = T.split (== '\t')

procTat :: T -> Maybe (I, Tat)
procTat l = let [id, lang, sent] = untab l in
  if lang == "deu" || lang == "por"
  then Just (read $ T.unpack id, Tat lang sent) else Nothing

tInt :: T -> I
tInt = read . T.unpack

--procSent :: HashMap T I -> (I, Tat) -> a
--procSent wordCounts (i, 

{-
-- FIXME: This always returns Nothing?
-- Prefer to cloze words that are not capitalized in Portuguese.
-- This is a simple way to exclude proper nouns, such as names (eg. John),
-- which tend to be too easy to translate (eg. they may not even change).
-- This also means we try to avoid using the first word in the sentence too
-- (since it is usually, or always?, capitalized), but that's ok.
genCloze :: IntMap Tat -> HashMap T I -> T -> Maybe (I,T,T)
genCloze idToTat wordCounts line = let [id1, id2] = T.split (== '\t') line in
  case (IM.lookup (tInt id1) idToTat, IM.lookup (tInt id2) idToTat) of
    (Just (Tat "por" s1), Just (Tat "deu" s2)) -> let
      procWd wdNum wd1 wd2 = let
        wd1Alpha = T.filter isAlpha wd1; wd1Clean = T.toLower wd1Alpha
        wd2Alpha = T.filter isAlpha wd2; wd2Clean = T.toLower wd1Alpha
        in case (wd1Clean, wd1Clean == wd2Clean, HM.lookup wd1Clean wordCounts) of
        ("", _,     _)             -> trace ("1:" ++ show (s1, s2)) Nothing
        ( _, _,     Nothing)       -> error $ 
          "Word " ++ show wd1Clean ++ " not in ~/data/t/cur/word-count-por.tsv"
        ( _, False, Just wd1Count) -> Just $ if wd1Alpha == wd1Clean
          then Left  (wd1Count, wdNum)
          else Right (wd1Count, wdNum)
        --_ -> Nothing
        _ -> trace ("2:" ++ show (s1, s2)) Nothing
      in case partitionEithers $ catMaybes $
      zipWith3 procWd [0..] (T.words s1) (cycle $ T.words s2) of
      --([],[]) -> Nothing
      ([],[]) -> trace ("3:" ++ show (s1, s2)) Nothing
      ([], l) -> Just (snd $ minimum l, s1, s2)
      ( l, _) -> Just (snd $ minimum l, s1, s2)
    _ -> Nothing
-}

readWordCounts :: IO (HashMap T I)
readWordCounts = do
  home <- getHomeDirectory
  let curTatDir = home </> "data" </> "t" </> "cur"
  HM.fromList . map ((\[n,w]->(w,tInt n)) . untab) . T.lines <$>
    T.readFile (curTatDir </> "word-count-por.tsv")

readSentences :: IO [(I, Tat)]
readSentences = do
  home <- getHomeDirectory
  let curTatDir = home </> "data" </> "t" </> "cur"
  catMaybes . map procTat . T.lines <$> T.readFile (curTatDir </> "sentences.tsv")

main :: IO ()
main = do
  hPutStrLn stderr "Reading wordCounts."
  wordCounts <- readWordCounts
  hPutStrLn stderr "Reading sentences."
  idToTat <- IM.fromList <$> readSentences
  clozes <- catMaybes . map (genCloze idToTat wordCounts) . T.lines <$> T.getContents
  mapM_ (\(n,s1,s2) -> T.putStrLn $ T.pack (show n) <> "\n" <> s1 <> "\n" <> s2) clozes
