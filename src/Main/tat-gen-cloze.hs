{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Run as: pv cur/sentences.tsv | ./countPairs

import Control.Exception (bracket)
import Control.Monad (liftM2)
import Data.Char (isAlpha)
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
import System.IO (hClose, hGetLine, hIsEOF, openFile, readFile, stdin, Handle, IOMode(ReadMode))

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
  }

untab :: T -> [T]
untab = T.split (== '\t')

procTat :: T -> Maybe (I, Tat)
procTat l = let [id, lang, sent] = untab l in
  if lang == "spa" || lang == "por"
  then Just (read $ T.unpack id, Tat lang sent) else Nothing

tInt :: T -> I
tInt = read . T.unpack

genCloze :: IntMap Tat -> HashMap T I -> T -> Maybe (I,T,T)
genCloze idToTat wordCount l = let [id1, id2] = T.split (== '\t') l in
  case (IM.lookup (tInt id1) idToTat, IM.lookup (tInt id2) idToTat) of
    (Just (Tat "por" s1), Just (Tat "spa" s2)) -> let
      (n,freq_) = minimumBy (comparing snd) . catMaybes .
        map (\(i,mb) -> maybe Nothing (Just.(,)i) mb) . zip [0..] . 
        map (flip HM.lookup wordCount . T.toLower . T.filter isAlpha) $
        T.words s1
      in Just (n,s1,s2)
    _ -> Nothing

main :: IO ()
main = do
  home <- getHomeDirectory
  let curTatDir = home </> "data" </> "t" </> "cur"
  hPutStrLn stderr "Reading wordCount."
  wordCount <- HM.fromList . map ((\[n,w]->(w,tInt n)) . untab) . T.lines <$>
    T.readFile (curTatDir </> "word-count-por.tsv")
  hPutStrLn stderr "Reading sentences."
  idToTat <- IM.fromList . catMaybes . map procTat . T.lines <$>
    T.readFile (curTatDir </> "sentences.tsv")
  clozes <- catMaybes . map (genCloze idToTat wordCount) . T.lines <$> T.getContents
  mapM_ (\(n,s1,s2) -> T.putStrLn $ T.pack (show n) <> "\n" <> s1 <> "\n" <> s2) clozes
