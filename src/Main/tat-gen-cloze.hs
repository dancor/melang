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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', minimumBy, sort)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
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

data Cloze = Cloze
  { cPos   :: Int
  , cSent  :: T
  , cTrans :: T
  } deriving (Eq, Ord, Show)

untab :: T -> [T]
untab = T.split (== '\t')

procTat :: T -> Maybe (I, Tat)
procTat l = let [id, lang, sent] = untab l in if lang == "deu" || lang == "por"
  then Just (read $ T.unpack id, Tat lang sent) else Nothing

tInt :: T -> I
tInt = read . T.unpack

lineToPorDeu :: IntMap Tat -> T -> Maybe (T,T)
lineToPorDeu !idToTat line = let [id1, id2] = T.split (== '\t') line in
  case (IM.lookup (tInt id1) idToTat, IM.lookup (tInt id2) idToTat) of
  (Just (Tat "por" s1), Just (Tat "deu" s2)) -> Just (s1, s2); _ -> Nothing

-- Consider por words where the clean form isn't "" and doesn't match the
-- clean form of any deu word. Take one that is rarest by total word count
-- from all the Portuguese sentences.
porDeuBestI :: HashMap T I -> (T,T) -> I
porDeuBestI !porWdCounts (!por,!deu) = let
  contenders = sort . map (\(i,porCleanWd) ->
    (fromJust $ HM.lookup porCleanWd porWdCounts, i)) .
    filter (not . (`HS.member` deuCleanWds) . snd) .
    filter (not . T.null . snd) $ zip [0..] (map clean $ T.words por)
  deuCleanWds = HS.fromList . filter (not . T.null) . map clean $ T.words deu
  clean = T.toLower . T.filter isAlpha
  in case contenders of (_,i):_ -> i; _ -> error $ show (por, deu)

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

genMem :: Cloze -> (T,[T])
genMem (Cloze p s t) =
  ( t <> "<br><br>" <> T.replace "|" "\\|"
    (T.replace "\\" "\\\\" . T.unwords $
    take p wds ++ "______" : drop (p + 1) wds)
  , [wds !! p]
  ) where wds = T.words s

main :: IO ()
main = do
  hPutStrLn stderr "Reading sentences."
  idToTat <- IM.fromList <$> readSentences
  ls <- T.lines <$> T.getContents
  let porDeus = catMaybes $ map (lineToPorDeu idToTat) ls
  hPutStrLn stderr $ "There were " ++ show (length porDeus) ++ 
    " sentence pairs from Portuguese to German."
  hPutStrLn stderr "Reading wordCounts."
  porWordCounts <- readWordCounts
  let clozes = map (\(por,deu) ->
        Cloze (porDeuBestI porWordCounts (por,deu)) por deu) porDeus
  mapM_ (\(q,a) -> T.putStrLn $ q <> "|" <> T.intercalate "," a) $ HM.toList $
    HM.fromListWith (++) $ map genMem clozes
