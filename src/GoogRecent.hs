#include <h>
import Data.Char.Properties.GeneralCategory
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

data GLine = GLine {
  lWd :: DT.Text,
  lYr :: Int,
  lOccurs :: Int
  } deriving Show

bsReadPosInt :: BS.ByteString -> Int
bsReadPosInt = 
  foldl1 ((+) . (10 *)) . map ((subtract $ ord '0') . fromIntegral) . 
  BS.unpack

parseLine :: Int -> BS.ByteString -> GLine
parseLine n s = GLine (DTE.decodeUtf8 a) (bsReadPosInt b) (bsReadPosInt c) 
  where
  (a, s2) = lol s
  (b, s3) = lol s2
  (c, _) = BS.breakByte 9 s3
  lol = second (tailOrDie eMsg) . BS.breakByte 9
  tailOrDie e a = if BS.null a then error e else BS.tail a
  eMsg = "parse error line " ++ show n

main :: IO ()
main = do
  ls <- zipWith parseLine [1..] . BSC.lines <$> 
    BS.readFile "/mnt/unenc/l/l/z/n-grams/1/20090715.csv"
  let
    topOccurWds = sortBy (flip $ comparing snd `mappend` comparing fst) . 
      M.toList . M.fromListWith (+) . map (\ g -> (lWd g, lOccurs g)) . 
      filter ((\ c -> not (isAscii c) && 
        ClLetter == gcMajorClass (getGeneralCategory c)) . DT.head . lWd) $ 
      filter ((>= 1900) . lYr) ls
  BS.writeFile "/home/danl/p/l/melang/out/gbRec/freq" . BSC.unlines $ map 
    (\ (c, n) -> DTE.encodeUtf8 c `BS.append` BSC.pack ('\t' : show n)) 
    topOccurWds 
