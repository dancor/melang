{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#include <h>
import Data.Char.Properties.GeneralCategory
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Text.Parsec.Prim as TPP
import qualified Text.Parsec.ByteString as P

data GLine = GLine {
  lWd :: DT.Text,
  lYr :: Int,
  lOccurs :: Int
  } deriving Show

(<<) :: (Monad m) => m a -> m b -> m a
a << b = do
  r <- a
  b >> return r

lineParser :: P.Parser GLine
lineParser = liftM3 GLine oneField tabR tabR << tabR << tabR << char '\n'
  where
  --oneField = DTE.decodeUtf8 <$> many1 (noneOf "\t")
  oneField = DT.pack <$> many1 (noneOf "\t")
  tabR = char '\t' >> read <$> (many1 digit)

main :: IO ()
main = do
  ls <- either (error . show) id <$> P.parseFromFile 
    (many1 lineParser << eof) "/mnt/unenc/l/l/z/n-grams/1/20090715.csv"
  let
    topOccurWds = sortBy (flip $ comparing snd `mappend` comparing fst) . 
      M.toList . M.fromListWith (+) . map (\ g -> (lWd g, lOccurs g)) . 
      filter ((\ c -> not (isAscii c) && 
        ClLetter == gcMajorClass (getGeneralCategory c)) . DT.head . lWd) $ 
      filter ((>= 1970) . lYr) ls
  {-
  putStr . unlines $ map (\ (c, n) -> DT.unpack c ++ "\t" ++ show n)
    topOccurWds
  -}
  BS.putStr . DTE.encodeUtf8 . DT.unlines $ 
    map (\ (c, n) -> c `DT.append` DT.pack ('\t' : show n)) topOccurWds
