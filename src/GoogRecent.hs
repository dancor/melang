import Data.Char.Properties.GeneralCategory
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

data GLine = GLine
    { lWd :: DT.Text
    , lYr :: Int
    , lOccurs :: Int
    } deriving Show

bsReadPosInt :: BS.ByteString -> Int
bsReadPosInt = 
    foldl1 ((+) . (10 *)) . map ((subtract $ ord '0') . fromIntegral) . 
    BS.unpack

parseLine :: Int -> BS.ByteString -> Maybe GLine
parseLine n s =
    case aTextOrErr of
        Right aText -> Just $ GLine aText (bsReadPosInt b) (bsReadPosInt c) 
        Left _err -> Nothing
  where
    aTextOrErr = DTE.decodeUtf8' a
    (a, s2) = lol s
    (b, s3) = lol s2
    (c, _) = BS.breakByte 9 s3
    lol = second (tailOrDie eMsg) . BS.breakByte 9
    tailOrDie e a = if BS.null a then error e else BS.tail a
    eMsg = "parse error line " ++ show n

main :: IO ()
main = do
    [googCsvFilename, outputFilename] <- getArgs
    ls <- catMaybes . zipWith parseLine [1..] . 
        BSC.lines <$> BS.readFile googCsvFilename
    let topOccurWds = sortBy (flip $ comparing snd `mappend` comparing fst) . 
            M.toList . M.fromListWith (+) . map (\ g -> (lWd g, lOccurs g)) . 
            filter ((\ c -> not (isAscii c) && 
            ClLetter == gcMajorClass (getGeneralCategory c)) . DT.head . lWd) $ 
            filter ((>= 1970) . lYr) ls
    BS.writeFile outputFilename . BSC.unlines $ map 
      (\ (c, n) -> DTE.encodeUtf8 c `BS.append` BSC.pack ('\t' : show n)) 
      topOccurWds 
