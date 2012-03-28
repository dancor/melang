#include <h>
import Data.Char.Properties.GeneralCategory
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

data FreqLine = FreqLine {
  fWd :: DT.Text,
  fOccurs :: Int
  } deriving Show

data DictLine = DictLine {
  dWd :: DT.Text,
  dType :: DT.Text,
  dDef :: DT.Text
  } deriving Show

data CedictLine = CedictLine {
  cTrad :: DT.Text,
  cSimp :: DT.Text,
  cPron :: DT.Text,
  cDef :: [DT.Text]
  } deriving Show

data MemLine = MemLine {
  mSimp :: DT.Text,
  mPron :: DT.Text,
  mDef :: [DT.Text]
  } deriving Show

bsReadPosInt :: BS.ByteString -> Int
bsReadPosInt = 
  foldl1 ((+) . (10 *)) . map ((subtract $ ord '0') . fromIntegral) . 
  BS.unpack

tailOrDie e a = if BS.null a then error e else BS.tail a

parseFreqLine :: Int -> BS.ByteString -> FreqLine
parseFreqLine n s = 
  FreqLine (DTE.decodeUtf8 a) (bsReadPosInt b)
  where
  (a, s2) = lol s
  (b, _) = BS.breakByte 9 s2
  lol = second (tailOrDie eMsg) . BS.breakByte 9
  eMsg = "parse error line " ++ show n

parseDictLine :: Int -> BS.ByteString -> DictLine
parseDictLine n s = 
  DictLine (DTE.decodeUtf8 wd) (DTE.decodeUtf8 typ) (DTE.decodeUtf8 $ dMod def)
  where
  (wd, s2) = lol s
  (typ, s3) = lol s2
  (def, _) = BS.breakByte 9 s3
  lol = second (tailOrDie eMsg) . BS.breakByte 9
  eMsg = "parse error line " ++ show n
  dMod d = if BSC.pack "# " `BS.isPrefixOf` d then BS.drop 2 d else d

byteSp = fromIntegral $ ord ' '
bytePipe = fromIntegral $ ord '|'

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine n s = 
  CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) 
    (DTE.decodeUtf8 $ BS.filter (/= byteSp) pron) 
    (filter (not . (DT.pack "CL:" `DT.isPrefixOf`)) . 
    map DTE.decodeUtf8 . init . tail $ BSC.split '/' def)
  where
  (trad, s2) = lol s
  (simp, s3) = lol s2
  (pron, def) = lol2 (BS.tail s3)
  lol = second (tailOrDie eMsg) . BS.breakByte byteSp
  lol2 = second (tailOrDie eMsg) . BS.breakByte (fromIntegral $ ord ']')
  eMsg = "parse error line " ++ show n

parseMemLine :: Int -> BS.ByteString -> MemLine
parseMemLine n s = MemLine (DTE.decodeUtf8 simp) (DTE.decodeUtf8 pron) 
    [DTE.decodeUtf8 def]
  where
  (simp, s2) = lol s
  (pron, def) = lol2 (BS.tail s2)
  --lol = second (tailOrDie eMsg) . BS.breakByte bytePipe
  lol = BS.breakByte bytePipe
  lol2 = second (tailOrDie eMsg) . BS.breakByte byteSp
  eMsg = "parse error line " ++ show n

sndSeq (a, b) = (,) a <$> b

bestUnusedDef :: M.Map DT.Text Int -> (DT.Text, [DT.Text]) -> 
  (M.Map DT.Text Int, DT.Text)
bestUnusedDef usedDefs (pron, s) = 
  (M.insertWith (+) dBase 1 usedDefs, DT.append pron $ DT.cons ' ' dShow) where
  bestDef :: [DT.Text] -> Either DT.Text (DT.Text, Int)
  bestDef l@(hl:_) = head $ 
    map Left (filter (`M.notMember` usedDefs) l) ++ 
    [Right (hl, 1 + fromJust (M.lookup hl usedDefs))]
  bestDef [] = Left $ DT.pack "XXX"
  pairOff (a:b:_) = [DT.append a (DT.append (DT.pack "; ") b)]
  pairOff a = a
  --d = bestDef $ sortBy (comparing $ max 5 . DT.length) s
  d = bestDef $ pairOff $ sortBy (comparing $ max 5 . DT.length) $ 
    filter ((< 60) . DT.length) s
  dBase = either id fst d
  dShow = either id 
    (\ (base, n) -> DT.append base (DT.pack $ " (" ++ show n ++ ")")) d

main :: IO ()
main = do
  freqLs <- zipWith parseFreqLine [1..] . BSC.lines <$> 
    BS.readFile "/home/danl/p/l/melang/out/gbRec/freq"
  dict0Ls <- zipWith parseMemLine [1..] . 
    filter ((/= '#') . BSC.head) . BSC.lines <$> 
    BS.readFile "/home/danl/l/l/z/cedict/dict.danl"
  dict1Ls <- zipWith parseCedictLine [1..] . 
    filter ((/= '#') . BSC.head) . BSC.lines <$> 
    BS.readFile "/home/danl/l/l/z/cedict/dict"
  let
    tradWds = S.fromList $
      map cTrad (filter (\ l -> cTrad l /= cSimp l) dict1Ls)
    def1 =
      (M.map (M.fromListWith (++)) (M.fromListWith (++) $ 
        map (\ d -> (mSimp d, [(mPron d, mDef d)])) (reverse dict0Ls)))
    def2 =
      (M.map (M.fromListWith (++)) (M.fromListWith (++) $ 
        map (\ d -> (cSimp d, [(cPron d, cDef d)])) (reverse dict1Ls)))
    defs = M.map M.toList $ M.unionWith M.union
      def1 def2
    freqDefs = catMaybes . map (\ x -> sndSeq (x, flip M.lookup defs x)) . 
      filter (`S.notMember` tradWds) . map fWd $ freqLs
    res = snd $ mapAccumL 
      (\ a (x, ds) -> second ((,) x) $ mapAccumL bestUnusedDef a ds)
      M.empty freqDefs
  {-
  print $ M.lookup (DT.pack "与") defs
  print $ M.lookup (DT.pack "与") def1
  print $ M.lookup (DT.pack "与") def2
  -}
  BS.writeFile "/home/danl/p/l/melang/out/gbRec/z.mem" . BSC.unlines $ 
    BSC.pack "# question set: mandarin glosswords" : map 
    (\ (w, d) -> DTE.encodeUtf8 w `BS.append` BSC.cons '|' 
      (DTE.encodeUtf8 $ DT.intercalate (DT.pack "; ") d))
    res
