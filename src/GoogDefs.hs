import Data.Char.Properties.GeneralCategory
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

wikiByLangDir = "/mnt/unenc/wiktionary/by_lang"

outFile = "/home/danl/p/l/melang/out/gbRec/defs"

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
  cDef :: DT.Text
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

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine n s =
  CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) (DTE.decodeUtf8 def)
  where
  (trad, s2) = lol s
  (simp, def) = lol s2
  lol = second (tailOrDie eMsg) . BS.breakByte byteSp
  eMsg = "parse error line " ++ show n

sndSeq (a, b) = (,) a <$> b

killBrackets :: DT.Text -> DT.Text
killBrackets x =
  if DT.null r
    then x
    else before `DT.append` insideKeep `DT.append` killBrackets after
  where
  (before, r) = DT.breakOn (DT.pack "[[") x
  (inside, bbOutside) = DT.breakOn (DT.pack "]]") $ DT.drop 2 r
  (inL, inR) = DT.break (== '|') inside
  insideKeep = if DT.null inR
    then inL
    else DT.drop 1 inR
  after = DT.drop 2 bbOutside

defCleanUp :: DT.Text -> DT.Text
defCleanUp x =
  killBrackets .
  DT.replace (DT.pack ";") (DT.pack ",") $
  if DT.head x == '{'
    then DT.drop 3 . DT.dropWhile (/= '}') $ DT.drop 2 x
    else x

main :: IO ()
main = do
  freqLs <- zipWith parseFreqLine [1..] . BSC.lines <$>
    BS.readFile "/home/danl/p/l/melang/out/gbRec/freq"
  dict0Ls <- zipWith parseCedictLine [1..] .
    filter ((/= '#') . BSC.head) . BSC.lines <$>
    BS.readFile "/home/danl/l/l/z/cedict/dict"
  dict1Ls <-
    filter (not . (DT.pack "{{defn" `DT.isPrefixOf`) . dDef) .
    zipWith parseDictLine [1..] . BSC.lines <$>
    BS.readFile (wikiByLangDir </> "Mandarin")
  dict2Ls <- zipWith parseDictLine [1..] . BSC.lines <$>
    BS.readFile (wikiByLangDir </> "Translingual")
  let
    freqSet = S.fromList $ map fWd freqLs
    tradWds = S.fromList .
      map cTrad $ filter (\ l -> cTrad l /= cSimp l) dict0Ls

    --joinManAndTrans = M.differenceWith ((Just .) . (++))
    joinManAndTrans = M.unionWith (++)
    --joinManAndTrans = M.union

    joinWiktAndCe = M.unionWith (++)

    --dTypeDef d = dType d `DT.append` DT.pack ": " `DT.append` (dDef d)
    dTypeDef d = defCleanUp $ dDef d

    --dTypeDef d = (dType d, dDef d)
    defs =
      (
      M.fromListWith (++) (map (\ d -> (dWd d, [dTypeDef d])) $ dict1Ls)
      `joinManAndTrans`
      M.fromListWith (++) (map (\ d -> (dWd d, [dTypeDef d])) $
        reverse dict2Ls)
      )
      `joinWiktAndCe`
      M.fromListWith (++) (map (\ d -> (cSimp d, [cDef d])) $ reverse dict0Ls)
    res1 = (catMaybes . map (\ x -> sndSeq (x, flip M.lookup defs x)) .
      filter (`S.notMember` tradWds) . map fWd) freqLs
    res2 = M.toList (M.filterWithKey
        (\ k v -> not $
          isAscii (DT.head k) ||
          -- why doesn't this work?
          DT.head k == 'è' ||
          k `S.member` freqSet) defs)
    res = res1 ++ res2
  print $ length res1
  BS.writeFile outFile . BSC.unlines $ map
    (\ (w, d) -> DTE.encodeUtf8 w `BS.append` BSC.cons '\t'
      (DTE.encodeUtf8 $ DT.intercalate (DT.pack "; ") $ nub d))
    res
