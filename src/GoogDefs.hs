{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTIO
import System.FilePath

import BSUtil

-- Need to regenerate this (where is the code for that?)
--wiktByLangDir = "/home/danl/p/l/melang/data/wikt/???"

freqFile = "/home/danl/p/l/melang/data/ngrams/20120701/cmn/out/1980"

cedictFile = "/home/danl/l/l/z/cedict/dict"

outFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

--type MyStr = BS.ByteString
type MyStr = DT.Text

data FreqLine = FreqLine
    { fFreqRank       :: Int
    , fFreqPerMillion :: MyStr
    , fWd             :: MyStr
    , fPartOfSpeech   :: MyStr
    } deriving Show

{-
data DictLine = DictLine
    { dWd   :: MyStr
    , dType :: MyStr
    , dDef  :: MyStr
    } deriving Show
-}

data CedictLine = CedictLine
    { cTrad :: MyStr
    , cSimp :: MyStr
    , cDef  :: MyStr
    } deriving Show

parseFreqLine :: Int -> BS.ByteString -> FreqLine
parseFreqLine n str =
    --FreqLine n a b c
    FreqLine n (DTE.decodeUtf8 a) (DTE.decodeUtf8 b) (DTE.decodeUtf8 c)
  where
    (a, bAndC) = breakTab str
    (b, c) = second BS.tail $ BS.breakByte (fromIntegral $ ord '_') bAndC

{-
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
-}

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine n str =
  --CedictLine trad simp def
  CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) (DTE.decodeUtf8 def)
  where
  (trad, simpAndDef) = doSplit str
  (simp, def) = doSplit simpAndDef
  doSplit = second BS.tail . BS.breakByte (fromIntegral $ ord ' ')

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

{-
defToPinyin :: DT.Text -> DT.Text
defToPinyin def = if DT.null def || DT.head def /= '[' then "" else
    DT.takeWhile (/= ']') $ DT.drop 1 def

defToPinyinMap :: DT.Text -> M.Map DT.Text Int
defToPinyinMap =
    M.fromListWith (+) . map (\ x -> (x, 1)) . DT.words .
    DT.toLower . defToPinyin
-}

main :: IO ()
main = do
    freqLs <- zipWith parseFreqLine [1..] . BSC.lines <$>
        BS.readFile freqFile
    cedictLs <- zipWith parseCedictLine [1..] .
        filter ((/= '#') . BSC.head) . BSC.lines <$>
        BS.readFile cedictFile
    let wdToDefMap = M.map (map defCleanUp) .
            M.fromListWith (++) $
            map (\ x -> (cSimp x, [cDef x])) cedictLs
        freqAndDef = catMaybes $
            map (\ x -> (,) x <$> M.lookup (fWd x) wdToDefMap) freqLs
        makePretty (FreqLine rank perM wd pos, defs) =
            DT.intercalate "\t" [ DT.pack $ show rank, perM, wd, pos
                                , DT.intercalate "; " defs
                                ]
    DTIO.writeFile outFile . DT.unlines $ map makePretty freqAndDef
    {-
    -- Tone Incidences.
    let m1 = M.map (\ x -> (x, 0)) .
            M.unionsWith (+) . concatMap (map defToPinyinMap . snd) $
            take 1000 freqAndDef
        m2 = M.map (\ x -> (0, x)) .
            M.unionsWith (+) . concatMap (map defToPinyinMap . snd) $
            take 10000 freqAndDef
    putStr . unlines . map show . M.toList $
        M.unionWith (\ (a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) m1 m2
    -}
{-
  {-
  dict1Ls <-
    filter (not . (DT.pack "{{defn" `DT.isPrefixOf`) . dDef) .
    zipWith parseDictLine [1..] . BSC.lines <$>
    BS.readFile (wiktByLangDir </> "Mandarin")
  dict2Ls <- zipWith parseDictLine [1..] . BSC.lines <$>
    BS.readFile (wiktByLangDir </> "Translingual")
  -}
    {-
    freqSet = S.fromList $ map fWd freqLs
    tradWdsSet = S.fromList .
      map cTrad $ filter (\ l -> cTrad l /= cSimp l) cedictLs
    -}

    {-
    --joinManAndTrans = M.differenceWith ((Just .) . (++))
    joinManAndTrans = M.unionWith (++)
    --joinManAndTrans = M.union

    joinWiktAndCe = M.unionWith (++)
    -}

    --dTypeDef d = dType d `DT.append` DT.pack ": " `DT.append` (dDef d)
    dTypeDef d = defCleanUp $ dDef d

    --dTypeDef d = (dType d, dDef d)
    defs =
      {-
      (
      M.fromListWith (++) (map (\ d -> (dWd d, [dTypeDef d])) $ dict1Ls)
      `joinManAndTrans`
      M.fromListWith (++) (map (\ d -> (dWd d, [dTypeDef d])) $
        reverse dict2Ls)
      )
      `joinWiktAndCe`
      -}
      M.fromListWith (++) (map (\ d -> (cSimp d, [cDef d])) $ reverse cedictLs)
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
-}
