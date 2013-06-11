{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTIO
import System.FilePath

import BSUtil
import qualified Cmn.GoogBk1Grams as GB1
import Cmn.Cedict

-- Need to regenerate this (where is the code for that?)
--wiktByLangDir = "/home/danl/p/l/melang/data/wikt/???"

outFile :: FilePath
outFile = "/home/danl/p/l/melang/data/cmn/gbRec/defs.20120701"

{-
data DictLine = DictLine
    { dWd   :: DT.Text
    , dType :: DT.Text
    , dDef  :: DT.Text
    } deriving Show
-}

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

-- sndSeq (a, b) = (,) a <$> b

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
    gb1Ls <- GB1.load
    cedictLs <- zipWith parseCedictLine [1..] .
        filter ((/= '#') . BSC.head) . BSC.lines <$>
        BS.readFile cedictFile
    let wdToDefMap = M.map (map defCleanUp) .
            M.fromListWith (++) $
            map (\ x -> (cSimp x, [cDef x])) cedictLs
        freqAndDef = catMaybes $
            map (\ x -> (,) x <$> M.lookup (GB1.wd x) wdToDefMap) gb1Ls
        makePretty (GB1.Entry rank perM wd pos, defs) =
            DT.intercalate "\t"
            [ DT.pack $ show rank, DT.pack $ show perM, wd, pos
            , DT.intercalate "; " defs
            ]
    DTIO.writeFile outFile . DT.unlines $ map makePretty freqAndDef

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
    freqSet = S.fromList $ map GB1.wd gb1Ls
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
      filter (`S.notMember` tradWds) . map GB1.wd) gb1Ls
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
