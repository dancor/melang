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

import BSUtil
import LangCmn
import SigFig

parseFreqLine :: Int -> BS.ByteString -> FreqLine
parseFreqLine n str =
    FreqLine n (DTE.decodeUtf8 a) (DTE.decodeUtf8 b) (DTE.decodeUtf8 c)
  where
    (a, bAndC) = breakTab str
    (b, c) = second BS.tail $ BS.breakByte (fromIntegral $ ord '_') bAndC

parseCedictLine :: Int -> BS.ByteString -> CedictLine
parseCedictLine _n str =
    CedictLine (DTE.decodeUtf8 trad) (DTE.decodeUtf8 simp) (DTE.decodeUtf8 def)
  where
    (trad, simpAndDef) = doSplit str
    (simp, def) = doSplit simpAndDef
    doSplit = second BS.tail . BS.breakByte (fromIntegral $ ord ' ')

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

defGetPinyin :: DT.Text -> DT.Text
defGetPinyin def = if DT.null def || DT.head def /= '[' then "" else
    DT.takeWhile (/= ']') $ DT.drop 1 def

defGetSyllableMap :: DT.Text -> M.Map DT.Text Int
defGetSyllableMap =
    M.fromListWith (+) . map (\ x -> (x, 1)) . DT.words .
    DT.toLower . defGetPinyin

defsGetSyllableMap :: [DT.Text] -> M.Map DT.Text Int
defsGetSyllableMap = M.unionsWith (+) . map defGetSyllableMap

main :: IO ()
main = do
    freqLs <- zipWith parseFreqLine [1..] . BSC.lines <$>
        BS.readFile freqFile
    cedictLs <- zipWith parseCedictLine [1..] .
        filter ((/= '#') . BSC.head) . BSC.lines <$>
        BS.readFile cedictFile
    let wdToDefsMap = M.map (map defCleanUp) .
            M.fromListWith (++) $
            map (\ x -> (cSimp x, [cDef x])) cedictLs
        freqAndDefsList = take 50000 . catMaybes $
            map (\ x -> (,) x <$> M.lookup (fWd x) wdToDefsMap) freqLs
        _makePretty (FreqLine rank perM wd pos, defs) =
            DT.intercalate "\t" [ DT.pack $ show rank, perM, wd, pos
                                , DT.intercalate "; " defs
                                ]
    let syllableToFreqDefCountMap = 
            M.unionsWith earlyFreqDefSumCount $
            map freqDefsGetSyllableMap freqAndDefsList
        earlyFreqDefSumCount (freqDef1, n1) (freqDef2, n2) =
            ( if fRank (fst freqDef1) <= 
                 fRank (fst freqDef2)
                  then freqDef1
                  else freqDef2
            , n1 + n2
            )
        freqDefsGetSyllableMap
            :: (FreqLine, [DT.Text])
            -> M.Map DT.Text ((FreqLine, [DT.Text]), Int)
        freqDefsGetSyllableMap (freq, defs) = 
            M.map ((,) (freq, defs)) $ defsGetSyllableMap defs
    let flatSyllableToFreqDefCountMap = 
            M.mapKeysWith earlyFreqDefSumCount (DT.takeWhile (not . isDigit))
            syllableToFreqDefCountMap
        toneToFreqDefCountMap = 
            M.mapKeysWith earlyFreqDefSumCount (DT.dropWhile (not . isDigit))
            syllableToFreqDefCountMap
    let topSyllables =
            reverse . sortBy (comparing (snd . snd)) $
            M.toList syllableToFreqDefCountMap
        topFlatSyllables =
            reverse . sortBy (comparing (snd . snd)) $
            M.toList flatSyllableToFreqDefCountMap
        topTones = 
            reverse . sortBy (comparing (snd . snd)) $
            M.toList toneToFreqDefCountMap
    let myShow total (syllable, ((freq, defs), count)) = 
            DT.concat $
                [ DT.pack . sigFig 2 $
                  100 * fromIntegral count / fromIntegral total, "% "
                , syllable, ":\tWord #", DT.pack . show $ fRank freq, ": "
                , DT.intercalate "; " defs
                ]
    putStrLn "Of the 50k most common Mandarin words in Google Books since 1980 and defined in CEDICT:\n"
    putStrLn "Top 10 Syllables:"
    DTIO.putStr . DT.unlines . 
        zipWith (\ n rest -> DT.concat [DT.pack $ show n, ") ", rest]) [1..] .
        map (myShow (sum $ map (snd . snd) topSyllables)) $
        take 10 topSyllables
    putStrLn "\nTop 10 Syllables Ignoring Tone:"
    DTIO.putStr . DT.unlines . 
        zipWith (\ n rest -> DT.concat [DT.pack $ show n, ") ", rest]) [1..] .
        map (myShow (sum $ map (snd . snd) topFlatSyllables)) $
        take 10 topFlatSyllables
    putStrLn "\nTone Occurrences:"
    DTIO.putStr . DT.unlines . 
        zipWith (\ n rest -> DT.concat [DT.pack $ show n, ") ", rest]) [1..] .
        map (myShow (sum $ map (snd . snd) topTones)) $
        topTones
