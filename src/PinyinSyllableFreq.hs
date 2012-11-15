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

-- Assume each definition is equally likely.  We don't have the needed
-- information to weight them by actual incidence.
defsGetSyllableMap :: [DT.Text] -> M.Map DT.Text Float
defsGetSyllableMap defs =
    M.map ((/ fromIntegral (length defs)) . fromIntegral) .
    M.unionsWith (+) $ map defGetSyllableMap defs

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
            DT.intercalate "\t" [ DT.pack $ show rank, DT.pack $ show perM
                                , wd, pos, DT.intercalate "; " defs
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
            -> M.Map DT.Text ((FreqLine, [DT.Text]), Float)
        freqDefsGetSyllableMap (freq, defs) = 
            M.map (\ num -> ((freq, defs'), num * fNumPerMillion freq)) $
            defsGetSyllableMap defs'
          where
            defs' = case onlyKeepOneDef of
                Nothing -> defs
                Just pinyin ->
                    filter (DT.concat ["[", pinyin, "]"] `DT.isPrefixOf`) defs
            onlyKeepOneDef = case fRank freq of
                1 -> Just "de5"
                4 -> Just "le5"
                _ -> Nothing
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
                [ DT.pack . sigFig 2 $ 100 * count / total, "% "
                , syllable, ":\tWord #", DT.pack . show $ fRank freq, ": "
                , DT.intercalate "; " defs
                ]
    putStrLn "Of the 50k most common Mandarin words in Google Books since 1980 and defined in CEDICT, weighted by word frequency:\n"
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
