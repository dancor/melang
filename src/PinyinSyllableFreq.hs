{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

import Cmn.Cedict
import qualified Cmn.GoogBk1Grams as GB1
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
    gb1Ls <- GB1.load
    cedictLs <- zipWith parseCedictLine [1..] .
        filter ((/= '#') . BSC.head) . BSC.lines <$>
        BS.readFile cedictFile
    let wdToDefsMap = M.map (map defCleanUp) .
            M.fromListWith (++) $
            map (\ x -> (cSimp x, [cDef x])) cedictLs
        freqAndDefsList = drop 100 . take 20000 . catMaybes $
            map (\ x -> (,) x <$> M.lookup (GB1.wd x) wdToDefsMap) gb1Ls
        _makePretty (GB1.Entry rank perM wd pos, defs) =
            DT.intercalate "\t" [ DT.pack $ show rank, DT.pack $ show perM
                                , wd, pos, DT.intercalate "; " defs
                                ]
    let syllableToFreqDefCountMap =
            M.unionsWith earlyFreqDefSumCount $
            map freqDefsGetSyllableMap freqAndDefsList
        earlyFreqDefSumCount (freqDef1, n1) (freqDef2, n2) =
            ( if GB1.rank (fst freqDef1) <=
                 GB1.rank (fst freqDef2)
                  then freqDef1
                  else freqDef2
            , n1 + n2
            )
        freqDefsGetSyllableMap
            :: (GB1.Entry, [DT.Text])
            -> M.Map DT.Text ((GB1.Entry, [DT.Text]), Float)
        freqDefsGetSyllableMap (freq, defs) =
            M.map (\ num -> ((freq, defs'), num * GB1.numPerMillion freq)) $
            defsGetSyllableMap defs'
          where
            defs' = case onlyKeepOneDef of
                Nothing -> defs
                Just pinyin ->
                    filter (DT.concat ["[", pinyin, "]"] `DT.isPrefixOf`) defs
            onlyKeepOneDef = case GB1.rank freq of
                1 -> Just "de5"
                4 -> Just "le5"
                5 -> Just "he2"
                -- Doesn't change the results.
                -- 11 -> Just "zhong1"
                103 -> Just "yu3"
                131 -> Just "jian1"
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
                , syllable, ":\tWord #", DT.pack . show $ GB1.rank freq, ": "
                , DT.intercalate "; " defs
                ]
    putStrLn $
        "Of the 50k most common Mandarin words (ignoring the top 100) " ++
        "in Google Books since 1980 and defined in CEDICT, weighted by " ++
        "word frequency:\n"
    putStrLn "Top 10 Syllables:"
    DTI.putStr . DT.unlines .
        zipWith (\ n rest -> DT.concat [DT.pack $ show n, ") ", rest])
        [1 :: Int ..] .
        map (myShow (sum $ map (snd . snd) topSyllables)) $
        take 10 topSyllables
    putStrLn "\nTop 10 Syllables Ignoring Tone:"
    DTI.putStr . DT.unlines .
        zipWith (\ n rest -> DT.concat [DT.pack $ show n, ") ", rest])
        [1 :: Int ..] .
        map (myShow (sum $ map (snd . snd) topFlatSyllables)) $
        take 10 topFlatSyllables
    putStrLn "\nTone Occurrences:"
    DTI.putStr . DT.unlines .
        zipWith (\ n rest -> DT.concat [DT.pack $ show n, ") ", rest])
        [1 :: Int ..] .
        map (myShow (sum $ map (snd . snd) topTones)) $
        topTones
