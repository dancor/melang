{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmn.Lcmc where

import Control.Applicative
import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTI
import System.FilePath
import Text.XML.Expat.SAX

data Tag
    = TAdjective
    | TAdjectiveAsAdverbial
    | TAdjectiveMorpheme
    | TAdjectiveWithNominalFunction
    | TNonPredicateAdjective
    | TNonPredicateAdjectiveMorpheme
    | TConjunction
    | TConjunctionMorpheme
    | TAdverb
    | TAdverbMorpheme
    | TInterjection
    | TSententialPunctuation
    | TDirectionalLocality
    | TLocalityMorpheme
    | TMorpheme
    | TPrefix
    | TIdiom
    | TAbbreviation
    | TSuffix
    | TFixedExpressions
    | TNumeral
    | TNumericMorpheme
    | TCommonNoun
    | TNounMorpheme
    | TPersonalName
    | TPlaceName
    | TOrganizationName
    | TNominalCharacterString
    | TOtherProperNoun
    | TOnomatopoeia
    | TPreposition
    | TPrepositionMorpheme
    | TClassifier
    | TClassifierMorpheme
    | TPronoun
    | TPronounMorpheme
    | TSpaceWord
    | TTimeWord
    | TTimeWordMorpheme
    | TAuxiliary
    | TVerb
    | TVerbAsAdverbial
    | TVerbMorpheme
    | TVerbWithNominalFunction
    | TSymbolAndNonSententialPunctuation
    | TUnclassifiedItems
    | TModalParticle
    | TModalParticleMorpheme
    | TDescriptive
    | TDescriptiveMorpheme
    deriving (Eq, Ord, Show)

tagAbbrs :: [(Tag, BS.ByteString)]
tagAbbrs =
    [ (TAdjective, "a")
    , (TAdjectiveAsAdverbial, "ad")
    , (TAdjectiveMorpheme, "ag")
    , (TAdjectiveWithNominalFunction, "an")
    , (TNonPredicateAdjective, "b")
    , (TNonPredicateAdjectiveMorpheme, "bg")
    , (TConjunction, "c")
    , (TConjunctionMorpheme, "cg")
    , (TAdverb, "d")
    , (TAdverbMorpheme, "dg")
    , (TInterjection, "e")
    , (TSententialPunctuation, "ew")
    , (TDirectionalLocality, "f")
    , (TLocalityMorpheme, "fg")
    , (TMorpheme, "g")
    , (TPrefix, "h")
    , (TIdiom, "i")
    , (TAbbreviation, "j")
    , (TSuffix, "k")
    , (TFixedExpressions, "l")
    , (TNumeral, "m")
    , (TNumericMorpheme, "mg")
    , (TCommonNoun, "n")
    , (TNounMorpheme, "ng")
    , (TPersonalName, "nr")
    , (TPlaceName, "ns")
    , (TOrganizationName, "nt")
    , (TNominalCharacterString, "nx")
    , (TOtherProperNoun, "nz")
    , (TOnomatopoeia, "o")
    , (TPreposition, "p")
    , (TPrepositionMorpheme, "pg")
    , (TClassifier, "q")
    , (TClassifierMorpheme, "qg")
    , (TPronoun, "r")
    , (TPronounMorpheme, "rg")
    , (TSpaceWord, "s")
    , (TTimeWord, "t")
    , (TTimeWordMorpheme, "tg")
    , (TAuxiliary, "u")
    , (TVerb, "v")
    , (TVerbAsAdverbial, "vd")
    , (TVerbMorpheme, "vg")
    , (TVerbWithNominalFunction, "vn")
    , (TSymbolAndNonSententialPunctuation, "w")
    , (TUnclassifiedItems, "x")
    , (TModalParticle, "y")
    , (TModalParticleMorpheme, "yg")
    , (TDescriptive, "z")
    , (TDescriptiveMorpheme, "zg")
    ]

lcmcDataDir :: FilePath
lcmcDataDir = "/home/danl/p/l/melang/data/cmn/LCMC/2474/Lcmc/data"

data LcmcWord = LcmcWord
    { lwPos    :: !Tag
    , lwWord   :: !DT.Text
    , lwPinyin :: !DT.Text
    } deriving (Eq, Ord, Show)

instance NFData LcmcWord

lookupTag :: BS.ByteString -> Tag
lookupTag !str = fst .
    fromMaybe (error $ "Unknown Tag: " ++ show str) . listToMaybe $
    filter ((== strNorm) . snd) tagAbbrs
  where
    strNorm = BSC.filter (not . isSpace) str

breakCharacterData
    :: [SAXEvent BS.ByteString BS.ByteString]
    -> ([BS.ByteString], [SAXEvent BS.ByteString BS.ByteString])
breakCharacterData = breakCharacterDataAccum []
  where
    breakCharacterDataAccum !accum (CharacterData text : rest) =
        breakCharacterDataAccum (accum ++ [text]) rest
    breakCharacterDataAccum !accum rest = (accum, rest)

data NewSection = NewSentence
    deriving (Eq, Ord, Show)

instance NFData NewSection

procSax
    :: [SAXEvent BS.ByteString BS.ByteString]
    -> [Either NewSection (BS.ByteString, BS.ByteString)]
procSax (StartElement "s" _ : rest) = Left NewSentence : procSax rest
procSax (StartElement "w" [("POS", pos)] : rest) =
    Right (pos, BS.concat texts) : procSax rest'
  where
    (texts, EndElement "w" : rest') = breakCharacterData rest
procSax (StartElement "w" attrs : _rest) =
    error $ "Unexpected attrs:" ++ show attrs
procSax (_ : rest) = procSax rest
procSax [] = []

saxErr
    :: SAXEvent BS.ByteString BS.ByteString
    -> SAXEvent BS.ByteString BS.ByteString
saxErr (FailDocument e) = error $ show e
saxErr x = x

readLcmcCategory :: String -> IO [Either NewSection LcmcWord]
readLcmcCategory cat = do
    let procX = procSax . map saxErr . parse defaultParseOptions
        combineF wordP pinyinP = case (wordP, pinyinP) of
          (Right (pos, word), Right (_, pinyin)) ->
            Right $ LcmcWord (lookupTag pos)
                (DTE.decodeUtf8 word)
                (DTE.decodeUtf8 pinyin)
          (Left s1, Left s2) | s1 == s2 -> Left s1
          _ -> error "Word and pinyin data not aligned."
    wordX <- procX <$> BSL.readFile
        (lcmcDataDir </> "character" </> ("LCMC_" ++ cat ++ ".XML"))
    pinyinX <- procX <$> BSL.readFile
        (lcmcDataDir </> "pinyin" </> ("LCMC_" ++ cat ++ ".xml"))
    return $ zipWith combineF wordX pinyinX

readLcmcCorpus :: IO [Either NewSection LcmcWord]
readLcmcCorpus =
    concat <$> mapM (\x -> readLcmcCategory [x]) "ABCDEFGHJKLMNPR"

showPinyinStats :: Map.Map DT.Text (Map.Map Tag Int) -> [String]
showPinyinStats =
    map ("- " ++) .
    map (\(k, _) -> DT.unpack k) .
    Map.toList

wordStats :: [LcmcWord] -> [String]
wordStats =
    concatMap (\(k, (n, pinyinStats)) -> 
        (show (n :: Int) ++ "\t" ++ DT.unpack k) :
        showPinyinStats pinyinStats
    ) .
    sortBy (flip $ comparing (fst . snd)) .
    Map.toList .
    Map.fromListWith
        (\(n1, m1) (n2, m2) ->
            (n1 + n2, Map.unionWith (Map.unionWith (+)) m1 m2)
        ) .
    map (\lw ->
        ( lwWord lw
        , ( 1
          , Map.singleton (lwPinyin lw) (Map.singleton (lwPos lw) 1)
          )
        )
    )

chunkRights :: [Either a b] -> [[b]]
chunkRights = chunkRightsAccum []
  where
    chunkRightsAccum !accum [] = [accum]
    chunkRightsAccum !accum (Left _ : rest) =
        accum : chunkRightsAccum [] rest
    chunkRightsAccum !accum (Right x : rest) =
        chunkRightsAccum (accum ++ [x]) rest

myMain :: IO ()
myMain = do
    res <- readLcmcCorpus
    res `deepseq` do
{-
        mapM_ DTI.putStrLn .
            concatMap (\x ->
                [ DT.intercalate " " $ map lwWord x
                , DT.intercalate " " $ map lwPinyin x
                ]
            ) .
            tail $ chunkRights res
-}
        mapM_ putStrLn . wordStats $ rights res
