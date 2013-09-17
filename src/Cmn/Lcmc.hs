{-# LANGUAGE BangPatterns #-}

module Cmn.Lcmc where

import Control.Applicative
import Control.DeepSeq
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Text.IO as DTI
import System.FilePath
import qualified Text.XML.Light as TXL

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

tagAbbrs :: [(Tag, String)]
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
    , lwWord   :: !String
    , lwPinyin :: !String
    } deriving (Eq, Ord, Show)

instance NFData LcmcWord

type LcmcSentence = [LcmcWord]

type LcmcParagraph = [LcmcSentence]

type LcmcSample = [LcmcParagraph]

type LcmcCategory = [LcmcSample]

type LcmcCorpus = [LcmcCategory]

lookupTag :: String -> Tag
lookupTag !str = fst .
    fromMaybe (error $ "Unknown Tag: " ++ show str) . listToMaybe $
    filter ((== strNorm) . snd) tagAbbrs
  where
    strNorm = filter (not . isSpace) str

procWord :: TXL.Element -> TXL.Element -> LcmcWord
procWord !wordX !pinyinX =
    LcmcWord (lookupTag tStr) (TXL.strContent wordX) (TXL.strContent pinyinX)
  where
    Just tStr = TXL.findAttr (TXL.unqual "POS") wordX

procSentence :: TXL.Element -> TXL.Element -> LcmcSentence
procSentence !wordX !pinyinX = zipWith procWord (f wordX) (f pinyinX)
  where
    f = filter ((== TXL.unqual "w") . TXL.elName) .
        TXL.onlyElems . TXL.elContent

procParagraph :: TXL.Element -> TXL.Element -> LcmcParagraph
procParagraph !wordX !pinyinX = zipWith procSentence (f wordX) (f pinyinX)
  where
    f = filter ((== TXL.unqual "s") . TXL.elName) .
        TXL.onlyElems . TXL.elContent

procSample :: TXL.Element -> TXL.Element -> LcmcSample
procSample !wordX !pinyinX = zipWith procParagraph (f wordX) (f pinyinX)
  where
    f = filter ((== TXL.unqual "p") . TXL.elName) .
        TXL.onlyElems . TXL.elContent

procCategory :: TXL.Element -> TXL.Element -> LcmcCategory
procCategory !wordX !pinyinX = zipWith procSample (f wordX) (f pinyinX)
  where f = TXL.findChildren (TXL.unqual "file")

readLcmcCategory :: String -> IO LcmcCategory
readLcmcCategory !cat = do
    let procX =
            fromMaybe (error $ "No text node in: " ++ cat) . listToMaybe .
            TXL.findChildren (TXL.unqual "text") .
            fromMaybe (error $ "Couldn't parse " ++ cat ++ " XML.") .
            TXL.parseXMLDoc
    wordX <- procX <$> DTI.readFile
        (lcmcDataDir </> "character" </> ("LCMC_" ++ cat ++ ".XML"))
    pinyinX <- procX <$> DTI.readFile
        (lcmcDataDir </> "pinyin" </> ("LCMC_" ++ cat ++ ".xml"))
    return $ procCategory wordX pinyinX

readLcmcCorpus :: IO LcmcCorpus
readLcmcCorpus = mapM (\x -> readLcmcCategory [x]) "ABCDEFGHJKLMNPR"

-- readFlatCorpus :: IO [LcmcWord]
readFlatCorpus = do
    res1 <- flattenCategory <$> readLcmcCategory "A"
    print $ length res1
    res2 <- flattenCategory <$> readLcmcCategory "B"
    print $ length res2
    res3 <- flattenCategory <$> readLcmcCategory "C"
    print $ length res3

flattenParagraph :: LcmcParagraph -> [LcmcWord]
flattenParagraph = concat

flattenSample :: LcmcSample -> [LcmcWord]
flattenSample = concatMap flattenParagraph

flattenCategory :: LcmcCategory -> [LcmcWord]
flattenCategory = concatMap flattenSample

flattenCorpus :: LcmcCorpus -> [LcmcWord]
flattenCorpus = concatMap flattenCategory

showPinyinStats :: Map.Map String (Map.Map Tag Int) -> [String]
showPinyinStats =
    map ("- " ++) .
    map (\(k, _) -> k) .
    Map.toList


wordStats :: [LcmcWord] -> [String]
wordStats =
    concatMap (\(k, (n, pinyinStats)) -> 
        (show (n :: Int) ++ "\t" ++ k) : showPinyinStats pinyinStats
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
