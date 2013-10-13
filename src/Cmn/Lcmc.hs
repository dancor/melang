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

tagGroups :: [(String, [Tag])]
tagGroups =
    [ ("ADJ",   [ TAdjective
                , TNonPredicateAdjective
                , TAdjectiveMorpheme
                , TNonPredicateAdjectiveMorpheme
                ])
    , ("ADV",   [ TAdjectiveAsAdverbial
                , TAdverb
                , TAdverbMorpheme
                , TVerbAsAdverbial
                , TSpaceWord
                , TTimeWord
                , TTimeWordMorpheme
                ])
    , ("CONJ",  [ TConjunction
                , TConjunctionMorpheme
                ])
    , ("MEAS",  [ TClassifier
                , TClassifierMorpheme
                ])
    , ("NOUN",  [ TAdjectiveWithNominalFunction
                , TCommonNoun
                , TNounMorpheme
                , TPersonalName
                , TPlaceName
                , TOrganizationName
                , TOtherProperNoun
                , TVerbWithNominalFunction
                , TAbbreviation
                ])
    , ("NUM",   [ TNumeral
                , TNumericMorpheme
                ])
    , ("PREP",  [ TPreposition
                , TPrepositionMorpheme
                ])
    , ("PRON",  [ TPronoun
                , TPronounMorpheme
                ])
    , ("PRT",   [ TAuxiliary
                , TDirectionalLocality
                , TLocalityMorpheme
                , TModalParticle
                , TModalParticleMorpheme
                , TMorpheme
                , TOnomatopoeia
                , TPrefix
                , TSuffix
                , TInterjection
                ])
    , ("VERB",  [ TVerb
                , TVerbMorpheme
                ])
    ]

{-
TFixedExpressions
TIdiom
TSententialPunctuation
TNominalCharacterString
TSymbolAndNonSententialPunctuation
TUnclassifiedItems
TDescriptive
TDescriptiveMorpheme
-}

tagNameMap :: Map.Map Tag String
tagNameMap =
    Map.fromList [(tag, name) | (name, tags) <- tagGroups, tag <- tags]

showTag :: Tag -> String
showTag tag =
    --fromMaybe ("Tag" ++ tail (show tag)) $ Map.lookup tag tagNameMap
    maybe ("Tag" ++ baseName) (\n -> n ++ ":" ++ baseName) $
    Map.lookup tag tagNameMap
  where
    baseName = tail (show tag)

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

makeLwiPos :: Map.Map Tag Int -> [(Int, String)]
makeLwiPos stats
  | Map.null stats = []
  | otherwise =
        map (\(k, v) -> (100 * v `div` totOccur, showTag k)) $
        takeWhile ((> maxOccur) . (* 2) . snd) statsL
      where
        totOccur = sum occurs
        maxOccur = head occurs
        occurs = map snd statsL
        statsL = sortBy (flip $ comparing snd) $ Map.toList stats

data LcmcWdInfo = LcmcWdInfo
    { lwiRank :: Int
    , lwiWd   :: DT.Text
    -- LCMC's pinyin data is incomplete: each word has only one pinyin, not
    -- necessarily even the most common (e.g., 地 is always assigned di4).
    , lwiPy   :: DT.Text
    , lwiPos  :: [(Int, String)]  -- Percentage, Part-of-speech
    }

showLwi :: LcmcWdInfo -> DT.Text
showLwi (LcmcWdInfo r w py poss) = DT.intercalate "\t"
    [ DT.pack (show r), w, py
    , DT.intercalate " \\ " $
      map (\(n, pos) -> DT.pack $ show n ++ "% " ++ pos) poss
    ]

makeLwis :: [LcmcWord] -> [LcmcWdInfo]
makeLwis =
    zipWith (\n (wd, (_, py, stats)) ->
        LcmcWdInfo n wd py (makeLwiPos stats)
    ) [1..] .
    sortBy (flip $ comparing ((\(n, _, _) -> n :: Int) . snd)) .
    Map.toList .
    Map.fromListWith
        (\(n1, py, m1) (n2, _, m2) ->
            (n1 + n2, py, Map.unionWith (+) m1 m2)
        ) .
    map (\lw ->
        ( lwWord lw
        , ( 1
          , lwPinyin lw
          , Map.singleton (lwPos lw) 1
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

loadLwis :: IO [LcmcWdInfo]
loadLwis = makeLwis . rights <$> readLcmcCorpus
