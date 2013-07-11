{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Data.Aeson
import Data.Char
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Maybe
import qualified Data.Tree as Rose
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
-- import Debug.Trace
import System.IO

import qualified Cmn.GoogBk1Grams as GB1
import Util.BS
import Util.CoverZip
import Util.Json

seqMb :: Maybe (Maybe a) -> Maybe a
seqMb = maybe Nothing id

data DefLine = DefLine
    { _dlZh   :: !DT.Text
    , _dlPy   :: !DT.Text
    , _dlTree :: !(Rose.Tree DefNode)
    }

data DefNode = DefNode
    { dnField              :: !(Maybe (DT.Text))
    , dnSpeechPart         :: !(Maybe (DT.Text))
    , dnDefn               :: !(Maybe (DT.Text))
    , dnUsage              :: !(Maybe (DT.Text))
    , dnConstruction       :: !(Maybe (DT.Text))
    , _dnExampleZh          :: !(Maybe (DT.Text))
    , _dnExampleTranslation :: !(Maybe (DT.Text))
    , dnMeasureWord        :: !(Maybe (DT.Text))
    }

killAts :: DT.Text -> DT.Text
killAts s =
    if "@" `DT.isInfixOf` s
      then DT.pack . sKillAts $ DT.unpack s
      else s

sKillAts :: String -> String
sKillAts [] = []
sKillAts ('@':xs) = sKillAts . drop 1 $ dropWhile (/= '@') xs
sKillAts (x:xs) = x : sKillAts xs

showDefLine :: DefLine -> (DT.Text, DT.Text, DT.Text)
showDefLine (DefLine zh py tr) =
    ( killAts zh
    , tcText . tcConcat $ TCSemi py : (measWdT ++ constrs)
    , tcText $ tcConcat defs)
  where
    (measWds, constrs, defs) = unzip3 $ map showDefNode (Rose.flatten tr)
    measWdT = case catMaybes $ measWds of
      [] -> []
      ms -> [TCSemi $ DT.concat
        ["(M: "
        , DT.intercalate ", " $ map ((DT.cons '-') . pyMakeAscii) ms
        , ")"]]

-- | Text which needs padding from continued text, only if more text is
-- present.
data TextCont
    = TCWord
    { tcText :: !DT.Text
    }
    | TCSemi
    { tcText :: !DT.Text
    }
    | TCNone
    { tcText :: !DT.Text
    }
    | TCSemiNoRep
    { tcText :: !DT.Text
    }

tcAppend :: TextCont -> TextCont -> TextCont
tcAppend tc (TCNone t) = tc {tcText = DT.concat [tcText tc, t]}
tcAppend (TCWord t) tc = tc {tcText = DT.concat [t, " ", tcText tc]}
tcAppend (TCSemi t) tc = tc {tcText = DT.concat [t, "; ", tcText tc]}
tcAppend (TCNone t) tc = tc {tcText = DT.concat [t, tcText tc]}
tcAppend tc@(TCSemiNoRep _) (TCSemiNoRep _) = tc
tcAppend (TCSemiNoRep t) tc = tc {tcText = DT.concat [t, "; ", tcText tc]}

tcConcat :: [TextCont] -> TextCont
tcConcat [] = TCNone ""
tcConcat l = foldl1' tcAppend l

showDefNode :: DefNode -> (Maybe DT.Text, TextCont, TextCont)
showDefNode dn = (measWd, constrPart, tcConcat defParts)
  where
    measWd = dnMeasureWord dn
    constrPart = case dnConstruction dn of
        Nothing -> TCNone ""
        Just s -> TCSemi $ DT.concat
          [ "(CONS: "
          , DT.unwords . map (doHyphen . pyMakeAscii) . DT.words $
            DT.replace "∼" "~" s
          , maybe "" (": " `DT.append`) $ dnDefn dn
          , ")"
          ]
    doHyphen s =
        if isDigit (DT.last s) && not (DT.length s == 2 && s == DT.toUpper s)
          then DT.cons '-' s
          else s
    defParts = catMaybes
        [ (\s -> TCWord $ DT.concat ["<", killAts s, ">"]) <$> dnField dn
        , case dnConstruction dn of
          -- Only show speech part when there is some definition as well.
          -- (That is, kill phantom references.):
          Nothing -> case dnDefn dn of
              -- For now show anyway..
              Nothing -> speechPartMb
              Just s -> Just . tcConcat $ catMaybes
                [ speechPartMb
                , Just . TCSemi . DT.replace ";" "," .
                  DT.replace "“" "\"" .
                  DT.replace "”" "\"" $
                  killAts s
                ]
          Just _ -> Just $ TCSemi "(CONS?)"
        , (\s -> TCSemi $ DT.concat ["(", s, ")"]) <$> dnUsage dn
        , (const $ TCSemiNoRep "(M?)") <$> dnMeasureWord dn
        ]
    speechPartMb =
        case dnSpeechPart dn of
          Nothing -> Nothing
          Just "cons." -> Nothing
          Just s -> Just . TCWord $
            DT.concat [DT.toUpper . killAts $ DT.replace "." "" s, ":"]

nothToTreeNoth :: Maybe (Tree (Maybe a)) -> Tree (Maybe a)
nothToTreeNoth = maybe (Leaf Nothing) id

assembleDefLine :: Int -> HMS.HashMap DT.Text Value -> DefLine
assembleDefLine _n m =
    DefLine
        (sPart "zh")
        (pyMakeAscii $ sPart "key") $
    trMbCoverZipWith8 DefNode
        (tPart "field")
        (tPart "speech_part")
        (tPart "defn")
        (tPart "usage")
        (tPart "construction")
        (tPart "example_zh")
        (tPart "example_translation")
        (tPart "measure_word")
  where
    part :: DT.Text -> Maybe (Tree (Maybe DT.Text))
    part k = valToTree <$> HMS.lookup k m
    sPart = fromJust . fromLeaf . fromJust . part
    tPart = nothToTreeNoth . part

-- | "Cover" because we don't stop when one Tree ends.
trMbCoverZipWith8
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> Maybe g -> Maybe h
       -> i)
    -> Tree (Maybe a) -> Tree (Maybe b) -> Tree (Maybe c) -> Tree (Maybe d)
    -> Tree (Maybe e) -> Tree (Maybe f) -> Tree (Maybe g) -> Tree (Maybe h)
    -> Rose.Tree i
trMbCoverZipWith8 f t1 t2 t3 t4 t5 t6 t7 t8 =
    Rose.Node curLabel kids
  where
    curLabel =
        f
        (asLeaf' t1) (asLeaf' t2) (asLeaf' t3) (asLeaf' t4)
        (asLeaf' t5) (asLeaf' t6) (asLeaf' t7) (asLeaf' t8)
    asLeaf' = seqMb . asLeaf
    kids =
        coverZipWith8 trMbCoverZipWith8Helper
        (asKids' t1) (asKids' t2) (asKids' t3) (asKids' t4)
        (asKids' t5) (asKids' t6) (asKids' t7) (asKids' t8)
    asKids' = map Just . asKids
    trMbCoverZipWith8Helper m1 m2 m3 m4 m5 m6 m7 m8 =
        trMbCoverZipWith8 f
        (nothToTreeNoth m1)
        (nothToTreeNoth m2)
        (nothToTreeNoth m3)
        (nothToTreeNoth m4)
        (nothToTreeNoth m5)
        (nothToTreeNoth m6)
        (nothToTreeNoth m7)
        (nothToTreeNoth m8)

pyMakeAscii :: DT.Text -> DT.Text
pyMakeAscii =
-- pyMakeAscii xx =
    DT.pack . step0 . DT.unpack
    -- Mysteriously present in the data with combining tone marks following:
    . DT.replace "ạ" "a"
    . DT.replace "ẹ" "e"
    . DT.replace "ị" "i"
    . DT.replace "ọ" "o"
    . DT.replace "u\776" "ü"
    . DT.replace "ụ" "u"
    -- Why does "combining minus sign below" ever appear?
    . DT.replace "\800" ""
  where
    step0 [] = []
    -- To Ascii:
    step0 ('“':rest) = '"' : step0 rest
    step0 ('”':rest) = '"' : step0 rest
    -- Keep:
    step0 (' ':rest) = ' ' : step0 rest
    step0 ('-':rest) = '-' : step0 rest
    step0 ('*':rest) = '*' : step0 rest
    step0 ('(':rest) = '(' : step0 rest
    step0 (')':rest) = ')' : step0 rest
    step0 ('\'':rest) = '\'' : step0 rest
    step0 ('/':rest) = '/' : step0 rest
    step0 (',':rest) = ',' : step0 rest
    step0 ('¨':rest) = '¨' : step0 rest

    step0 rest = step1IsNum (0 :: Int) rest

    -- Up to 58 appears!
    step1IsNum n ('¹':rest) = step1IsNum (10 * n + 1) rest
    step1IsNum n ('²':rest) = step1IsNum (10 * n + 2) rest
    step1IsNum n ('³':rest) = step1IsNum (10 * n + 3) rest
    step1IsNum n ('⁴':rest) = step1IsNum (10 * n + 4) rest
    step1IsNum n ('⁵':rest) = step1IsNum (10 * n + 5) rest
    step1IsNum n ('⁶':rest) = step1IsNum (10 * n + 6) rest
    step1IsNum n ('⁷':rest) = step1IsNum (10 * n + 7) rest
    step1IsNum n ('⁸':rest) = step1IsNum (10 * n + 8) rest
    step1IsNum n ('⁹':rest) = step1IsNum (10 * n + 9) rest
    step1IsNum n ('⁰':rest) = step1IsNum (10 * n) rest
    step1IsNum n s =
        if n > 0
          then "[" ++ show n ++ "]" ++ step1IsNum 0 s
          else step2IsTones s
    step2IsTones s =
        if not (null pyVowel) || pyInitial `elem` ["m", "ng", "r"]
          then
            pyInitial ++ pyVowel ++ pyFinal ++ maybe "" show pyTone ++
            step0 sRest
          else
            {-
            -- To make sure the exceptions are legit.  Looked good to me.
            trace
            ("No pinyin syllable: " ++
            show (pyInitial, pyVowel, pyTone, pyFinal) ++ ": " ++ s) $
            -}
            head s : step0 (drop 1 s)
      where
        (pyInitial, s2) = span ((`elem` "bpmfdtnlgkhjqxzhcsryw") . toLower) s
        ((pyVowel, pyTone), s3) = vAndT Nothing s2
        (pyFinal, sRest)
            | "ng" `isPrefixOf` s3 && all (not . isVow) (take 1 $ drop 2 s3)
            = ("ng", drop 2 s3)
            | "n" `isPrefixOf` s3 && all (not . isVow) (take 1 $ drop 1 s3)
            = ("n", drop 1 s3)
            -- For when n with a tone mark is the nucleus:
            | "g" `isPrefixOf` s3 && all (not . isVow) (take 1 $ drop 1 s3)
            = ("g", drop 1 s3)
            -- For er2 etc:
            | "r" `isPrefixOf` s3 && all (not . isVow) (take 1 $ drop 1 s3)
            = ("r", drop 1 s3)
            | otherwise           = ("", s3)
        isVow = (`elem` "aāáǎàeēéěèiīíǐìoōóǒòuūúǔùüǖǘǚǜ")

        doTone :: Maybe Int -> Int -> Maybe Int
        doTone (Just 5) newTone = Just newTone
        doTone (Just oldTone) newTone = error $
            "Two tone marks: " ++ show (oldTone, newTone) ++ ": " ++ show s
        doTone Nothing newTone = Just newTone
        mb5Tone :: Maybe Int -> Maybe Int
        mb5Tone Nothing = Just 5
        mb5Tone oldTone = oldTone

        -- Combining marks:
        vAndT t (x:'\772':r) = first (first (x:)) $ vAndT (doTone t 1) r
        vAndT t (x:'\769':r) = first (first (x:)) $ vAndT (doTone t 2) r
        vAndT t (x:'\780':r) = first (first (x:)) $ vAndT (doTone t 3) r
        vAndT t (x:'\768':r) = first (first (x:)) $ vAndT (doTone t 4) r

        -- Only in lone ng (ng5 handled above, through pyInitial):
        -- n^_ never appears and there may not be a unicode char for it.
        --vAndT t ('':r) = first (first ('n':)) $ vAndT 1 r
        vAndT t ('ń':r) = first (first ('n':)) $ vAndT (doTone t 2) r
        vAndT t ('ň':r) = first (first ('n':)) $ vAndT (doTone t 3) r
        vAndT t ('ǹ':r) = first (first ('n':)) $ vAndT (doTone t 4) r

        vAndT t ('a':r) = first (first ('a':)) $ vAndT (mb5Tone t) r
        vAndT t ('ā':r) = first (first ('a':)) $ vAndT (doTone t 1) r
        vAndT t ('á':r) = first (first ('a':)) $ vAndT (doTone t 2) r
        vAndT t ('ǎ':r) = first (first ('a':)) $ vAndT (doTone t 3) r
        vAndT t ('à':r) = first (first ('a':)) $ vAndT (doTone t 4) r
        vAndT t ('e':r) = first (first ('e':)) $ vAndT (mb5Tone t) r
        vAndT t ('ē':r) = first (first ('e':)) $ vAndT (doTone t 1) r
        vAndT t ('é':r) = first (first ('e':)) $ vAndT (doTone t 2) r
        vAndT t ('ě':r) = first (first ('e':)) $ vAndT (doTone t 3) r
        vAndT t ('è':r) = first (first ('e':)) $ vAndT (doTone t 4) r
        vAndT t ('i':r) = first (first ('i':)) $ vAndT (mb5Tone t) r
        vAndT t ('ī':r) = first (first ('i':)) $ vAndT (doTone t 1) r
        vAndT t ('í':r) = first (first ('i':)) $ vAndT (doTone t 2) r
        vAndT t ('ǐ':r) = first (first ('i':)) $ vAndT (doTone t 3) r
        vAndT t ('ì':r) = first (first ('i':)) $ vAndT (doTone t 4) r
        vAndT t ('o':r) = first (first ('o':)) $ vAndT (mb5Tone t) r
        vAndT t ('ō':r) = first (first ('o':)) $ vAndT (doTone t 1) r
        vAndT t ('ó':r) = first (first ('o':)) $ vAndT (doTone t 2) r
        vAndT t ('ǒ':r) = first (first ('o':)) $ vAndT (doTone t 3) r
        vAndT t ('ò':r) = first (first ('o':)) $ vAndT (doTone t 4) r
        vAndT t ('u':r) = first (first ('u':)) $ vAndT (mb5Tone t) r
        vAndT t ('ū':r) = first (first ('u':)) $ vAndT (doTone t 1) r
        vAndT t ('ú':r) = first (first ('u':)) $ vAndT (doTone t 2) r
        vAndT t ('ǔ':r) = first (first ('u':)) $ vAndT (doTone t 3) r
        vAndT t ('ù':r) = first (first ('u':)) $ vAndT (doTone t 4) r
        vAndT t ('ü':r) = first (first ('v':)) $ vAndT (mb5Tone t) r
        vAndT t ('ǖ':r) = first (first ('v':)) $ vAndT (doTone t 1) r
        vAndT t ('ǘ':r) = first (first ('v':)) $ vAndT (doTone t 2) r
        vAndT t ('ǚ':r) = first (first ('v':)) $ vAndT (doTone t 3) r
        vAndT t ('ǜ':r) = first (first ('v':)) $ vAndT (doTone t 4) r

        -- A, E, and O appear as English only, so shouldn't get a tone.
        -- The other letters appear as pinyin only (I think..).
        vAndT t ('A':r) = first (first ('A':)) $ vAndT t r
        vAndT t ('Ā':r) = first (first ('A':)) $ vAndT (doTone t 1) r
        vAndT t ('Á':r) = first (first ('A':)) $ vAndT (doTone t 2) r
        vAndT t ('Ǎ':r) = first (first ('A':)) $ vAndT (doTone t 3) r
        vAndT t ('À':r) = first (first ('A':)) $ vAndT (doTone t 4) r
        vAndT t ('E':r) = first (first ('E':)) $ vAndT t r
        vAndT t ('Ē':r) = first (first ('E':)) $ vAndT (doTone t 1) r
        vAndT t ('É':r) = first (first ('E':)) $ vAndT (doTone t 2) r
        vAndT t ('Ě':r) = first (first ('E':)) $ vAndT (doTone t 3) r
        vAndT t ('È':r) = first (first ('E':)) $ vAndT (doTone t 4) r
        vAndT t ('I':r) = first (first ('I':)) $ vAndT (mb5Tone t) r
        vAndT t ('Ī':r) = first (first ('I':)) $ vAndT (doTone t 1) r
        vAndT t ('Í':r) = first (first ('I':)) $ vAndT (doTone t 2) r
        vAndT t ('Ǐ':r) = first (first ('I':)) $ vAndT (doTone t 3) r
        vAndT t ('Ì':r) = first (first ('I':)) $ vAndT (doTone t 4) r
        vAndT t ('O':r) = first (first ('O':)) $ vAndT t r
        vAndT t ('Ō':r) = first (first ('O':)) $ vAndT (doTone t 1) r
        vAndT t ('Ó':r) = first (first ('O':)) $ vAndT (doTone t 2) r
        vAndT t ('Ǒ':r) = first (first ('O':)) $ vAndT (doTone t 3) r
        vAndT t ('Ò':r) = first (first ('O':)) $ vAndT (doTone t 4) r
        vAndT t ('U':r) = first (first ('U':)) $ vAndT (mb5Tone t) r
        vAndT t ('Ū':r) = first (first ('U':)) $ vAndT (doTone t 1) r
        vAndT t ('Ú':r) = first (first ('U':)) $ vAndT (doTone t 2) r
        vAndT t ('Ǔ':r) = first (first ('U':)) $ vAndT (doTone t 3) r
        vAndT t ('Ù':r) = first (first ('U':)) $ vAndT (doTone t 4) r
        vAndT t ('Ü':r) = first (first ('V':)) $ vAndT (mb5Tone t) r
        vAndT t ('Ǖ':r) = first (first ('V':)) $ vAndT (doTone t 1) r
        vAndT t ('Ǘ':r) = first (first ('V':)) $ vAndT (doTone t 2) r
        vAndT t ('Ǚ':r) = first (first ('V':)) $ vAndT (doTone t 3) r
        vAndT t ('Ǜ':r) = first (first ('V':)) $ vAndT (doTone t 4) r

        vAndT t r = (("", t), r)

main :: IO ()
main = do
    let keepEarlierLoadingBackward _ x = x
    gb <- HMS.fromListWith keepEarlierLoadingBackward .
        map (\x -> (GB1.wd x, (GB1.numPerMillion x, GB1.partOfSpeech x))) <$>
        GB1.load
    ls <- map parseJson <$> hReadLines stdin
    let wdMap =
            HMS.map (first  (DT.intercalate " \\ ") .
                     second (DT.intercalate " \\ ") . unzip . reverse) .
            HMS.fromListWith (++) .
            map (\(zh, py, def) -> (zh, [(py, def)])) .
            map showDefLine $
            zipWith assembleDefLine [1..] ls
        wdLinesInOrder =
            sortBy (flip compare) .
            map (first (\zh -> (HMS.lookup zh gb, zh))) $
            HMS.toList wdMap
        showZhPyDef ((gbInfo, zh), (py, def)) =
            DT.intercalate "\t"
                [ zh
                -- , freq
                , py
                , partOfSpeech
                , def
                ]
          where
            -- freq = maybe "?" (DT.pack . show . fst) gbInfo
            partOfSpeech = maybe "?" snd gbInfo
    mapM_ DTI.putStrLn $ map showZhPyDef wdLinesInOrder
