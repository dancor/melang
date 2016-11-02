#include <h>

import Data.Conduit as C
import Util.DT

-- A list of 8 forms (nom s, nom p, gen s, gen p, dat s, dat p, akk s, akk p)
type NounForm = [DT.Text]

data N = N
    { nS :: !Tetrad
    , nP :: !Tetrad
    , nGloss :: !DT.Text
    } deriving Show

data Tetrad = Tetrad
    { tN :: !DT.Text
    , tG :: !DT.Text
    , tD :: !DT.Text
    , tA :: !DT.Text
    } deriving Show

killDer :: DT.Text -> DT.Text
killDer text = if
    "der " `DT.isPrefixOf` text ||
    "die " `DT.isPrefixOf` text ||
    "das " `DT.isPrefixOf` text ||
    "den " `DT.isPrefixOf` text ||
    "dem " `DT.isPrefixOf` text ||
    "des " `DT.isPrefixOf` text
  then DT.drop 4 text
  else text

pullWds :: DT.Text -> [DT.Text]
pullWds text =
    case DT.dropWhile (not . isWordPart) text of
      "" -> []
      wdRest -> wd : pullWds rest where (wd, rest) = DT.span isWordPart wdRest
  where
    isWordPart c = isAlpha c || c == '-'

{-
pullWds :: DT.Text -> [DT.Text]
pullWds x =
    if DT.null x then [] else wd : pullWds rest
  where
    (wd, rest) = DT.span isAlpha $ DT.dropWhile (not . isAlpha) x

wdsToNounForms :: DT.Text -> [DT.Text] -> [Maybe NounForm]
wdsToNounForms wikt wds =

wdsToNounFormsUnord :: DT.Text -> Set.Set DT.Text -> Map DT.Text NounForm
wdsToNounFormsUnord wikt wds =
    filter (Set.member wds) .
    partitions (== "{{Deutsch Substantiv Übersicht") $
    DT.lines wikt
    -}

nounFormToN :: NounForm -> N
nounFormToN [nS, gS, dS, aS, nP, gP, dP, aP] =
    N (Tetrad nS gS dS aS) (Tetrad nP gP dP aP) ""

showN :: N -> DT.Text
showN (N s p gloss) = DT.intercalate "\t"
    [ nDer
    , tS
    , DT.intercalate "," $
      tSChanges ++ [DT.pack $ concatMap show tPUmlauts ++ tPRec] ++ tPChanges
    , gloss
    ]
  where
    (nDer, tS, tSChanges) = showTetrad s
    (_, tP, tPChanges) = showTetrad p
    (tPRec, (_, tPUmlauts)) = reconcilePlural (DT.unpack tS) (DT.unpack tP)

reconcilePlural :: String -> String -> (String, (Int, [Int]))
reconcilePlural "-" p = (p, (0, []))
reconcilePlural _ "-" = ("-", (0, []))
reconcilePlural [] pRest = ('=':pRest, (0, []))
reconcilePlural sRest [] =
    ("{" ++ sRest ++ "}", (0, []))
    --error $ "Error: Singular exceeds plural:" ++ sRest
reconcilePlural (sL:sRest) (pL:pRest) =
    if sL == pL then (base, (i2, is)) else (base, (i2, i2:is))
  where
    isVowel = pL `elem` "aeiouäöü"
    i2 = if isVowel then i + 1 else i
    (base, (i, is)) = reconcilePlural sRest pRest

showTetrad :: Tetrad -> (DT.Text, DT.Text, [DT.Text])
showTetrad (Tetrad n g d a) = (nDer, prefix, changes)
  where
    nDer = DT.take 3 n
    nK = killDer n
    gK = killDer g
    dK = killDer d
    aK = killDer a
    prefix = DT.pack . map (\(x, _, _, _) -> x) .
        takeWhile (\(nL, gL, dL, aL) -> gL == nL && dL == nL && aL == nL) $
        zip4 (DT.unpack nK) (DT.unpack gK) (DT.unpack dK) (DT.unpack aK)
    changes =
        -- map (\(label, suffix) -> "(" <> label <> ":" <> suffix <> ")") .
        map (\(label, suffix) -> label <> suffix) .
        filter (not . DT.null . snd) $
        map (second (DT.drop (DT.length prefix)))
        [("n", nK), ("g", gK), ("d", dK), ("a", aK)]

nounFormLRepls :: DT.Text -> DT.Text
nounFormLRepls =
    DT.replace "; " " \\ " .
    DT.dropAround (== ' ') .
    -- DT.replace "/" "" .
    DT.replace "&lt;small&gt;" "" .
    DT.replace "&lt;/small&gt;" "" .
    DT.replace "&lt;br&gt;" "; " .
    DT.replace "&lt;br/&gt;" "; " .
    DT.replace "&lt;br &gt;" "; " .
    DT.replace "&lt;br /&gt;" "; " .
    DT.replace "&lt;/br&gt;" "; " .
    DT.replace "&lt;/br/&gt;" "; " .
    DT.replace "(" "" .
    DT.replace ")" ""
    -- &lt;!-- von oben der Tag übernommen --&gt;
    -- &lt;ref&gt;Dativ und Akkusativ  ...

getNounForm :: Monad m => Conduit DT.Text m NounForm
getNounForm = do
    CC.dropWhile (/= "{{Deutsch Substantiv Übersicht")
    CC.drop 1
    l1 <- CC.takeWhile ("|Nominativ Singular" `DT.isPrefixOf`) =$ CL.consume
    l2 <- CC.takeWhile ("|Nominativ Plural" `DT.isPrefixOf`)   =$ CL.consume
    l3 <- CC.takeWhile ("|Genitiv Singular" `DT.isPrefixOf`)   =$ CL.consume
    l4 <- CC.takeWhile ("|Genitiv Plural" `DT.isPrefixOf`)     =$ CL.consume
    l5 <- CC.takeWhile ("|Dativ Singular" `DT.isPrefixOf`)     =$ CL.consume
    l6 <- CC.takeWhile ("|Dativ Plural" `DT.isPrefixOf`)       =$ CL.consume
    l7 <- CC.takeWhile ("|Akkusativ Singular" `DT.isPrefixOf`) =$ CL.consume
    l8 <- CC.takeWhile ("|Akkusativ Plural" `DT.isPrefixOf`)   =$ CL.consume
    C.yield $ map (DT.intercalate ", " . map (nounFormLRepls . DT.drop 1 .
        DT.dropWhile (/= '='))) [l1, l3, l5, l7, l2, l4, l6, l8]
    xMb <- await
    case xMb of
      Just x -> leftover x >> getNounForm
      _ -> return ()

nounFormIsGood :: NounForm -> Bool
nounFormIsGood (nS:_:_:_:nP:_) = if DT.length nS == 1
  then
    "die " `DT.isPrefixOf` nP && DT.length (DT.filter (== ' ') nP) == 1
  else
    ("der " `DT.isPrefixOf` nS ||
    "die " `DT.isPrefixOf` nS ||
    "das " `DT.isPrefixOf` nS) && DT.length (DT.filter (== ' ') nS) == 1
nounFormIsGood _ = False

main :: IO ()
main = do
    goodWds <- Set.fromList . take 1000 .
        filter (DT.all (/= '.')) . map (DT.takeWhile (/= '\t')) . DT.lines <$>
        DTI.readFile "/home/danl/data/goog-ngrams/cur/ger/nouns.txt"
    
    runResourceT $ CC.sourceFile "/home/danl/data/wikt/de/cur" $$
        conduitLines (
            getNounForm =$
            CC.filter (any (any (`Set.member` goodWds) . pullWds)) =$
            CC.filter nounFormIsGood =$
            CC.map (DT.intercalate "; ")
        ) =$ CC.sinkFile "out"

    {- map (showN . procLine) -}
