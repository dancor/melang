{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Char
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.Random

import Lang.De.Adj
import Lang.De.Base
import Lang.De.Noun
import Str

-- plans:
-- - Allow for random choice of different English possibilities.
-- - More sentence patterns.
-- - More verbs.
-- - Incorporate prepositional phrases.

prepsAcc :: [T]
prepsAcc =
    [ T "für" "for"
    -- bis is weird.
    , T "durch" "through"
    , T "entlang" "along"
    , T "gegen" "against"
    , T "ohne" "without"
    , T "um" "around"
    -- Two-way, only adverbial?:
    -- , T "an" "to"
    -- , T "in" "into"
    ]

prepsDat :: [T]
prepsDat =
    [ T "mit" "with"
    , T "aus" "out-of"
    , T "außer" "except-for"
    , T "bei" "by"
    , T "gegenüber" "opposite"
    , T "nach" "after"
    , T "seit" "since"
    , T "von" "from"
    , T "zu" "to"
    -- Two-way:
    , T "an" "at"
    , T "auf" "on"
    , T "hinter" "behind"
    , T "in" "inside"
    , T "neben" "next-to"
    , T "über" "above"
    , T "unter" "under"
    , T "vor" "in-front-of"
    , T "zwischen" "between"
    ]

persPron :: [T]
persPron =
    [ T "mein"  "my"
    , T "dein"  "your-singular"
    , T "sein"  "his"
    , T "ihr"   "her"
    , T "unser" "our"
    , T "euer"  "yall's"
    , T "ihr"   "their"
    , T "Ihr"   "your-formal"
    {- Have to figure out usage on these.
    , T "ein"   "the"
    , T "kein"  "not-any"
    -}
    ]

pronOf :: Case -> Person -> Gender -> Number -> Formality -> T

-- ich mich mir
-- du  dich dir
-- er  ihn  ihm
-- sie sie  ihr
-- es  es   ihm

-- wir uns  uns
-- ihr euch euch
-- sie sie  ihnen

pronOf Nom P1 _ Sing _ = T "ich" "I"
pronOf Nom P1 _ Plur _ = T "wir" "we"
pronOf Nom P2 _ _ Formal = T "Sie" "you-formal"
pronOf Nom P2 _ Sing _ = T "du" "you-singular"
pronOf Nom P2 _ Plur _ = T "ihr" "you-plural"
pronOf Nom P3 Mas Sing _ = T "er" "he"
pronOf Nom P3 Fem Sing _ = T "sie" "she"
pronOf Nom P3 Neu Sing _ = T "es" "it"
pronOf Nom P3 _ Plur _ = T "sie" "they"

pronOf Acc P1 _ Sing _ = T "mich" "me"
pronOf Acc P1 _ Plur _ = T "uns" "us"
pronOf Acc P2 _ _ Formal = T "Sie" "you-formal"
pronOf Acc P2 _ Sing _ = T "dich" "you-singular"
pronOf Acc P2 _ Plur _ = T "euch" "you-plural"
pronOf Acc P3 Mas Sing _ = T "ihn" "him"
pronOf Acc P3 Fem Sing _ = T "sie" "her"
pronOf Acc P3 Neu Sing _ = T "es" "it"
pronOf Acc P3 _ Plur _ = T "sie" "them"

pronOf Dat P1 _ Sing _ = T "mir" "me"
pronOf Dat P1 _ Plur _ = T "uns" "us"
pronOf Dat P2 _ _ Formal = T "Ihnen" "you-formal"
pronOf Dat P2 _ Sing _ = T "dir" "you-singular"
pronOf Dat P2 _ Plur _ = T "euch" "you-plural"
pronOf Dat P3 Mas Sing _ = T "ihm" "him"
pronOf Dat P3 Fem Sing _ = T "ihr" "her"
pronOf Dat P3 Neu Sing _ = T "ihm" "it"
pronOf Dat P3 _ Plur _ = T "ihnen" "them"

pronOf Gen P1 _ Sing _ = T "meiner" "of-mine"
pronOf Gen P1 _ Plur _ = T "unser" "of-us"
pronOf Gen P2 _ _ Formal = T "Ihrer" "of-you-formal"
pronOf Gen P2 _ Sing _ = T "deiner" "of-you-singular"
pronOf Gen P2 _ Plur _ = T "euer" "of-you-plural"
pronOf Gen P3 Mas Sing _ = T "seiner" "of-him"
pronOf Gen P3 Fem Sing _ = T "ihrer" "of-her"
pronOf Gen P3 Neu Sing _ = T "seiner" "of-it"
pronOf Gen P3 _ Plur _ = T "ihrer" "of-them"

contractions :: Map.Map (Str, Str) Str
contractions = Map.fromList
    -- consonant (but not von)
    [ (("an",     "das"), "ans")
    , (("auf",    "das"), "aufs")
    , (("durch",  "das"), "durchs")
    , (("für",    "das"), "fürs")
    , (("hinter", "das"), "hinters")
    , (("in",     "das"), "ins")
    , (("über",   "das"), "übers")
    , (("um",     "das"), "ums")
    , (("unter",  "das"), "unters")
    , (("vor",    "das"), "vors")

    -- n or vowel
    , (("an",     "dem"), "am")
    , (("bei",    "dem"), "beim")
    , (("in",     "dem"), "im")
    , (("von",    "dem"), "vom")
    , (("zu",     "dem"), "zum")

    , (("zu",     "der"), "zur")
    ]

doContractions :: [Str] -> [Str]
doContractions [] = []
doContractions [w] = [w]
doContractions (w1:w2:rest) = case Map.lookup (w1, w2) contractions of
  Just c -> c : doContractions rest
  _ -> w1 : doContractions (w2:rest)

derOf :: Case -> Gender -> Number -> T
derOf Nom _ Plur = T "die" "the"
derOf Nom Mas _ = T "der" "the"
derOf Nom Fem _ = T "die" "the"
derOf Nom Neu _ = T "das" "the"
derOf Acc _ Plur = T "die" "the"
derOf Acc Mas _ = T "den" "the"
derOf Acc Fem _ = T "die" "the"
derOf Acc Neu _ = T "das" "the"
derOf Dat _ Plur = T "den" "the"
derOf Dat Mas _ = T "dem" "the"
derOf Dat Fem _ = T "der" "the"
derOf Dat Neu _ = T "dem" "the"
derOf Gen _ Plur = T "der" "the"
derOf Gen Mas _ = T "des" "the"
derOf Gen Fem _ = T "der" "the"
derOf Gen Neu _ = T "des" "the"

sel :: [a] -> IO a
sel xs = do
    i <- randomIO
    return $ xs !! (i `mod` length xs)

strOnHead :: (Char -> Char) -> Str -> Str
strOnHead f t = DT.map f t0 <> tRest
  where
    (t0, tRest) = DT.splitAt 1 t

inflPP :: Case -> Gender -> Number -> T -> T
inflPP Nom _ Plur = onTF (<> "e")
inflPP Nom Mas _ = onTF (<> "")
inflPP Nom Fem _ = onTF (<> "e")
inflPP Nom Neu _ = onTF (<> "")
inflPP Acc _ Plur = onTF (<> "e")
inflPP Acc Mas _ = onTF (<> "en")
inflPP Acc Fem _ = onTF (<> "e")
inflPP Acc Neu _ = onTF (<> "")
inflPP Dat _ Plur = onTF (<> "en")
inflPP Dat Mas _ = onTF (<> "em")
inflPP Dat Fem _ = onTF (<> "er")
inflPP Dat Neu _ = onTF (<> "em")
inflPP Gen _ Plur = onTF (<> "er")
inflPP Gen Mas _ = onTF (<> "en")
inflPP Gen Fem _ = onTF (<> "en")
inflPP Gen Neu _ = onTF (<> "er")

-- verbs decline based on person, number, and tense.

{-
data VObjType = VIntrans | VTrans | VIoDo

data VDecl = VReg Str | VIrreg Str [Str]

data V = V
    { vObjs :: [[Case]]
    , vParticiple :: Str
    , vHelper :: V
    , vFDecl :: VDecl
    , vEDecl :: VDecl
    }

sein :: V
sein = V [[Nom]] "gewesen" sein
    (VIrreg "sein" ["bin", "bist", "ist", "sind", "seid", "sind"])
    (VIrreg "be" ["am", "are", "is", "are", "are", "are"])

haben :: V
haben =

verbs =
    [ V [[Nom]]
      {-
    , V Nothing
      (VIrreg "werden"
      ["werde", "wirst", "wird", "werden", "werdet", "werden"])
      (VIrreg "to-be-going-to" 
      -}
    , V [[Acc]]
      (VIrreg "haben" ["habe", "habst", 
    ]
-}

gebenOf :: Person -> Number -> Formality -> T
gebenOf P1 Sing _ = T "gebe"  "give"
gebenOf P1 Plur _ = T "geben" "give"
gebenOf P2 _ Formal = T "geben" "give"
gebenOf P2 Sing _ = T "gibst" "give"
gebenOf P2 Plur _ = T "gebt"  "give"
gebenOf P3 Sing _ = T "gibt"  "gives"
gebenOf P3 Plur _ = T "geben" "give"

doSent :: [Str] -> Str
doSent = (<> ".") . strOnHead toUpper . DT.unwords

genNP :: Int -> Case -> IO ([T], (Person, Number, Formality))
genNP maxParts cas = do
    npType <- sel [1 :: Int, 2]
    case npType of
      1 -> do
        pers <- sel [P1, P2, P3]
        gend <- sel [Mas, Fem, Neu]
        num <- sel [Sing, Plur]
        formality <- sel [Formal, Informal]
        return ([pronOf cas pers gend num formality], (pers, num, formality))
      _ -> do
        noun <- sel nouns
        nounNum <- sel [Sing, Plur]
        adj <- sel adjs
        inflType <- sel [Strong, Weak, Mixed]
        inside <- case inflType of
          Strong -> return [T "etwas" "some"]
          Mixed -> do
              pp <- sel persPron
              return [inflPP  cas (nG noun) nounNum pp]
          Weak -> return [derOf cas (nG noun) nounNum]
        doExtra <- (> 1) <$> sel [1 .. min 2 maxParts]
        extra <- if doExtra
          then do
            prepCas <- sel [Acc, Dat]
            prep <- sel (if prepCas == Acc then prepsAcc else prepsDat)
            (prep :) . fst <$> genNP 1 prepCas
          else return []
        return
            ( inside ++
              [ inflAdj cas (nG noun) nounNum inflType adj
              , inflN cas nounNum noun
              ] ++
              extra
            , (P3, nounNum, Informal)
            )

doQ :: Str -> Str -> IO ()
doQ q a = do
    DTI.putStrLn q
    i <- getLine
    if DT.pack i == a
      then DTI.putStrLn "Correct."
      else do
          DTI.putStrLn "WRONG!"
          DTI.putStrLn a

main :: IO ()
main = do
    (npSubj, (pers, num, formality)) <- genNP 2 Nom
    npIndObj <- fst <$> genNP 2 Dat
    npObj <- fst <$> genNP 2 Acc
    let s = npSubj ++ [gebenOf pers num formality] ++ npIndObj ++ npObj
    doQ (doSent $ map tE s) (doSent . doContractions $ map tF s)
