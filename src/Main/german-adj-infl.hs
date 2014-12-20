#include <h>

-- plans:
-- - Allow for random choice of different English possibilities.
-- - More sentence patterns.
-- - More verbs.
-- - Incorporate prepositional phrases.

data Case = Nom | Acc | Dat | Gen deriving Eq

data Person = P1 | P2 | P3 deriving Eq

data Gender = Mas | Fem | Neu deriving Eq

data Number = Sing | Plur deriving Eq

data Formality = Formal | Informal deriving Eq

data InflType = Strong | Weak | Mixed deriving Eq

data Mood = Indicative | Imperative | Reported | Conditional deriving Eq

data Tense = Present | Past deriving Eq

-- data Voice = Active | Passive deriving Eq

-- NB: German doesn't grammatically encode any Aspect information.

-- type Str = BS.ByteString
type Str = DT.Text

data T = T -- translation term
    { tF :: Str -- foreign
    , tE :: Str -- English
    }

data N = N -- noun
    { nG :: Gender
    , nF :: Str
    , nFPlur :: Str
    , nFDatPlur :: Str
    , nE :: Str
    , nEPlur :: Str
    }

data Adj = AdjInfl T | AdjNoInfl T
    | AdjIrreg
    { adjIrregF :: Str
    , adjIrregFStem :: Str
    , adjIrregFEr :: Str
    , adjIrregFEst :: Str
    , adjIrregE :: Str
    }

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

adjsNoInfl =
    [ T "rosa" "pink"
    ]

persPron =
    [ T "mein"  "my"
    , T "dein"  "your-singular"
    , T "sein"  "his"
    , T "ihr"   "her"
    , T "unser" "our"
    , T "euer"  "yall's"
    , T "ihr"   "their"
    , T "ihr"   "your-formal"
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

adjs =
    [ AdjNoInfl $ T "rosa" "pink"
    , AdjIrreg "orange" "orang" "oranger" "am orangest" "orange"
    , AdjIrreg "hoch" "hoch" "höher" "am höchsten" "high" 
    , AdjInfl $ T "neu"   "new"
    , AdjInfl $ T "alt"   "old"
    , AdjInfl $ T "groß"  "big"
    , AdjInfl $ T "klein" "small"
    , AdjInfl $ T "schön" "pretty"
    , AdjInfl $ T "hässlich"  "ugly"
    , AdjInfl $ T "weiß"      "white"
    , AdjInfl $ T "schwartz"  "black"
    , AdjInfl $ T "rot"       "red"
    , AdjInfl $ T "grün"      "green"
    , AdjInfl $ T "blau"      "blue"
    , AdjInfl $ T "gelb"      "yellow"
    , AdjInfl $ T "grau"      "gray"
    , AdjInfl $ T "violett"   "violet"
    , AdjInfl $ T "azur"      "azure"
    , AdjInfl $ T "golden"    "gold"
    , AdjInfl $ T "silber"    "silver"
    , AdjInfl $ T "glücklich" "happy"
    , AdjInfl $ T "traurig"   "sad"
    , AdjInfl $ T "normal"    "normal"
    , AdjInfl $ T "seltsam"   "weird"
    , AdjInfl $ T "dünn" "thin"
    , AdjInfl $ T "dick" "thick"
    , AdjInfl $ T "schlank" "slim"
    , AdjInfl $ T "fett" "fat"
    , AdjInfl $ T "einfach" "easy"
    , AdjInfl $ T "schwierig" "difficult"
    , AdjInfl $ T "klug" "smart"
    , AdjInfl $ T "dumm" "dumb"
    , AdjInfl $ T "schnell" "fast"
    , AdjInfl $ T "langsam" "slow"
    , AdjInfl $ T "teuer" "expensive"
    , AdjInfl $ T "billig" "cheap"
    , AdjInfl $ T "bunt" "colorful"
    , AdjInfl $ T "matt" "dull"
    , AdjInfl $ T "heiß" "hot"
    , AdjInfl $ T "kalt" "cold"
    , AdjInfl $ T "warm" "warm"
    , AdjInfl $ T "kühl" "cool"
    -- , T "" ""
    -- , T "dies" "this"
    ]

nouns =
    [ N Mas "Hund" "Hunde" "Hunden"
      "dog" "dogs"
    , N Fem "Katze" "Katzen" "Katzen"
      "cat" "cats"
    , N Neu "Kätzchen" "Kätzchen" "Kätzchen"
      "kitten" "kittens"
    , N Neu "Buch" "Bücher" "Büchern"
      "book" "books"
    , N Neu "Hemd" "Hemden" "Hemden"
      "shirt" "shirts"
    , N Neu "T-Shirt" "T-Shirts" "T-Shirts"
      "T-shirt" "T-shirts"
    , N Mas "Schal" "Schale" "Schalen"
      "scarf" "scarves"
    , N Mas "Schuh" "Schuhe" "Schuhen"
      "shoe" "shoes"
    , N Mas "Turnschuh" "Turnschuhe" "Turnschuhen"
      "gym-shoe" "gym-shoes"
    , N Mas "Handschuh" "Handschuhe" "Handschuhen"
      "glove" "gloves"
    , N Mas "Smoking" "Smokings" "Smokings"
      "tuxedo" "tuxedos"
    , N Mas "Gürtel" "Gürtel" "Gürteln"
      "belt" "belts"
    , N Mas "Hut" "Hüte" "Hüten"
      "hat" "hats"
    , N Mas "Mantel" "Mäntel" "Mänteln"
      "coat" "coats"
    , N Mas "Regenmantel" "Regenmäntel" "Regenmänteln"
      "raincoat" "raincoats"
    , N Mas "Anzug" "Anzüge" "Anzügen"
      "suit" "suits"
    , N Fem "Mütze" "Mützen" "Mützen"
      "cap" "caps"
    , N Fem "Tasche" "Taschen" "Taschen"
      "bag" "bags"
    , N Fem "Hose" "Hosen" "Hosen"
      "pant" "pants"
    , N Fem "Socke" "Socken" "Socken"
      "sock" "socks"
    , N Fem "Jacke" "Jacken" "Jacken"
      "jacket" "jackets"
    , N Fem "Krawatte" "Krawatten" "Krawatten"
      "necktie" "neckties"
    , N Fem "Unterhose" "Unterhosen" "Unterhosen"
      "underpant" "underpants"
    , N Fem "Kleidung" "Kleidungen" "Kleidungen"
      "piece-of-clothing" "pieces-of-clothing"
    , N Fem "Jeans" "Jeans" "Jeans"
      "jean-pant" "jean-pants"
    , N Mas "Baum" "Bäume" "Bäumen"
      "tree" "trees"
    , N Mas "Knopf" "Knöpfe" "Knöpfen"
      "button" "buttons"
    , N Mas "Apfel" "Äpfel" "Äpfeln"
      "apple" "apples"
    , N Mas "Zucker" "Zucker" "Zuckern"
      "sugar" "sugars"
    , N Mas "Wurm" "Würmer" "Würmern"
      "worm" "worms"
    , N Fem "Banane" "Bananen" "Bananen"
      "banana" "bananas"
    , N Fem "Beere" "Beeren" "Beeren"
      "berry" "berries"
    , N Fem "Himbeere" "Himbeeren" "Himbeeren"
      "raspberry" "raspberries"
    , N Fem "Blaubeere" "Blaubeeren" "Blaubeeren"
      "blueberry" "blueberries"
    , N Fem "Erde" "Erden" "Erden"
      "earth" "earths"
    , N Fem "Erdbeere" "Erdbeeren" "Erdbeeren"
      "strawberry" "strawberries"
    , N Fem "Brombeere" "Brombeeren" "Brombeeren"
      "blackberry" "blackberries"
    , N Neu "Bier" "Biere" "Bieren"
      "beer" "beers"
    , N Mas "Wein" "Weine" "Weinen"
      "wine" "wines"
    , N Fem "Kartoffel" "Kartoffeln" "Kartoffeln"
      "potato" "potatoes"
    , N Fem "Karotte" "Karotten" "Karotten"
      "carrot" "carrots"
    , N Mas "Kohl" "Kohle" "Kohlen"
      "cabbage" "cabbages"
    , N Fem "Bohne" "Bohnen" "Bohnen"
      "bean" "beans"
    , N Mas "Fisch" "Fische" "Fischen"
      "fish" "fishes"
    , N Mas "Reis" "Reise" "Reisen"
      "rice" "rices"
    , N Fem "Orange" "Orangen" "Orangen"
      "orange" "oranges"
    , N Fem "Zitrone" "Zitronen" "Zitronen"
      "lemon" "lemons"
    , N Mas "Spinat" "Spinate" "Spinaten"
      "spinach" "spinaches"
    , N Fem "Traube" "Trauben" "Trauben"
      "grape" "grapes"
    , N Mas "Pilz" "Pilze" "Pilzen"
      "mushroom" "mushrooms"
    , N Neu "Ding" "Dinge" "Dingen"
      "thing" "things"
    , N Mas "Winter" "Winter" "Wintern"
      "winter" "winters"
    , N Fem "Frühling" "Frühlinge" "Frühlingen"
      "spring-season" "spring-seasons"
    , N Mas "Sommer" "Sommer" "Sommern"
      "summer" "summers"
    , N Mas "Herbst" "Herbste" "Herbsten"
      "autumn" "autumns"
    , N Mas "Name" "Namen" "Namen"
      "name" "names"
    , N Fem "Schwiegermutter" "Schwiegermütter" "Schwiegerüttern"
      "mother-in-law" "mothers-in-law"
    , N Mas "Stift" "Stifte" "Stiften"
      "pen(cil)" "pen(cil)s"
    , N Mas "Bleistift" "Bleistifte" "Bleistiften"
      "pencil" "pencils"
    , N Neu "Bier" "Biere" "Bieren"
      "beer" "beers"
    , N Neu "Loch" "Löcher" "Löchern"
      "hole" "holes"
    , N Mas "Artikel" "Artikel" "Artikeln"
      "article" "articles"
    , N Fem "Art" "Arten" "Arten"
      "kind" "kinds"
    , N Neu "Wort" "Wörter" "Wörtern"
      "word" "words"
    , N Neu "Tier" "Tiere" "Tieren"
      "animal" "animals"
    , NEnForm Mas "Mensch" "Menschen" "Menschen"
      "human" "humans"
    , N Mas "Garten" "Gärten" "Gärten"
      "garden" "gardens"
    {-
    , N Fem "Wortart" "Wortarten" "Wortarten"
      "part-of-speech" "parts-of-speech"
    , N Neu "Verb" "Verben" "Verben"
      "verb" "verbs"
    , N Neu "Partizip" "Partizipien" "Partizipien"
      "participle" "participles"
    , N Neu "Substantiv" "Substantive" "Substantiven"
      "noun" "nouns"
    , N Neu "Pronomen" "Pronomen" "Pronomen"
      "pronoun" "pronouns"
    , N Neu "Adjektiv" "Adjektive" "Adjektiven"
      "adjective" "adjectives"
    , N Neu "Determinativ" "Determinative" "Determinativen"
      "determiner" "determiners"
    , N Neu "Adverb" "Adverbien" "Adverbien"
      "adverb" "adverbs"
    , N Fem "Präposition" "Präpositionen" "Präpositionen"
      "prepositon" "prepositions"
    , N Neu "Präsens" "Präsentia" "Präsentia"
      "present-tense" "present-tenses"
    -}
    ]

contractions =
    [ (("an",     "das"), "ans")
    , (("an",     "dem"), "am")
    , (("auf",    "das"), "aufs")
    , (("bei",    "dem"), "beim")
    , (("durch",  "das"), "durchs")
    , (("für",    "das"), "fürs")
    , (("hinter", "das"), "hinters")
    , (("in",     "das"), "ins")
    , (("in",     "dem"), "im")
    , (("über",   "das"), "übers")
    , (("um",     "das"), "ums")
    , (("unter",  "das"), "unters")
    , (("von",    "dem"), "vom")
    , (("vor",    "das"), "vors")
    , (("zu",     "dem"), "zum")
    , (("zur",    "der"), "zur")
    ]

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

onTF :: (Str -> Str) -> T -> T
onTF f (T tF tE) = T (f tF) tE

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

inflAdj :: Case -> Gender -> Number -> InflType -> Adj -> T
inflAdj _ _ _ _ (AdjNoInfl t) = t
inflAdj c g n i (AdjInfl t) = onTF (<> adjEnding c g n i) t
inflAdj c g n i (AdjIrreg _ fStem _ _ e) = T (fStem <> adjEnding c g n i) e

adjEnding :: Case -> Gender -> Number -> InflType -> Str
adjEnding Nom _ Plur Strong = "e"
adjEnding Nom Mas _  Strong = "er"
adjEnding Nom Fem _  Strong = "e"
adjEnding Nom Neu _  Strong = "es"
adjEnding Acc _ Plur Strong = "e"
adjEnding Acc Mas _  Strong = "en"
adjEnding Acc Fem _  Strong = "e"
adjEnding Acc Neu _  Strong = "es"
adjEnding Dat _ Plur Strong = "en"
adjEnding Dat Mas _  Strong = "em"
adjEnding Dat Fem _  Strong = "er"
adjEnding Dat Neu _  Strong = "em"
adjEnding Gen _ Plur Strong = "er"
adjEnding Gen Mas _  Strong = "en"
adjEnding Gen Fem _  Strong = "en"
adjEnding Gen Neu _  Strong = "er"
adjEnding Nom _ Plur Mixed = "en"
adjEnding Nom Mas _  Mixed = "er"
adjEnding Nom Fem _  Mixed = "e"
adjEnding Nom Neu _  Mixed = "es"
adjEnding Acc _ Plur Mixed = "en"
adjEnding Acc Mas _  Mixed = "en"
adjEnding Acc Fem _  Mixed = "e"
adjEnding Acc Neu _  Mixed = "es"
adjEnding Dat _   _  Mixed = "en"
adjEnding Gen _   _  Mixed = "en"
adjEnding Nom _ Plur Weak = "en"
adjEnding Nom Mas _  Weak = "e"
adjEnding Nom Fem _  Weak = "e"
adjEnding Nom Neu _  Weak = "e"
adjEnding Acc _ Plur Weak = "en"
adjEnding Acc Mas _  Weak = "en"
adjEnding Acc Fem _  Weak = "e"
adjEnding Acc Neu _  Weak = "e"
adjEnding Dat _   _  Weak = "en"
adjEnding Gen _   _  Weak = "en"

nToT :: Case -> Number -> N -> T
nToT _   Sing n = T (nF n) (nE n)
nToT Dat Plur n = T (nFDatPlur n) (nEPlur n)
nToT _   Plur n = T (nFPlur n) (nEPlur n)

-- verbs decline based on person, number, and tense.

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

{-
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
    npType <- sel [1, 2]
    case npType of
      1 -> do
        pers <- sel [P1, P2, P3]
        gend <- sel [Mas, Fem, Neu]
        num <- sel [Sing, Plur]
        formality <- sel [Formal, Informal]
        return ([pronOf cas pers gend num formality], (pers, num, formality))
      2 -> do
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
              , nToT cas nounNum noun
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
    doQ (doSent $ map tE s) (doSent $ map tF s)
