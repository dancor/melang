{-# LANGUAGE OverloadedStrings #-}

module Lang.De.Noun where

import Data.Monoid

import Lang.De.Base
import Str

data N
    = N
    { nG :: Gender
    , nDe :: NDe
    , nEn :: NEn
    }

data NDe
    = NDeReg !Str
    | NDeS !Str
    | NDeSame !Str
    | NDeIrreg !Str !Str !Str

data NEn
    = NEnS !Str
    | NEnSame !Str  -- Unused here since can be ambiguous for En->De.
    | NEnIrreg !Str !Str

inflNDe :: Case -> Number -> NDe -> Str
inflNDe _   Sing (NDeReg w)       = w
inflNDe Dat Plur (NDeReg w)       = w <> "en"
inflNDe _   Plur (NDeReg w)       = w <> "e"
inflNDe _   Sing (NDeS w)         = w
inflNDe _   _    (NDeS w)         = w <> "s"
inflNDe _   _    (NDeSame w)      = w
inflNDe _   Sing (NDeIrreg w _ _) = w
inflNDe Dat Plur (NDeIrreg _ _ w) = w
inflNDe _   Plur (NDeIrreg _ w _) = w

inflNEn :: Number -> NEn -> Str
inflNEn Sing (NEnS w)     = w
inflNEn Plur (NEnS w)     = w <> "s"
inflNEn _    (NEnSame w)    = w
inflNEn Sing (NEnIrreg w _) = w
inflNEn Plur (NEnIrreg _ w) = w

inflN :: Case -> Number -> N -> T
inflN cas num (N _ de en) = T (inflNDe cas num de) (inflNEn num en)

nouns :: [N]
nouns =
    [ N Mas (NDeReg "Hund") (NEnS "dog")
    , N Fem (NDeIrreg "Katze" "Katzen" "Katzen") (NEnS "cat")
    , N Neu (NDeSame "Kätzchen") (NEnS "kitten")
    , N Neu (NDeIrreg "Buch" "Bücher" "Büchern") (NEnS "book")
    , N Neu (NDeIrreg "Hemd" "Hemden" "Hemden") (NEnS "shirt")
    , N Neu (NDeS "T-Shirt") (NEnS "T-shirt")
    , N Mas (NDeReg "Schal") (NEnIrreg "scarf" "scarves")
    , N Mas (NDeReg "Schuh") (NEnS "shoe")
    , N Mas (NDeReg "Turnschuh") (NEnS "gym-shoe")
    , N Mas (NDeReg "Handschuh") (NEnS "glove")
    , N Mas (NDeS "Smoking") (NEnS "tuxedo")
    , N Mas (NDeIrreg "Gürtel" "Gürtel" "Gürteln") (NEnS "belt")
    , N Mas (NDeIrreg "Hut" "Hüte" "Hüten") (NEnS "hat")
    , N Mas (NDeIrreg "Mantel" "Mäntel" "Mänteln") (NEnS "coat")
    , N Mas (NDeIrreg "Regenmantel" "Regenmäntel" "Regenmänteln")
      (NEnS "raincoat")
    , N Mas (NDeIrreg "Anzug" "Anzüge" "Anzügen") (NEnS "suit")
    , N Fem (NDeIrreg "Mütze" "Mützen" "Mützen") (NEnS "cap")
    , N Fem (NDeIrreg "Tasche" "Taschen" "Taschen") (NEnS "bag")
    , N Fem (NDeIrreg "Hose" "Hosen" "Hosen") (NEnS "pant")
    , N Fem (NDeIrreg "Socke" "Socken" "Socken") (NEnS "sock")
    , N Fem (NDeIrreg "Jacke" "Jacken" "Jacken") (NEnS "jacket")
    , N Fem (NDeIrreg "Krawatte" "Krawatten" "Krawatten") (NEnS "necktie")
    , N Fem (NDeIrreg "Unterhose" "Unterhosen" "Unterhosen") (NEnS "underpant")
    , N Fem (NDeIrreg "Kleidung" "Kleidungen" "Kleidungen")
      (NEnIrreg "piece-of-clothing" "pieces-of-clothing")
    , N Fem (NDeSame "Jeans") (NEnS "jean-pant")
    , N Mas (NDeIrreg "Baum" "Bäume" "Bäumen") (NEnS "tree")
    , N Mas (NDeIrreg "Knopf" "Knöpfe" "Knöpfen") (NEnS "button")
    , N Mas (NDeIrreg "Apfel" "Äpfel" "Äpfeln") (NEnS "apple")
    , N Mas (NDeIrreg "Zucker" "Zucker" "Zuckern") (NEnS "sugar")
    , N Mas (NDeIrreg "Wurm" "Würmer" "Würmern") (NEnS "worm")
    , N Fem (NDeIrreg "Banane" "Bananen" "Bananen") (NEnS "banana")
    , N Fem (NDeIrreg "Beere" "Beeren" "Beeren") (NEnIrreg "berry" "berries")
    , N Fem (NDeIrreg "Himbeere" "Himbeeren" "Himbeeren")
      (NEnIrreg "raspberry" "raspberries")
    , N Fem (NDeIrreg "Blaubeere" "Blaubeeren" "Blaubeeren")
      (NEnIrreg "blueberry" "blueberries")
    , N Fem (NDeIrreg "Erde" "Erden" "Erden") (NEnS "earth")
    , N Fem (NDeIrreg "Erdbeere" "Erdbeeren" "Erdbeeren")
      (NEnIrreg "strawberry" "strawberries")
    , N Fem (NDeIrreg "Brombeere" "Brombeeren" "Brombeeren")
      (NEnIrreg "blackberry" "blackberries")
    , N Neu (NDeReg "Bier") (NEnS "beer")
    , N Mas (NDeReg "Wein") (NEnS "wine")
    , N Fem (NDeIrreg "Kartoffel" "Kartoffeln" "Kartoffeln")
      (NEnIrreg "potato" "potatoes")
    , N Fem (NDeIrreg "Karotte" "Karotten" "Karotten") (NEnS "carrot")
    , N Mas (NDeReg "Kohl") (NEnS "cabbage")
    , N Fem (NDeIrreg "Bohne" "Bohnen" "Bohnen") (NEnS "bean")
    , N Mas (NDeReg "Fisch") (NEnIrreg "fish" "fishes")
    , N Mas (NDeReg "Reis") (NEnS "rice")
    , N Fem (NDeIrreg "Orange" "Orangen" "Orangen") (NEnS "orange")
    , N Fem (NDeIrreg "Zitrone" "Zitronen" "Zitronen") (NEnS "lemon")
    , N Mas (NDeReg "Spinat") (NEnIrreg "spinach" "spinaches")
    , N Fem (NDeIrreg "Traube" "Trauben" "Trauben") (NEnS "grape")
    , N Mas (NDeReg "Pilz") (NEnS "mushroom")
    , N Neu (NDeReg "Ding") (NEnS "thing")
    , N Mas (NDeIrreg "Winter" "Winter" "Wintern") (NEnS "winter")
    , N Fem (NDeReg "Frühling") (NEnS "spring-season")
    , N Mas (NDeIrreg "Sommer" "Sommer" "Sommern") (NEnS "summer")
    , N Mas (NDeReg "Herbst") (NEnS "autumn")
    , N Mas (NDeIrreg "Name" "Namen" "Namen") (NEnS "name")
    , N Fem (NDeIrreg "Schwiegermutter" "Schwiegermütter" "Schwiegerüttern")
      (NEnIrreg "mother-in-law" "mothers-in-law")
    , N Mas (NDeReg "Stift") (NEnS "pen(cil)")
    , N Mas (NDeReg "Bleistift") (NEnS "pencil")
    , N Neu (NDeReg "Bier") (NEnS "beer")
    , N Neu (NDeIrreg "Loch" "Löcher" "Löchern") (NEnS "hole")
    , N Mas (NDeIrreg "Artikel" "Artikel" "Artikeln") (NEnS "article")
    , N Fem (NDeIrreg "Art" "Arten" "Arten") (NEnS "kind")
    , N Neu (NDeIrreg "Wort" "Wörter" "Wörtern") (NEnS "word")
    , N Neu (NDeReg "Tier") (NEnS "animal")
    , N Mas (NDeIrreg "Garten" "Gärten" "Gärten") (NEnS "garden")
    , N Mas (NDeIrreg "Himmel" "Himmel" "Himmeln") (NEnIrreg "sky" "skies")
    , N Fem (NDeIrreg "Wolke" "Wolken" "Wolken") (NEnS "cloud")
    , N Fem (NDeIrreg "Sandale" "Sandalen" "Sandalen") (NEnS "sandal")
    , N Neu (NDeReg "Bein") (NEnS "leg")
    , N Mas (NDeIrreg "Knochen" "Knochen" "Knochen") (NEnS "bone")
    , N Neu (NDeIrreg "Knie" "Knie" "Knien") (NEnS "knee")
    , N Mas (NDeIrreg "Kopf" "Köpfe" "Köpfen") (NEnS "head")
    , N Fem (NDeIrreg "Brust" "Brüste" "Brüsten") (NEnS "breast")
    , N Mas (NDeIrreg "Finger" "Finger" "Fingern") (NEnS "finger")
    , N Mas (NDeSame "Samen") (NEnS "seed")
    , N Neu (NDeIrreg "Sperma" "Spermen" "Spermen") (NEnS "sperm")
    , N Fem (NDeIrreg "Wichse" "Wichsen" "Wichsen") (NEnS "cum")
    , N Neu (NDeReg "Wachs") (NEnIrreg "wax" "waxes")
    , N Fem (NDeIrreg "Liebe" "Lieben" "Lieben") (NEnS "love")
    , N Mas (NDeSame "Körper") (NEnIrreg "body" "bodies")
    , N Mas (NDeReg "Gehirn") (NEnS "brain")
    , N Neu (NDeSame "Vergnügen") (NEnS "pleasure")
    , N Mas (NDeSame "Rücken") (NEnS "back")
    , N Mas (NDeIrreg "Zeh" "Zehen" "Zehen") (NEnS "toe")
    , N Neu (NDeReg "Klavier") (NEnS "piano")
    {-
    , NEnForm Mas "Mensch" "Menschen" "Menschen"
      "human" "humans"
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
    , N Fem "Abkürzung" "Abkürzungen" "Abkürzungen"
      "abbreviation" "abbreviations"
    , N Mas "Weg" "Wege" "Wegen"
      "way" "ways"
    , N -   -     "Shorts" "Shorts"
      - "shorts"
    , N Mas "Begriff" "Begriffe" "Begriffen"
      "term" "terms"
    -}
    ]

