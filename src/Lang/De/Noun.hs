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
    | NDeEn !Str
    | NDeN !Str
    | NDeS !Str
    | NDeSame !Str
    | NDeSameN !Str
    | NDeIrreg !Str !Str !Str

data NEn
    = NEnS !Str
    | NEnSame !Str  -- Unused here since can be ambiguous for En->De.
    | NEnIrreg !Str !Str

inflNDe :: Case -> Number -> NDe -> Str
inflNDe _   Sing (NDeReg w)       = w
inflNDe Dat Plur (NDeReg w)       = w <> "en"
inflNDe _   Plur (NDeReg w)       = w <> "e"
inflNDe _   Sing (NDeEn w)        = w
inflNDe _   Plur (NDeEn w)        = w <> "en"
inflNDe _   Sing (NDeN w)         = w
inflNDe _   Plur (NDeN w)         = w <> "n"
inflNDe _   Sing (NDeS w)         = w
inflNDe _   Plur (NDeS w)         = w <> "s"
inflNDe _   _    (NDeSame w)      = w
inflNDe Dat Plur (NDeSameN w)     = w <> "n"
inflNDe _   _    (NDeSameN w)     = w
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
    , N Fem (NDeN "Katze") (NEnS "cat")
    , N Neu (NDeSame "Kätzchen") (NEnS "kitten")
    , N Neu (NDeIrreg "Buch" "Bücher" "Büchern") (NEnS "book")
    , N Neu (NDeEn "Hemd") (NEnS "shirt")
    , N Neu (NDeS "T-Shirt") (NEnS "T-shirt")
    , N Mas (NDeReg "Schal") (NEnIrreg "scarf" "scarves")
    , N Mas (NDeReg "Schuh") (NEnS "shoe")
    , N Mas (NDeReg "Turnschuh") (NEnS "gym-shoe")
    , N Mas (NDeReg "Handschuh") (NEnS "glove")
    , N Mas (NDeS "Smoking") (NEnS "tuxedo")
    , N Mas (NDeSameN "Gürtel") (NEnS "belt")
    , N Mas (NDeIrreg "Hut" "Hüte" "Hüten") (NEnS "hat")
    , N Mas (NDeIrreg "Mantel" "Mäntel" "Mänteln") (NEnS "coat")
    , N Mas (NDeIrreg "Regenmantel" "Regenmäntel" "Regenmänteln")
      (NEnS "raincoat")
    , N Mas (NDeIrreg "Anzug" "Anzüge" "Anzügen") (NEnS "suit")
    , N Fem (NDeN "Mütze") (NEnS "cap")
    , N Fem (NDeN "Tasche") (NEnS "bag")
    , N Fem (NDeN "Hose") (NEnS "pant")
    , N Fem (NDeN "Socke") (NEnS "sock")
    , N Fem (NDeN "Jacke") (NEnS "jacket")
    , N Fem (NDeN "Krawatte") (NEnS "necktie")
    , N Fem (NDeN "Unterhose") (NEnS "underpant")
    , N Fem (NDeEn "Kleidung")
      (NEnIrreg "piece-of-clothing" "pieces-of-clothing")
    , N Fem (NDeSame "Jeans") (NEnS "jean-pant")
    , N Mas (NDeIrreg "Baum" "Bäume" "Bäumen") (NEnS "tree")
    , N Mas (NDeIrreg "Knopf" "Knöpfe" "Knöpfen") (NEnS "button")
    , N Mas (NDeIrreg "Apfel" "Äpfel" "Äpfeln") (NEnS "apple")
    , N Mas (NDeIrreg "Zucker" "Zucker" "Zuckern") (NEnS "sugar")
    , N Mas (NDeIrreg "Wurm" "Würmer" "Würmern") (NEnS "worm")
    , N Fem (NDeN "Banane") (NEnS "banana")
    , N Fem (NDeN "Beere") (NEnIrreg "berry" "berries")
    , N Fem (NDeN "Himbeere") (NEnIrreg "raspberry" "raspberries")
    , N Fem (NDeN "Blaubeere") (NEnIrreg "blueberry" "blueberries")
    , N Fem (NDeN "Erde") (NEnS "earth")
    , N Fem (NDeN "Erdbeere") (NEnIrreg "strawberry" "strawberries")
    , N Fem (NDeN "Brombeere") (NEnIrreg "blackberry" "blackberries")
    , N Neu (NDeReg "Bier") (NEnS "beer")
    , N Mas (NDeReg "Wein") (NEnS "wine")
    , N Fem (NDeN "Kartoffel") (NEnIrreg "potato" "potatoes")
    , N Fem (NDeN "Karotte") (NEnS "carrot")
    , N Mas (NDeReg "Kohl") (NEnS "cabbage")
    , N Fem (NDeN "Bohne") (NEnS "bean")
    , N Mas (NDeReg "Fisch") (NEnIrreg "fish" "fishes")
    , N Mas (NDeReg "Reis") (NEnS "rice")
    , N Fem (NDeN "Orange") (NEnS "orange")
    , N Fem (NDeN "Zitrone") (NEnS "lemon")
    , N Mas (NDeReg "Spinat") (NEnIrreg "spinach" "spinaches")
    , N Fem (NDeN "Traube") (NEnS "grape")
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
    , N Fem (NDeEn "Art") (NEnS "kind")
    , N Neu (NDeIrreg "Wort" "Wörter" "Wörtern") (NEnS "word")
    , N Neu (NDeReg "Tier") (NEnS "animal")
    , N Mas (NDeIrreg "Garten" "Gärten" "Gärten") (NEnS "garden")
    , N Mas (NDeIrreg "Himmel" "Himmel" "Himmeln") (NEnIrreg "sky" "skies")
    , N Fem (NDeN "Wolke") (NEnS "cloud")
    , N Fem (NDeN "Sandale") (NEnS "sandal")
    -- Body parts:
    , N Neu (NDeIrreg "Haar" "Haare" "Haaren") (NEnS "hair")
    , N Mas (NDeIrreg "Kopf" "Köpfe" "Köpfen") (NEnS "head")
    , N Mas (NDeReg "Gehirn") (NEnS "brain")
    , N Neu (NDeIrreg "Auge" "Augen" "Augen") (NEnS "eye")
    , N Neu (NDeIrreg "Ohr" "Ohren" "Orhen") (NEnS "ear")
    , N Fem (NDeN "Nase") (NEnS "nose")
    , N Fem (NDeN "Lippe") (NEnS "lip")
    , N Mas (NDeIrreg "Mund" "Münder" "Mündern") (NEnS "mouth")
    , N Mas (NDeIrreg "Hals" "Hälse" "Hälsen") (NEnS "neck/throat")
    , N Fem (NDeIrreg "Brust" "Brüste" "Brüsten") (NEnS "breast")
    , N Fem (NDeN "Schulter") (NEnS "shoulder")
    , N Mas (NDeReg "Arm") (NEnS "arm")
    , N Mas (NDeSame "Ellbogen") (NEnS "elbow")
    , N Mas (NDeIrreg "Finger" "Finger" "Fingern") (NEnS "finger")
    , N Fem (NDeN "Taille") (NEnS "waist")
    , N Fem (NDeN "Hüfte") (NEnS "hip")
    , N Neu (NDeReg "Bein") (NEnS "leg")
    , N Neu (NDeIrreg "Knie" "Knie" "Knien") (NEnS "knee")
    , N Mas (NDeIrreg "Zeh" "Zehen" "Zehen") (NEnS "toe")
    , N Fem (NDeIrreg "Haut" "Häute" "Häuten") (NEnS "skin")
    , N Mas (NDeIrreg "Knochen" "Knochen" "Knochen") (NEnS "bone")
    , N Mas (NDeSame "Körper") (NEnIrreg "body" "bodies")
    , N Mas (NDeSame "Rücken") (NEnS "back")

    , N Mas (NDeIrreg "Knöchel" "Knöchel" "Knöcheln") (NEnS "ankle/knuckle")
    , N Mas (NDeIrreg "Nagel" "Nägel" "Nägeln") (NEnS "nail")
    , N Mas (NDeSame "Samen") (NEnS "seed")
    , N Neu (NDeIrreg "Sperma" "Spermen" "Spermen") (NEnS "sperm")
    , N Fem (NDeN "Wichse") (NEnS "cum")
    , N Neu (NDeReg "Wachs") (NEnIrreg "wax" "waxes")
    , N Fem (NDeN "Liebe") (NEnS "love")
    , N Neu (NDeSame "Vergnügen") (NEnS "pleasure")
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
