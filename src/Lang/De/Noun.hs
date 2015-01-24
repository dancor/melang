{-# LANGUAGE OverloadedStrings #-}

module Lang.De.Noun where

import Data.Monoid
import qualified Data.Text as Str

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
    -- 3rd is always 2nd plus "n" if 2nd ends "e", "l", or "r"
    -- and nothing if 2nd ends in "s"
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

nDeIrreg :: Str -> Str -> NDe
nDeIrreg s p = NDeIrreg s p (if "n" `Str.isSuffixOf` p then p else p <> "n")

nouns :: [N]
nouns =
    [ N Mas (NDeReg "Hund") (NEnS "dog")
    , N Fem (NDeN "Katze") (NEnS "cat")
    , N Neu (NDeSame "Kätzchen") (NEnS "kitten")
    , N Neu (nDeIrreg "Buch" "Bücher") (NEnS "book")

    -- Clothing terms.
    , N Fem (NDeEn "Kleidung")
      (NEnIrreg "piece-of-clothing" "pieces-of-clothing")
    , N Neu (NDeEn "Hemd") (NEnS "shirt")
    , N Neu (NDeS "T-Shirt") (NEnS "T-shirt")
    , N Mas (NDeReg "Schal") (NEnIrreg "scarf" "scarves")
    , N Mas (NDeReg "Schuh") (NEnS "shoe")
    , N Mas (NDeReg "Turnschuh") (NEnS "gym-shoe")
    , N Mas (NDeReg "Handschuh") (NEnS "glove")
    , N Mas (NDeS "Smoking") (NEnS "tuxedo")
    , N Mas (NDeSameN "Gürtel") (NEnS "belt")
    , N Mas (nDeIrreg "Hut" "Hüte") (NEnS "hat")
    , N Mas (nDeIrreg "Mantel" "Mäntel") (NEnS "coat")
    , N Mas (nDeIrreg "Regenmantel" "Regenmäntel") (NEnS "raincoat")
    , N Mas (nDeIrreg "Anzug" "Anzüge") (NEnS "suit")
    , N Fem (NDeN "Mütze") (NEnS "cap")
    , N Fem (NDeN "Tasche") (NEnS "bag")
    , N Fem (NDeN "Hose") (NEnS "pant")
    , N Fem (NDeN "Socke") (NEnS "sock")
    , N Fem (NDeN "Jacke") (NEnS "jacket")
    , N Fem (NDeN "Krawatte") (NEnS "necktie")
    , N Fem (NDeN "Unterhose") (NEnS "underpant")
    , N Fem (NDeN "Sandale") (NEnS "sandal")
    , N Fem (NDeSame "Jeans") (NEnS "jean-pant")

    , N Mas (nDeIrreg "Baum" "Bäume") (NEnS "tree")
    , N Mas (nDeIrreg "Knopf" "Knöpfe") (NEnS "button")
    , N Mas (nDeIrreg "Apfel" "Äpfel") (NEnS "apple")
    , N Mas (NDeSameN "Zucker") (NEnS "sugar")
    , N Mas (nDeIrreg "Wurm" "Würmer") (NEnS "worm")
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
    , N Mas (NDeSameN "Winter") (NEnS "winter")
    , N Fem (NDeReg "Frühling") (NEnS "spring-season")
    , N Mas (NDeSameN "Sommer") (NEnS "summer")
    , N Mas (NDeReg "Herbst") (NEnS "autumn")
    , N Mas (NDeN "Name") (NEnS "name")
    , N Mas (NDeReg "Stift") (NEnS "pen(cil)")
    , N Mas (NDeReg "Bleistift") (NEnS "pencil")
    , N Neu (NDeReg "Bier") (NEnS "beer")
    , N Neu (nDeIrreg "Loch" "Löcher") (NEnS "hole")
    , N Mas (NDeSameN "Artikel") (NEnS "article")
    , N Fem (NDeEn "Art") (NEnS "kind")
    , N Neu (nDeIrreg "Wort" "Wörter") (NEnS "word")
    , N Neu (NDeReg "Tier") (NEnS "animal")
    , N Mas (nDeIrreg "Garten" "Gärten") (NEnS "garden")
    , N Mas (NDeSameN "Himmel") (NEnIrreg "sky" "skies")
    , N Fem (NDeN "Wolke") (NEnS "cloud")
    , N Fem (NDeEn "Erfahrung") (NEnS "experience")
    , N Mas (NDeEn "Typ") (NEnS "type")
    , N Mas (NDeSameN "Computer") (NEnS "computer")
    , N Mas (NDeS "Laptop") (NEnS "laptop")
    , N Fem (NDeN "Welle") (NEnS "wave")
    , N Neu (NDeIrreg "Gefängnis" "Gefängnisse") (NEnS "prison")
    , N Neu (NDeIrreg "Datum" "Daten") (NEnS "time-date")
    , N Fem (NDeN "Stelle") (NEnS "location")
    , N Neu (NDeReg "Geshenk") (NEnS "gift")
    , N Fem (NDeIrreg "Kuh" "Kühe") (NEnS "cow")
    , N Neu (NDeEr "Hausrind") (NEnS "domestic-cattle")

    -- Body parts:
    , N Neu (NDeReg "Haar") (NEnS "hair")
    , N Mas (nDeIrreg "Kopf" "Köpfe") (NEnS "head")
    , N Mas (NDeReg "Gehirn") (NEnS "brain")
    , N Neu (NDeN "Auge") (NEnS "eye")
    , N Neu (NDeEn "Ohr") (NEnS "ear")
    , N Fem (NDeN "Nase") (NEnS "nose")
    , N Fem (NDeN "Lippe") (NEnS "lip")
    , N Mas (nDeIrreg "Mund" "Münder") (NEnS "mouth")
    , N Mas (nDeIrreg "Hals" "Hälse") (NEnS "neck/throat")
    , N Fem (nDeIrreg "Brust" "Brüste") (NEnS "breast")
    , N Fem (NDeN "Schulter") (NEnS "shoulder")
    , N Mas (NDeReg "Arm") (NEnS "arm")
    , N Mas (NDeSame "Ellbogen") (NEnS "elbow")
    , N Mas (NDeSameN "Finger") (NEnS "finger")
    , N Fem (NDeN "Taille") (NEnS "waist")
    , N Fem (NDeN "Hüfte") (NEnS "hip")
    , N Neu (NDeReg "Bein") (NEnS "leg")
    , N Neu (NDeSameN "Knie") (NEnS "knee")
    , N Mas (NDeEn "Zeh") (NEnS "toe")
    , N Fem (nDeIrreg "Haut" "Häute") (NEnS "skin")
    , N Mas (NDeSame "Knochen") (NEnS "bone")
    , N Mas (NDeSameN "Körper") (NEnIrreg "body" "bodies")
    , N Mas (NDeSame "Rücken") (NEnS "back")

    , N Mas (NDeSameN "Knöchel") (NEnS "ankle/knuckle")
    , N Mas (nDeIrreg "Nagel" "Nägel") (NEnS "nail")
    , N Mas (NDeSame "Samen") (NEnS "seed")
    , N Neu (nDeIrreg "Sperma" "Spermen") (NEnS "sperm")
    , N Fem (NDeN "Wichse") (NEnS "cum")
    , N Neu (NDeReg "Wachs") (NEnIrreg "wax" "waxes")
    , N Fem (NDeN "Liebe") (NEnS "love")
    , N Neu (NDeSame "Vergnügen") (NEnS "pleasure")
    , N Neu (NDeN "Ende") (NEnS "end")
    , N Mas (nDeIrreg "Platz" "Plätze") (NEnS "place")

    -- Materials.
    , N Neu (nDeIrreg "Holz" "Hölzer") (NEnS "woods")

    -- House terms.
    , N Neu (NDeEn "Bett") (NEnS "bed")
    , N Mas (NDeIrreg "Stuhl" "Stühle") (NEnS "chair")

    -- Instruments.
    , N Neu (NDeReg "Klavier") (NEnS "piano")
    , N Fem (NDeN "Geige") (NEnS "violin")

    -- Name terms.
    , N Mas (NDeN "Name") (NEnS "name")

    -- Professions
    , N Mas (NDeSameN "Schreiner") (NEnS "carpenter")
    , N Mas (NDeSameN "Müller") (NEnS "miller")

    -- Time terms.
    , N Mas (NDeReg "Tag") (NEnS "day")
    , N Fem (NDeN "Woche") (NEnS "week")
    , N Mas (NDeReg "Monat") (NEnS "month")
    , N Neu (NDeReg "Jahr") (NEnS "year")
    , N Fem (nDeIrreg "Nacht" "Nächte") (NEnS "night")

    -- Family terms.
    , N Mas (nDeIrreg "Sohn" "Söhne") (NEnS "son")
    , N Fem (nDeIrreg "Tochter" "Töchter") (NEnS "daughter")
    , N Fem (nDeIrreg "Schwiegermutter" "Schwiegermütter")
      (NEnIrreg "mother-in-law" "mothers-in-law")
    , N Mas (nDeIrreg "Vater" "Väter") (NEnS "father")
    , N Fem (nDeIrreg "Mutter" "Mütter") (NEnS "mother")

    -- Meals.
    , N Neu (NDeReg "Frühstück") (NEnS "breakfast")
    , N Neu (NDeSame "Mittagessen") (NEnIrreg "lunch" "lunches")
    , N Neu (NDeSame "Abendessen") (NEnS "dinner")

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
