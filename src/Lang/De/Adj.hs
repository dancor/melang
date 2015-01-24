{-# LANGUAGE OverloadedStrings #-}

module Lang.De.Adj where

import Data.Monoid

import Lang.De.Base
import Str

data Adj = AdjInfl T | AdjNoInfl T
    | AdjIrreg
    { adjIrregF :: Str
    , adjIrregFStem :: Str
    , adjIrregFEr :: Str
    , adjIrregFEst :: Str
    , adjIrregE :: Str
    }

adjsNoInfl :: [T]
adjsNoInfl =
    [ T "rosa" "pink"
    ]

adjs :: [Adj]
adjs =
    [ AdjNoInfl $ T "rosa" "pink"
    , AdjIrreg "orange" "orang" "oranger" "am orangest" "orange"
    , AdjIrreg "hoch" "hoch" "höher" "am höchsten" "high"
    , AdjInfl $ T "nieder" "low"
    
    , AdjInfl $ T "neu"   "new"
    , AdjInfl $ T "alt"   "old"
    , AdjInfl $ T "jung"  "young"
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
    , AdjInfl $ T "nass" "wet"
    , AdjInfl $ T "trocken" "dry"
    , AdjInfl $ T "sauber" "clean"
    , AdjInfl $ T "schmutzig" "dirty"
    , AdjInfl $ T "genau" "exact"
    , AdjInfl $ T "ungefähr" "approximate"
    , AdjInfl $ T "reich" "rich"
    , AdjInfl $ T "arm" "poor"
    , AdjInfl $ T "früh" "early"
    , AdjInfl $ T "spät" "late"
    , AdjInfl $ T "laut" "loud"
    , AdjInfl $ T "ruhig" "quiet"
    , AdjInfl $ T "weiter" "forwards"
    , AdjInfl $ T "zurück" "backwards"
    , AdjInfl $ T "süß" "sweet"
    , AdjInfl $ T "salzig" "salty"
    , AdjInfl $ T "kurz" "short"
    , AdjInfl $ T "lang" "long"
    -- , T "" ""
    -- , T "dies" "this"
    ]

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
