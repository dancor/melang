module Lang.De.Base where

import Str

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

data T = T -- translation term
    { tF :: Str -- foreign
    , tE :: Str -- English
    }

onTF :: (Str -> Str) -> T -> T
onTF f (T theTF theTE) = T (f theTF) theTE
