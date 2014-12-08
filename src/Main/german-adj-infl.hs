#include <h>

data Case = Nom | Acc | Dat | Gen deriving Eq

data Gender = Mas | Fem | Neu deriving Eq

data InflType = Strong | Weak | Mixed deriving Eq

data Number = Sing | Plur deriving Eq

data Person = P1 | P2 | P3 deriving Eq

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

adjsNoInfl =
    [ T "rosa" "pink"
    ]

adjsEinType =
    [ T "ein"   "one"
    , T "kein"  "none"
    ]

persPron =
    [ T "mein"  "my"
    , T "dein"  "your-singular"
    , T "sein"  "his"
    , T "ihr"   "her-their-or-Your"
    , T "unser" "our"
    , T "euer"  "yall's"
    ]

adjsNormal =
    [ T "neu"   "new"
    , T "alt"   "old"
    , T "groß"  "big"
    , T "klein" "small"
    , T "schön" "pretty"
    , T "hässlich"  "ugly"
    , T "weiß"      "white"
    , T "schwartz"  "black"
    , T "rot"       "red"
    , T "grün"      "green"
    , T "blau"      "blue"
    , T "gelb"      "yellow"
    , T "grau"      "gray"
    , T "violett"   "violet"
    , T "azur"      "azure"
    , T "golden"    "gold"
    , T "silber"    "silver"
    , T "glücklich" "happy"
    , T "traurig"   "sad"
    , T "normal"    "normal"
    , T "seltsam"   "weird"
    , T "dünn" "thin"
    , T "dick" "thick"
    , T "schlank" "slim"
    , T "fett" "fat"
    -- , T "" ""
    -- , T "dies" "this"
    ]

nouns =
    [ N Mas "Hund" "Hunde" "Hunden"
      "dog" "dogs"
    , N Fem "Katze" "Katzen" "Katzen"
      "cat" "cats"
    , N Neu "Buch" "Bücher" "Büchern"
      "book" "books"
    , N Neu "Hemd" "Hemden" "Hemden"
      "shirt" "shirts"
    , N Neu "T-Shirt" "T-Shirts" "T-Shirts"
      "T-shirt" "T-shirts"
    , N Mas "Schal" "Schale" "Schalen"
      "scarf" "scarves"
    , N Mas "Schuh" "Schuhe" "Schuhe"
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

inflAdj :: Case -> Gender -> Number -> InflType -> T -> T
inflAdj Nom _ Plur Strong = onTF (<> "e")
inflAdj Nom Mas _  Strong = onTF (<> "er")
inflAdj Nom Fem _  Strong = onTF (<> "e")
inflAdj Nom Neu _  Strong = onTF (<> "es")
inflAdj Acc _ Plur Strong = onTF (<> "e")
inflAdj Acc Mas _  Strong = onTF (<> "en")
inflAdj Acc Fem _  Strong = onTF (<> "e")
inflAdj Acc Neu _  Strong = onTF (<> "es")
inflAdj Dat _ Plur Strong = onTF (<> "en")
inflAdj Dat Mas _  Strong = onTF (<> "em")
inflAdj Dat Fem _  Strong = onTF (<> "er")
inflAdj Dat Neu _  Strong = onTF (<> "em")
inflAdj Gen _ Plur Strong = onTF (<> "er")
inflAdj Gen Mas _  Strong = onTF (<> "en")
inflAdj Gen Fem _  Strong = onTF (<> "en")
inflAdj Gen Neu _  Strong = onTF (<> "er")
inflAdj Nom _ Plur Mixed = onTF (<> "en")
inflAdj Nom Mas _  Mixed = onTF (<> "er")
inflAdj Nom Fem _  Mixed = onTF (<> "e")
inflAdj Nom Neu _  Mixed = onTF (<> "es")
inflAdj Acc _ Plur Mixed = onTF (<> "en")
inflAdj Acc Mas _  Mixed = onTF (<> "en")
inflAdj Acc Fem _  Mixed = onTF (<> "e")
inflAdj Acc Neu _  Mixed = onTF (<> "es")
inflAdj Dat _   _  Mixed = onTF (<> "en")
inflAdj Gen _   _  Mixed = onTF (<> "en")
inflAdj Nom _ Plur Weak = onTF (<> "en")
inflAdj Nom Mas _  Weak = onTF (<> "e")
inflAdj Nom Fem _  Weak = onTF (<> "e")
inflAdj Nom Neu _  Weak = onTF (<> "e")
inflAdj Acc _ Plur Weak = onTF (<> "en")
inflAdj Acc Mas _  Weak = onTF (<> "en")
inflAdj Acc Fem _  Weak = onTF (<> "e")
inflAdj Acc Neu _  Weak = onTF (<> "e")
inflAdj Dat _   _  Weak = onTF (<> "en")
inflAdj Gen _   _  Weak = onTF (<> "en")

nToT :: Case -> Number -> N -> T
nToT _   Sing n = T (nF n) (nE n)
nToT Dat Plur n = T (nFDatPlur n) (nEPlur n)
nToT _   Plur n = T (nFPlur n) (nEPlur n)

gebenOf :: Person -> Number -> T
gebenOf P1 Sing = T "gebe"  "give"
gebenOf P1 Plur = T "geben" "give"
gebenOf P2 Sing = T "gibst" "give"
gebenOf P2 Plur = T "gebt"  "give"
gebenOf P3 Sing = T "gibt"  "gives"
gebenOf P3 Plur = T "geben" "give"

doSent :: [Str] -> IO ()
doSent = DTI.putStrLn . (<> ".") . strOnHead toUpper . DT.unwords

genNP :: Case -> IO [T]
genNP cas = do
    noun <- sel nouns
    nounNum <- sel [Sing, Plur]
    adj <- sel adjsNormal
    inflType <- sel [Strong, Weak, Mixed]
    inside <- case inflType of
      Strong -> return [T "etwas" "some"]
      Mixed -> do
          pp <- sel persPron
          return [inflPP  cas (nG noun) nounNum pp]
      Weak -> return [derOf cas (nG noun) nounNum]
    return $ inside ++
        [ inflAdj cas (nG noun) nounNum inflType adj
        , nToT cas nounNum noun
        ] ++ if cas == Nom then [gebenOf P3 nounNum] else []

main :: IO ()
main = do
    npSubj <- genNP Nom
    npIndObj <- genNP Dat
    npObj <- genNP Acc
    let s = npSubj ++ npIndObj ++ npObj
    doSent $ map tE s
    _ <- getLine
    doSent $ map tF s
