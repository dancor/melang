{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Attoparsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Maybe
import qualified Data.Tree as Rose
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.Vector as Vec
import Prelude hiding (catch)
import System.IO
import System.IO.Error hiding (catch)

hReadLines :: Handle -> IO [BS.ByteString]
hReadLines h =
    liftM2 (:) (BS.hGetLine h) (hReadLines h)
    `catch`
    (\e -> if isEOFError e then return [] else ioError e)

parseLine :: BS.ByteString -> HMS.HashMap DT.Text Value
parseLine l = result
  where
    Done _ (Object result) = parse json l

data Tree a
    = Leaf a | Tree [Tree a]
    deriving (Show)

tree :: (a -> b) -> ([Tree a] -> b) -> Tree a -> b
tree f _ (Leaf x) = f x
tree _ g (Tree x) = g x

fromLeaf :: Show a => Tree a -> a
fromLeaf = tree id (error . ("fromLeaf: " ++) . show)

asLeaf :: Tree a -> Maybe a
asLeaf = tree Just (const Nothing)

asKids :: Tree a -> [Tree a]
asKids = tree (const []) id

valToTree :: Value -> Tree (Maybe DT.Text)
valToTree Null = Leaf Nothing
valToTree (String x) = Leaf $ Just x
valToTree (Array x) = Tree . Vec.toList $ Vec.map valToTree x
valToTree x = error $ "valToTree: " ++ show x

-- When a parent tree is present, it becomes a Right.
-- Other inputs become Lefts, in a natural way.
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

showDefLine :: DefLine -> (DT.Text, DT.Text, DT.Text)
showDefLine (DefLine zh py tr) =
    (zh, tcText . tcConcat $ TCSemi py : pyExtra, tcText $ tcConcat def)
  where
    (pyExtra, def) = unzip $ map showDefNode (Rose.flatten tr)

-- | Text which needs padding from continued text, only if more text is
-- present.
data TextCont
    = TCWord 
    { tcText :: DT.Text
    }
    | TCSemi
    { tcText :: DT.Text
    }
    | TCNone
    { tcText :: DT.Text
    }

tcAppend :: TextCont -> TextCont -> TextCont
tcAppend tc (TCNone t) = tc {tcText = DT.concat [tcText tc, t]}
tcAppend (TCWord t) tc = tc {tcText = DT.concat [t, " ", tcText tc]}
tcAppend (TCSemi t) tc = tc {tcText = DT.concat [t, "; ", tcText tc]}
tcAppend (TCNone t) tc = tc {tcText = DT.concat [t, tcText tc]}

tcConcat :: [TextCont] -> TextCont
tcConcat [] = TCNone ""
tcConcat l = foldl1' tcAppend l

showDefNode :: DefNode -> (TextCont, TextCont)
showDefNode dn = (pyExtra, def)
  where
    pyExtra = tcConcat . catMaybes $
        [ (\s -> TCSemi $ DT.concat ["(M: ", s, ")"]) <$> dnMeasureWord dn
        , (\s -> TCSemi $ DT.concat ["(CONS: ", s, 
          maybe "" (\s2 -> DT.concat [": ", s2]) $ dnUsage dn,
          ")"]) <$> dnConstruction dn
        ]
    def = tcConcat $ catMaybes
        [ (\s -> TCWord $ DT.concat [DT.toUpper $ DT.replace "." "" s, ":"])
          <$> dnSpeechPart dn
        , (\s -> TCWord $ DT.concat ["<", s, ">"]) <$> dnField dn
        , (\s -> TCSemi $ DT.concat ["", DT.replace ";" "," s]) <$> dnDefn dn
        , (const $ TCSemi "(M?)") <$> dnMeasureWord dn
        , maybe
          ((\s -> TCSemi $ DT.concat ["(", s, ")"]) <$> dnUsage dn)
          (const . Just $ TCSemi "(CONS?)") $ dnConstruction dn
        ]
    -- , (\s -> DT.concat ["ExZ: ", s]) <$> dnExampleZh dn
    -- , (\s -> DT.concat ["ExT: ", s]) <$> dnExampleTranslation dn

nothToTreeNoth :: Maybe (Tree (Maybe a)) -> Tree (Maybe a)
nothToTreeNoth = maybe (Leaf Nothing) id

assembleDefLine :: Int -> HMS.HashMap DT.Text Value -> DefLine
assembleDefLine _n m =
    DefLine
        (sPart "zh")
        (sPart "key") $
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

coverUncons :: [Maybe a] -> (Maybe a, [Maybe a])
coverUncons (x:r) = (x, r)
coverUncons _ = (Nothing, [])

coverZipWith8
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> Maybe g -> Maybe h
       -> i)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [Maybe f] -> [Maybe g] -> [Maybe h]
    -> [i]
coverZipWith8 f [] [] [] [] [] [] [] l8 =
    map (f Nothing Nothing Nothing Nothing Nothing Nothing Nothing) l8
coverZipWith8 f [] [] [] [] [] [] l7 l8 =
    coverZipWith (f Nothing Nothing Nothing Nothing Nothing Nothing) l7 l8
coverZipWith8 f [] [] [] [] [] l6 l7 l8 =
    coverZipWith3 (f Nothing Nothing Nothing Nothing Nothing) l6 l7 l8
coverZipWith8 f [] [] [] [] l5 l6 l7 l8 =
    coverZipWith4 (f Nothing Nothing Nothing Nothing) l5 l6 l7 l8
coverZipWith8 f [] [] [] l4 l5 l6 l7 l8 =
    coverZipWith5 (f Nothing Nothing Nothing) l4 l5 l6 l7 l8
coverZipWith8 f [] [] l3 l4 l5 l6 l7 l8 =
    coverZipWith6 (f Nothing Nothing) l3 l4 l5 l6 l7 l8
coverZipWith8 f [] l2 l3 l4 l5 l6 l7 l8 =
    coverZipWith7 (f Nothing) l2 l3 l4 l5 l6 l7 l8
coverZipWith8 f (x1:r1) l2 l3 l4 l5 l6 l7 l8 =
    f x1 x2 x3 x4 x5 x6 x7 x8 :
    coverZipWith8 f r1 r2 r3 r4 r5 r6 r7 r8
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5
    (x6, r6) = coverUncons l6
    (x7, r7) = coverUncons l7
    (x8, r8) = coverUncons l8

coverZipWith7
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> Maybe g -> h)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [Maybe f] -> [Maybe g] -> [h]
coverZipWith7 f [] [] [] [] [] [] l7 =
    map (f Nothing Nothing Nothing Nothing Nothing Nothing) l7
coverZipWith7 f [] [] [] [] [] l6 l7 =
    coverZipWith (f Nothing Nothing Nothing Nothing Nothing) l6 l7
coverZipWith7 f [] [] [] [] l5 l6 l7 =
    coverZipWith3 (f Nothing Nothing Nothing Nothing) l5 l6 l7
coverZipWith7 f [] [] [] l4 l5 l6 l7 =
    coverZipWith4 (f Nothing Nothing Nothing) l4 l5 l6 l7
coverZipWith7 f [] [] l3 l4 l5 l6 l7 =
    coverZipWith5 (f Nothing Nothing) l3 l4 l5 l6 l7
coverZipWith7 f [] l2 l3 l4 l5 l6 l7 =
    coverZipWith6 (f Nothing) l2 l3 l4 l5 l6 l7
coverZipWith7 f (x1:r1) l2 l3 l4 l5 l6 l7 =
    f x1 x2 x3 x4 x5 x6 x7 :
    coverZipWith7 f r1 r2 r3 r4 r5 r6 r7
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5
    (x6, r6) = coverUncons l6
    (x7, r7) = coverUncons l7

coverZipWith6
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> Maybe f -> g)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [Maybe f] -> [g]
coverZipWith6 f [] [] [] [] [] l6 =
    map (f Nothing Nothing Nothing Nothing Nothing) l6
coverZipWith6 f [] [] [] [] l5 l6 =
    coverZipWith (f Nothing Nothing Nothing Nothing) l5 l6
coverZipWith6 f [] [] [] l4 l5 l6 =
    coverZipWith3 (f Nothing Nothing Nothing) l4 l5 l6
coverZipWith6 f [] [] l3 l4 l5 l6 =
    coverZipWith4 (f Nothing Nothing) l3 l4 l5 l6
coverZipWith6 f [] l2 l3 l4 l5 l6 =
    coverZipWith5 (f Nothing) l2 l3 l4 l5 l6
coverZipWith6 f (x1:r1) l2 l3 l4 l5 l6 =
    f x1 x2 x3 x4 x5 x6 :
    coverZipWith6 f r1 r2 r3 r4 r5 r6
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5
    (x6, r6) = coverUncons l6

coverZipWith5
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d
       -> Maybe e -> f)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d]
    -> [Maybe e] -> [f]
coverZipWith5 f [] [] [] [] l5 =
    map (f Nothing Nothing Nothing Nothing) l5
coverZipWith5 f [] [] [] l4 l5 =
    coverZipWith (f Nothing Nothing Nothing) l4 l5
coverZipWith5 f [] [] l3 l4 l5 =
    coverZipWith3 (f Nothing Nothing) l3 l4 l5
coverZipWith5 f [] l2 l3 l4 l5 =
    coverZipWith4 (f Nothing) l2 l3 l4 l5
coverZipWith5 f (x1:r1) l2 l3 l4 l5 =
    f x1 x2 x3 x4 x5 :
    coverZipWith5 f r1 r2 r3 r4 r5
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4
    (x5, r5) = coverUncons l5

coverZipWith4
    :: (Maybe a -> Maybe b -> Maybe c -> Maybe d -> e)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [Maybe d] -> [e]
coverZipWith4 f [] [] [] l4 =
    map (f Nothing Nothing Nothing) l4
coverZipWith4 f [] [] l3 l4 =
    coverZipWith (f Nothing Nothing) l3 l4
coverZipWith4 f [] l2 l3 l4 =
    coverZipWith3 (f Nothing) l2 l3 l4
coverZipWith4 f (x1:r1) l2 l3 l4 =
    f x1 x2 x3 x4 :
    coverZipWith4 f r1 r2 r3 r4
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3
    (x4, r4) = coverUncons l4

coverZipWith3
    :: (Maybe a -> Maybe b -> Maybe c -> d)
    -> [Maybe a] -> [Maybe b] -> [Maybe c] -> [d]
coverZipWith3 f [] [] l3 =
    map (f Nothing Nothing) l3
coverZipWith3 f [] l2 l3 =
    coverZipWith (f Nothing) l2 l3
coverZipWith3 f (x1:r1) l2 l3 =
    f x1 x2 x3 :
    coverZipWith3 f r1 r2 r3
  where
    (x2, r2) = coverUncons l2
    (x3, r3) = coverUncons l3

coverZipWith
    :: (Maybe a -> Maybe b -> c)
    -> [Maybe a] -> [Maybe b] -> [c]
coverZipWith f [] l2 =
    map (f Nothing) l2
coverZipWith f (x1:r1) l2 =
    f x1 x2 :
    coverZipWith f r1 r2
  where
    (x2, r2) = coverUncons l2

main :: IO ()
main = do
    ls <- map parseLine <$> hReadLines stdin
    let res = map (showZhPyDef . showDefLine) $
            zipWith assembleDefLine [1..] ls
        showZhPyDef (zh, py, def) = DT.unwords [zh, py, def]
    mapM_ DTI.putStrLn res
