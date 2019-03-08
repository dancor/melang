-- Create a map from simplified versions of Mandarin words to their pinyin
-- and a hopefully short gloss for them, derived from Cedict.

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Zh.Cedict where

import Data.Char
import Data.Char.WCWidth
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.Process

data CedictEntry = CedictEntry
  { cPinyin :: !Text
  , cGloss :: !CedictGloss
  } deriving Show

data CedictGloss = CedictGloss !Text | CedictRef !Text deriving Show

type Cedict = HashMap Text CedictEntry

-- type TextAndWidths = TextAndWidths !Text ![Int]

dotDotAfterWord :: Int -> Text -> Text
dotDotAfterWord maxWidth t =
    case go Nothing 0 0 of
      Nothing -> case T.words t of
        [w] -> w
        w:_ -> w <> ".."
        [] -> error "dotDotAfterWord: empty"
      Just i -> if i + 1 < len then T.take (i + 1) t <> ".." else t
  where
    len = T.length t
    -- Look for a longer solution until there are no more candidates;
    -- at that point fall back on the last solution.
    go :: Maybe Int -> Int -> Int -> Maybe Int
    go lastGood widthSoFar i =
      let c = t `T.index` i
          width = wcwidth c
          widthSoFar' = widthSoFar + width
      in if i >= len || widthSoFar' > maxWidth
      then lastGood
      else
        let lastGood' =
              if not (isSpace c) &&
                  (i + 1 >= len || isSpace (t `T.index` (i + 1)))
                then Just i
                else lastGood
        in  go lastGood' widthSoFar' (i + 1)

-- Convert things like:
-- "一點|一点" to "一点"
-- "一点[yi1 dian3]" to "一点 yi1dian3"
condenseZh :: Text -> Text
condenseZh = id
{-
condenseZh t = f s widths
  where
    s = T.unpack t
    widths = map wcwidth s
    f [] _ = []
    f [] _ = []
-}

whenMany :: ([a] -> [a]) -> [a] -> [a]
whenMany _ [] = []
whenMany _ [x] = [x]
whenMany f xs = f xs

-- Could perform the lookup when "see " is the only definition?
-- Same for "erhua variant of"
isBadDef d = any (`T.isPrefixOf` d)
  [ "CL:"
  , "also written"
  , "also pr"
  , "also transliterated"
  , "also known as "
  ]

checkRef :: Text -> Maybe Text
checkRef d = let
  refMb = catMaybes $ map ($ d)
      [ T.stripPrefix "see "
      , T.stripPrefix "erhua variant of "
      , T.stripPrefix "variant of "
      , T.stripPrefix "archaic variant of "
      ]
  in case refMb of
    rest:_ -> Just simp
      where
        tradMbSimp = T.takeWhile (/= '[') rest
        simp = if "|" `T.isInfixOf` tradMbSimp
          then T.dropEnd 1 $ T.dropWhileEnd  (/= '|') tradMbSimp
          else tradMbSimp
    [] -> Nothing

-- Could use width instead of length? Incorporate with condenseZh?
parseCedictEntry :: Text -> Maybe (Text, CedictEntry)
parseCedictEntry l = case (simp, pinyin) of
    ("个", "ge4") -> Nothing
    ("好", _) -> Just (simp, CedictEntry pinyin (CedictGloss "good"))
    ("帮", _) -> Just (simp, CedictEntry pinyin (CedictGloss "help"))
    ("你", _) -> Just (simp, CedictEntry pinyin (CedictGloss "you"))
    ("没", _) -> Just (simp, CedictEntry "mei2" (CedictGloss "not"))
    ("小", _) -> Just (simp, CedictEntry pinyin (CedictGloss "small"))
    ("了", _) -> Just (simp, CedictEntry "le5" (CedictGloss "le"))
    ("的", _) -> Just (simp, CedictEntry "de5" (CedictGloss "de"))
    ("他", _) -> Just (simp, CedictEntry "ta1" (CedictGloss "him"))
    ("她", _) -> Just (simp, CedictEntry "ta1" (CedictGloss "her"))
    ("我", _) -> Just (simp, CedictEntry "wo3" (CedictGloss "me"))
    ("要", _) -> Just (simp, CedictEntry "yao4" (CedictGloss "will"))
    _ -> Just (simp, CedictEntry pinyin gloss)
  where
    tradSimpPinyin:defsAndEmpty = T.splitOn "/" l
    simp = T.takeWhile (/= ' ') $ T.tail $ T.dropWhile (/= ' ') tradSimpPinyin
    pinyin = T.filter (/= ' ') . T.dropEnd 2 . T.drop 1 $
        T.dropWhile (/= '[') tradSimpPinyin
    defs = case defsAndEmpty of
      [] -> error "parseCedictEntry: null defsAndEmpty"
      x -> init x
    glosses = map (\x -> fromMaybe x $ T.stripPrefix "to "x) $
        filter (not . isBadDef) defs
    bestGloss = if null glosses then "?" else
        minimumBy (compare `on` T.length) glosses
    gloss = case checkRef bestGloss of
      Just ref -> CedictRef ref
      Nothing -> CedictGloss . T.replace " " "-" . condenseZh $
        dotDotAfterWord (11 * T.length simp) bestGloss

-- Later change this to use most common pinyin option from corpus or more
-- advanced analysis.
takeShorter :: CedictEntry -> CedictEntry -> CedictEntry
takeShorter (CedictEntry _ (CedictRef _)) b = b
takeShorter a (CedictEntry _ (CedictRef _)) = a
takeShorter a@(CedictEntry _ (CedictGloss aG))
        b@(CedictEntry _ (CedictGloss bG)) =
    if bG == "?" || T.length aG <= T.length bG then a else b

loadCedictGlosses :: IO [Text]
loadCedictGlosses = do
    homeDir <- getHomeDirectory
    (_, Just hOut, _, _) <- createProcess (proc "xzcat"
        [homeDir </> "data" </> "cedict" </> "cedict.txt.xz"])
        {std_out = CreatePipe}          
    filter ((/= "#") . T.take 1) . T.lines <$> T.hGetContents hOut

loadCedictGlossMap :: IO Cedict
loadCedictGlossMap = do
    homeDir <- getHomeDirectory
    (_, Just hOut, _, _) <- createProcess (proc "xzcat"
        [homeDir </> "data" </> "cedict" </> "cedict.txt.xz"])
        {std_out = CreatePipe}          
    ls <- filter ((/= "#") . T.take 1) . T.lines <$> T.hGetContents hOut
    return . HM.fromListWith takeShorter . catMaybes $ map parseCedictEntry ls
