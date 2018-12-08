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
import qualified Data.List.Split as Spl
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import HSH
import Safe
import System.Directory
import System.FilePath
import Data.StrictVector (Vector)
import qualified Data.StrictVector as V

import Debug.Trace

data CedictEntry = CedictEntry
  { cPinyin :: Text
  , cGloss :: Text
  } deriving Show

type Cedict = HashMap Text CedictEntry

{-


type WidthMarkedStr = Vector (Char, Int)

-- Currently this calls isExon up to twice as much as it needs to?
findLastExonIndices :: (a -> Bool) -> Vector a -> [Int]
findLastExonIndices isExon v = go 0 where
  go i = case V !? i of
    Nothing -> []
    Just val -> if isExon val
      then case V !? (i + 1) of
        Nothing -> [i]
        Just val2 -> if isExon val2 then go (i + 1) else i : go (i + 1)
      else go (i + 1)

-- Given a vector with length > n, truncate it to length <= n,
-- truncating at the end of an isExon section.
--
-- When there are no isExon regions, just truncate to length n.
truncTryAfterExon :: Int -> (a -> Bool) -> Vector a -> Vector a
truncTryAfterExon n isExon v =
  case filter (< n) $ findLastExonIndexes isExon v of
    [] -> V.take n v
    res -> last res
-}

dotDotAfterWord :: Int -> Text -> Text
dotDotAfterWord n t =
    case go Nothing 0 0 of
      Nothing -> error "dotDotAfterWord did not find a solution"
      Just i -> if i + 1 < len then T.take (i + 1) t <> ".." else t
  where
    maxWidth = n
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

go :: Int -> Text -> Maybe Int -> Int -> Int -> Maybe Int
go maxWidth t lastGood widthSoFar i = if i >= T.length t || widthSoFar > maxWidth
  then lastGood
  else
    let c = t `T.index` i
        width = wcwidth c
        lastGood' =
          if not (isSpace c) &&
              (i + 1 >= T.length t || isSpace (t `T.index` (i + 1)))
            then Just i
            else lastGood
    in  go maxWidth t lastGood' (widthSoFar + width) (i + 1)
{-
  where

  if  | not (V.any isInter vTrunc) -> Just vTrunc
      | isInter (v ! (n - 2)) || isInter (v ! (n - 3)) ->
        T.dropWhileEnd isSpace tTrunc <> ".."
      | otherwise ->
        T.dropWhileEnd isSpace $ T.dropWhileEnd (not . isSpace) tTrunc <> ".."
  where


dotDotAfterWord :: Int -> Text -> Text
dotDotAfterWord n t =
  if
    | tWidth <= n -> t
    | not (T.any isSpace tTrunc) -> tTrunc <> ".."
    | isSpace (t `T.index` (n - 2)) || isSpace (t `T.index` (n - 3)) ->
      T.dropWhileEnd isSpace tTrunc <> ".."
    | otherwise ->
      T.dropWhileEnd isSpace $ T.dropWhileEnd (not . isSpace) tTrunc <> ".."
  where
    tWithWidths = map (\c -> (c, wcwidth c)) $ T.unpack t
    cumulativeTWithWidths =
        scanl (\(t1,w1) (t2,w2) -> (t1 ++ [t2], w1 + w2)) ("", 0) tWithWidths
    tTrunc = T.pack . fst . lastNote "dotDotAfterWord tTrunc last failed" $
        filter ((<= n - 2) . snd) cumulativeTWithWidths
    tWidth = sum $ map snd tWithWidths

-- If the text t has length greater than n, truncate and append ".." with
-- result length n or less, truncating at a word boundary if possible.
-- n must be more than 2.
-- 
-- TODO: Account for double-width characters.
dotDotAfterWord :: Int -> Text -> Text
dotDotAfterWord n t =
  if
    | tWidth <= n -> t
    | not (T.any isSpace tTrunc) -> tTrunc <> ".."
    | isSpace (t `T.index` (n - 2)) || isSpace (t `T.index` (n - 3)) ->
      T.dropWhileEnd isSpace tTrunc <> ".."
    | otherwise ->
      T.dropWhileEnd isSpace $ T.dropWhileEnd (not . isSpace) tTrunc <> ".."
  where
    tWithWidths = map (\c -> (c, wcwidth c)) $ T.unpack t
    cumulativeTWithWidths =
        scanl (\(t1,w1) (t2,w2) -> (t1 ++ [t2], w1 + w2)) ("", 0) tWithWidths
    tTrunc = T.pack . fst . lastNote "dotDotAfterWord tTrunc last failed" $
        filter ((<= n - 2) . snd) cumulativeTWithWidths
    tWidth = sum $ map snd tWithWidths
-}

condenseMandarin :: Text -> Text
condenseMandarin = id

-- TODO: Have the gloss truncation length dependent on the number of Mandarin
-- characters.
polishGloss :: Text -> Text
polishGloss = dotDotAfterWord 40

parseCedictEntry :: Text -> (Text, CedictEntry)
parseCedictEntry l = (simp, CedictEntry pinyin gloss)
  where
    tradSimpPinyin:defsAndEmpty = T.splitOn "/" l
    simp = T.takeWhile (/= ' ') $ T.tail $ T.dropWhile (/= ' ') tradSimpPinyin
    pinyin = T.init $ T.init $ T.tail $ T.dropWhile (/= '[') tradSimpPinyin
    defs = init defsAndEmpty
    gloss = polishGloss $ minimumBy (compare `on` T.length) defs

takeShorter :: CedictEntry -> CedictEntry -> CedictEntry
takeShorter a@(CedictEntry _ aG) b@(CedictEntry _ bG) =
    if T.length aG <= T.length bG then a else b

loadCedictGlossMap :: IO Cedict
loadCedictGlossMap = do
    homeDir <- getHomeDirectory
    ls <- filter ((/= "#") . T.take 1) . T.lines . T.decodeUtf8 <$>
        run ("xzcat" :: String,
        [homeDir </> "data" </> "cedict" </> "cedict.txt.xz"])
    return $ HM.fromListWith takeShorter $ map parseCedictEntry ls
