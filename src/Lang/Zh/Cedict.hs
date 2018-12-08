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

data CedictEntry = CedictEntry
  { cPinyin :: Text
  , cGloss :: Text
  } deriving Show

type Cedict = HashMap Text CedictEntry

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
    tWithWidths = map (\c -> (c, wcwidth c)r $ T.unpack t
    cumulativeTWithWidths =
        scanl (\(t1,w1) (t2,w2) -> (t1 ++ [t2], w1 + w2)) ("", 0) tWithWidths
    tTrunc = T.pack . fst . lastNote "dotDotAfterWord tTrunc last failed" $
        filter ((<= n - 2) . snd) cumulativeTWithWidths
    tWidth = sum $ map snd tWithWidths

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
