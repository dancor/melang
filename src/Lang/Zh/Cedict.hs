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
  , cGloss :: !Text
  } deriving Show

type Cedict = HashMap Text CedictEntry

dotDotAfterWord :: Int -> Text -> Text
dotDotAfterWord n t =
    case go Nothing 0 0 of
      Nothing -> case T.words t of
        [w] -> w
        w:_ -> w <> ".."
        [] -> error "dotDotAfterWord: empty"
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

{-
-- Might want to look into doing this later
condenseMandarin :: Text -> Text
condenseMandarin = id
-}

whenMany :: ([a] -> [a]) -> [a] -> [a]
whenMany _ [] = []
whenMany _ [x] = [x]
whenMany f xs = f xs

{-
, ("好", "good")
-}
parseCedictEntry :: Text -> Maybe (Text, CedictEntry)
parseCedictEntry l = case (simp, pinyin) of
    ("了", "liao3") -> Nothing
    ("个", "ge4") -> Nothing
    ("好", _) -> Just (simp, CedictEntry pinyin "good")
    ("帮", _) -> Just (simp, CedictEntry pinyin "help")
    ("你", _) -> Just (simp, CedictEntry pinyin "you")
    ("没", _) -> Just (simp, CedictEntry "mei2" "not")
    ("小", _) -> Just (simp, CedictEntry pinyin "small")
    _ -> Just (simp, CedictEntry pinyin gloss)
  where
    tradSimpPinyin:defsAndEmpty = T.splitOn "/" l
    simp = T.takeWhile (/= ' ') $ T.tail $ T.dropWhile (/= ' ') tradSimpPinyin
    pinyin = T.filter (/= ' ') . T.dropEnd 2 . T.drop 1 $
        T.dropWhile (/= '[') tradSimpPinyin
    defs = case defsAndEmpty of
      [] -> error "parseCedictEntry: null defsAndEmpty"
      x -> init x
    glosses = whenMany (filter (not . ("see " `T.isPrefixOf`))) $
        filter (not . ("CL:" `T.isPrefixOf`)) defs
    gloss1 = if null glosses then "?" else
        dotDotAfterWord (11 * T.length simp) $
        minimumBy (compare `on` T.length) glosses
    gloss = T.replace " " "-" . fromMaybe gloss1 $ T.stripPrefix "to " gloss1

-- Later change this to use most common pinyin option from corpus or more
-- advanced analysis.
takeShorter :: CedictEntry -> CedictEntry -> CedictEntry
takeShorter a@(CedictEntry _ aG) b@(CedictEntry _ bG) =
    if T.length aG <= T.length bG then a else b

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
