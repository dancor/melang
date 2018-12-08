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
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import HSH
import System.Directory
import System.FilePath

data CedictEntry = CedictEntry
  { cPinyin :: Text
  , cGloss :: Text
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

parseCedictEntry :: Text -> (Text, CedictEntry)
parseCedictEntry l = (simp, CedictEntry pinyin gloss)
  where
    tradSimpPinyin:defsAndEmpty = T.splitOn "/" l
    simp = T.takeWhile (/= ' ') $ T.tail $ T.dropWhile (/= ' ') tradSimpPinyin
    pinyin = T.init $ T.init $ T.tail $ T.dropWhile (/= '[') tradSimpPinyin
    defs = init defsAndEmpty
    gloss = dotDotAfterWord (11 * T.length simp) $
        minimumBy (compare `on` T.length) glosses
    glosses = whenMany (filter (not . ("see " `T.isPrefixOf`))) $
        filter (not . ("CL:" `T.isPrefixOf`)) defs

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
