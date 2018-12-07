#include <h>

-- Create a map from simplified versions of Mandarin words to their pinyin
-- and a hopefully short gloss for them, derived from Cedict.

data CedictEntry = CedictEntry
  { cPinyin :: Text
  , cGloss :: Text
  }

type Cedict = HashMap Text CedictEntry

parseCedictEntry :: Text -> (Text, CedictEntry)
parseCedictEntry l = (simp, CedictEntry pinyin gloss)
  where
    simpTradPinyin:defsAndEmpty = T.splitOn "/" l
    simp = T.takeWhile (/= ' ') simpTradPinyin
    pinyin = T.init $ T.init $ T.tail $ T.dropWhile (/= '[') simpTradPinyin
    defs = init defsAndEmpty
    gloss = minimumBy (compare `on` T.length) defs

loadCedictGlosses :: IO Cedict
loadCedictGlosses = do
    homeDir <- getHomeDirectory
    ls <- filter ((/= "#") . T.take 1) . T.lines <$>
        run ("xzcat", homeDir </> "data" </> "cedict" </> "cedict.txt.xz")
    return $ HM.fromList $ map parseCedictEntry ls
