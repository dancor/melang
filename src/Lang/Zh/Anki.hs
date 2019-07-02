#include <hl>

module Lang.Zh.Anki where

#include <hi>

data PronDef = PronDef
  { pSylls :: ![Text]
  , pDef   :: !Text
  } deriving (Eq, Show)

data ZNote = ZNote
  { zWord     :: !Text
  , zPronDefs :: ![PronDef]
  , zParts    :: ![Text] 
  , zMem      :: !Text
  , zHtml     :: !Text  -- extra html part (example sentences)
  } deriving (Eq, Show)
        
procPartsT :: Text -> [Text]
procPartsT = filter (not . T.null) . T.splitOn "<br>" .
    T.replace "<br />" "<br>" .
    T.replace "</div>" "<br>" .
    T.replace "<div>" "<br>"

preQSpIfB :: Text -> Text
preQSpIfB t = if "[" `T.isPrefixOf` t then "? " <> t else t

pronDefToTexts :: PronDef -> (Text, Text)
pronDefToTexts (PronDef sylls def) = (T.concat sylls, def)

bracketOnly :: Text -> Text
bracketOnly t = t3
  where
    t2 = T.dropWhile (/= '[') t
    t3 = if T.null t2 then "?" else t2

dbCmd q = ("sqlite3" :: String,
    ["/home/danl/.local/share/Anki2/Usuario 1/collection.anki2", q])

bsSplit :: Text -> [Text]
bsSplit =
    concatMap (T.splitOn "&nbsp;\\&nbsp;") .
    concatMap (T.splitOn " \\&nbsp;") .
    concatMap (T.splitOn "&nbsp;\\ ") .
    T.splitOn " \\ "

-- Normalizes but doesn't do toLower since we might want caps sometimes.
pinyinToSylls :: Text -> [Text]
pinyinToSylls s
  | T.null s = []
  | otherwise =
    if T.null py1
      then []
      else (py1 <> py2) : pinyinToSylls sRest
    where
      (py1, s2) = T.span isAlpha $ T.replace " " "" $ T.replace "'" "" s
      (py2, sRest) = T.span isDigit s2

textToNote :: Text -> Either Text ZNote
textToNote t = if length cols /= 6 then error (show cols) else
    if length syllses == length defs &&
       length syllses >= 1 &&
       syllNum >= length parts
      then
        Right $ ZNote word (zipWith PronDef syllses defs) parts3 mem html
      else
        Left $ "WTF["
            <> T.pack (show $ length syllses) <> ","
            <> T.pack (show $ length defs) <> ","
            <> T.pack (show $ map length (take 1 syllses)) <> ",{"
            <> T.intercalate ";" (map (T.intercalate ",") syllses) <> "},"
            <> T.pack (show $ length parts) <> "]: " <> t
  where
    cols = T.split (== '\US') t
    [word, pinyinsT, defsT, partsT, mem, html] = cols
    syllses = map pinyinToSylls $ bsSplit pinyinsT
    defs = bsSplit defsT
    parts = procPartsT partsT
    syllNum = length (head syllses)
    parts2 = take syllNum $ parts ++ repeat "?"
    parts3 = if syllNum == 1 then map preQSpIfB parts2 else parts2

noteToText :: ZNote -> Text
noteToText = noteToTextSep "\US"

noteToTextSep :: Text -> ZNote -> Text
noteToTextSep sep (ZNote word pronDefs parts mem html) = T.intercalate sep
    [word, pinyinsT, defsT, partsT, mem, html]
  where
    (pinyins, defs) = unzip $ map pronDefToTexts pronDefs
    pinyinsT = T.intercalate " \\ " pinyins
    defsT = T.intercalate " \\ " defs
    parts2 = if length parts == 1
      then map bracketOnly parts
      else parts
    partsT = T.intercalate "<br>" parts2

dubSingQuote = T.replace "'" "''"

updateNote :: ZNote -> IO ()
updateNote z = do
    HSH.runIO $ dbCmd $ "update notes set flds = '" <>
        T.unpack (dubSingQuote (noteToText z)) <>
        "' where flds like '" <> T.unpack (dubSingQuote (zWord z)) <>
        "\US%' limit 1"
    return ()

hshRunText :: (String, [String]) -> IO [Text]
hshRunText p = T.lines . T.decodeUtf8 <$> run p

myFromRight (Right a) = a
myFromRight (Left e) = error $ T.unpack e

loadZhAnkiNotes :: IO [ZNote]
loadZhAnkiNotes = map (myFromRight . textToNote) <$>
    hshRunText (dbCmd "select flds from notes")
