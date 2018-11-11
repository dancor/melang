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
  , zHtml     :: !Text
  } deriving (Eq, Show)
        
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
noteToText (ZNote word pronDefs parts mem html) = T.intercalate "\US"
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
