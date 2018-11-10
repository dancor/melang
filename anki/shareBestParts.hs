-- Should we kill repeats from pron defs? Or too complicated?

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Tuple.Strict as S

#include <h>

data PronDef = PronDef
  { pSylls :: ![Text]
  , pDef   :: !Text
  } deriving (Eq, Show)

data ZNote = ZNote
  { zWord     :: !Text
  , zPronDefs :: ![PronDef]
  , zParts    :: ![Text]
  , zMem      :: !Text
  } deriving (Eq, Show)

type ZiMap = HMS.HashMap (S.Pair Char Text) Text

bsSplit :: Text -> [Text]
bsSplit =
    concatMap (T.splitOn "&nbsp;\\&nbsp;") .
    concatMap (T.splitOn " \\&nbsp;") .
    concatMap (T.splitOn "&nbsp;\\ ") .
    T.splitOn " \\ "

procPartsT :: Text -> [Text]
procPartsT = filter (not . T.null) . T.splitOn "<br>" .
    T.replace "<br />" "<br>" .
    T.replace "</div>" "<br>" .
    T.replace "<div>" "<br>"

preQSpIfB :: Text -> Text
preQSpIfB t = if "[" `T.isPrefixOf` t then "? " <> t else t

textToNote :: Text -> Either Text ZNote
textToNote t =
    if length syllses == length defs && 
       length syllses >= 1 &&
       syllNum >= length parts
      then
        Right $ ZNote word (zipWith PronDef syllses defs) parts3 mem
      else
        Left $ "WTF["
            <> T.pack (show $ length syllses) <> ","
            <> T.pack (show $ length defs) <> ","
            <> T.pack (show $ map length (take 1 syllses)) <> ",{"
            <> T.intercalate ";" (map (T.intercalate ",") syllses) <> "},"
            <> T.pack (show $ length parts) <> "]: " <> t
  where
    [word, pinyinsT, defsT, partsT, mem] = T.split (== '\US') t
    syllses = map pinyinToSylls $ bsSplit pinyinsT
    defs = bsSplit defsT
    parts = procPartsT partsT
    syllNum = length (head syllses)
    parts2 = take syllNum $ parts ++ repeat "?"
    parts3 = if syllNum == 1 then map preQSpIfB parts2 else parts2

pronDefToTexts :: PronDef -> (Text, Text)
pronDefToTexts (PronDef sylls def) = (T.concat sylls, def)

bracketOnly :: Text -> Text
bracketOnly t = t3
  where
    t2 = T.dropWhile (/= '[') t
    t3 = if T.null t2 then "?" else t2

noteToText :: ZNote -> Text
noteToText (ZNote word pronDefs parts mem) = T.intercalate "\US"
    [word, pinyinsT, defsT, partsT, mem]
  where
    (pinyins, defs) = unzip $ map pronDefToTexts pronDefs
    pinyinsT = T.intercalate " \\ " pinyins
    defsT = T.intercalate " \\ " defs
    parts2 = if length parts == 1
      then map bracketOnly parts
      else parts
    partsT = T.intercalate "<br>" parts2

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

dbCmd q = ("sqlite3" :: String,
    ["/home/danl/.local/share/Anki2/Usuario 1/collection.anki2", q])

hshRunText :: (String, [String]) -> IO [Text]
hshRunText p = T.lines . T.decodeUtf8 <$> run p

noteToMap :: ZNote -> ZiMap
noteToMap (ZNote word pronDefs parts _) = HMS.fromList $ zip
    (S.zip (T.unpack word) (map T.toLower $ concatMap pSylls pronDefs))
    parts

bestCharGloss g1 g2 = if T.length g1 > T.length g2 then g1 else g2

dubSingQuote = T.replace "'" "''"

updateNote :: ZNote -> IO ()
updateNote z = do
    runIO $ dbCmd $ "update notes set flds = '" <>
        T.unpack (dubSingQuote (noteToText z)) <>
        "' where flds like '" <> T.unpack (dubSingQuote (zWord z)) <>
        "\US%' limit 1"
    return ()

fromRight (Right a) = a
fromRight (Left e) = error $ T.unpack e

improveParts :: ZiMap -> ZNote -> ZNote
improveParts ziMap z@(ZNote word pronDefs _ _) = z {zParts = newParts}
  where
    zis = T.unpack word
    PronDef sylls _ = head pronDefs
    newParts = zipWith
        (\zi syll -> fromJust $ HMS.lookup (zi S.:!: T.toLower syll) ziMap)
        zis sylls

tryImprove :: Bool -> ZiMap -> ZNote -> IO ()
tryImprove saveChanges ziMap note = do
    let note2 = improveParts ziMap note
        text = noteToText note
        text2 = noteToText note2
    when (note /= note2 && text /= text2) $ do
        T.putStrLn $ noteToText note
        T.putStrLn " ---> "
        T.putStrLn $ noteToText note2
        T.putStrLn ""
        when saveChanges $ updateNote note2

main :: IO ()
main = do
    args <- getArgs
    saveChanges <- case args of
        [] -> return False
        ["--dry-run"] -> return False
        ["--save-changes"] -> return True
        _ -> fail "Usage"
    ls <- hshRunText (dbCmd "select flds from notes")
    let notes = map (fromRight . textToNote) ls
    let ziMap = foldl' (HMS.unionWith bestCharGloss) HMS.empty $
            map noteToMap notes
    mapM_ (tryImprove saveChanges ziMap) notes
