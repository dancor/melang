{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Tuple.Strict as S

#include <h>

data PronDef = PronDef
  { pSylls :: ![DT.Text]
  , pDef   :: !DT.Text
  } deriving (Eq, Show)

data ZNote = ZNote
  { zWord     :: !DT.Text
  , zPronDefs :: ![PronDef]
  , zParts    :: ![DT.Text]
  , zMem      :: !DT.Text
  } deriving (Eq, Show)

bsSplit :: DT.Text -> [DT.Text]
bsSplit =
    concatMap (DT.splitOn "&nbsp;\\&nbsp;") .
    concatMap (DT.splitOn " \\&nbsp;") .
    concatMap (DT.splitOn "&nbsp;\\ ") .
    DT.splitOn " \\ "

procPartsT :: DT.Text -> [DT.Text]
procPartsT = filter (not . DT.null) . DT.splitOn "<br>" .
    DT.replace "<br />" "<br>" . DT.replace "</div>" "<br>" .
    DT.replace "<div>" ""

preQSpIfNotQ :: DT.Text -> DT.Text
preQSpIfNotQ t = if "?" `DT.isPrefixOf` t then t else "? " <> t

textToNote :: DT.Text -> Either DT.Text ZNote
textToNote t =
    if length syllses == length defs && 
       length syllses >= 1 &&
       syllNum >= length parts
      then
        Right $ ZNote word (zipWith PronDef syllses defs) parts3 mem
      else
        Left $ "WTF["
            <> DT.pack (show $ length syllses) <> ","
            <> DT.pack (show $ length defs) <> ","
            <> DT.pack (show $ map length (take 1 syllses)) <> ",{"
            <> DT.intercalate ";" (map (DT.intercalate ",") syllses) <> "},"
            <> DT.pack (show $ length parts) <> "]: " <> t
  where
    [word, pinyinsT, defsT, partsT, mem] = DT.split (== '\US') t
    syllses = map pinyinToSylls $ bsSplit pinyinsT
    defs = bsSplit defsT
    parts = procPartsT partsT
    syllNum = length (head syllses)
    parts2 = take syllNum $ parts ++ repeat "?"
    parts3 = if syllNum == 1 then map preQSpIfNotQ parts2 else parts2

pronDefToTexts :: PronDef -> (DT.Text, DT.Text)
pronDefToTexts (PronDef sylls def) = (DT.concat sylls, def)

noteToText :: ZNote -> DT.Text
noteToText (ZNote word pronDefs parts mem) = DT.intercalate "\US"
    [word, pinyinsT, defsT, partsT, mem]
  where
    (pinyins, defs) = unzip $ map pronDefToTexts pronDefs
    pinyinsT = DT.intercalate " \\ " pinyins
    defsT = DT.intercalate " \\ " defs
    parts2 = if length parts == 1 && "? " `DT.isInfixOf` head parts
      then map (DT.drop 2) parts
      else parts
    partsT = DT.intercalate "<br>" parts2

-- Normalizes but doesn't do toLower since we might want caps sometimes.
pinyinToSylls :: DT.Text -> [DT.Text]
pinyinToSylls s
  | DT.null s = []
  | otherwise =
    if DT.null py1
      then []
      else (py1 <> py2) : pinyinToSylls sRest
    where
      (py1, s2) = DT.span isAlpha $ DT.replace " " "" $ DT.replace "'" "" s
      (py2, sRest) = DT.span isDigit s2

dbCmd q = ("sqlite3",
    ["/home/danl/.local/share/Anki2/Usuario 1/collection.anki2", q])

hshRunText :: (String, [String]) -> IO [DT.Text]
hshRunText p = DT.lines . DTE.decodeUtf8 <$> HSH.run p

noteToMap :: ZNote -> HMS.HashMap (S.Pair Char DT.Text) DT.Text
noteToMap (ZNote word pronDefs parts _) = HMS.fromList $ zip
    (S.zip (DT.unpack word) (map DT.toLower $ concatMap pSylls pronDefs))
    parts

bestCharGloss g1 g2 = if DT.length g1 > DT.length g2 then g1 else g2

dubSingQuote = DT.replace "'" "''"

updateNote :: ZNote -> IO ()
updateNote z = do
    DTI.putStrLn $ zWord z
    hshRun $ dbCmd $ "update notes set flds = '" <>
        DT.unpack (dubSingQuote (noteToText z)) <>
        "' where flds like '" <> DT.unpack (dubSingQuote (zWord z)) <>
        "\US%' limit 1"
    return ()

fromRight (Right a) = a

improveParts :: HMS.HashMap (S.Pair Char DT.Text) DT.Text -> ZNote -> ZNote
improveParts ziMap z@(ZNote word pronDefs _ _) = z {zParts = newParts}
  where
    zis = DT.unpack word
    PronDef sylls _ = head pronDefs
    newParts = zipWith
        (\zi syll -> fromJust $ HMS.lookup (zi S.:!: DT.toLower syll) ziMap)
        zis sylls

compareShow z1 z2 = zipWithM_ compareShowPart (zParts z1) (zParts z2)

compareShowPart p1 p2 = when (p1 /= p2 && "&lt;" `DT.isInfixOf` p2) $
    -- DTI.putStrLn $ "Repl " <> p1 <> " ---> " <> p2
    DTI.putStrLn p2

main :: IO ()
main = do
    notes <- map (fromRight . textToNote) <$>
        hshRunText (dbCmd "select flds from notes")
    --mapM_ (\(Left e) -> DTI.putStrLn e) $ filter isLeft notes
    let ziMap = foldl' (HMS.unionWith bestCharGloss) HMS.empty $
            map noteToMap notes
        notes2 = map (improveParts ziMap) notes
    print $ length ziMap
    --zipWithM_ compareShow notes notes2
    mapM_ updateNote notes2
