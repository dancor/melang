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

textToNote :: DT.Text -> Either DT.Text ZNote
textToNote t =
    if length syllses == length defs && 
       length syllses >= 1 &&
       length (head syllses) >= length parts
      then
        Right $ ZNote word (zipWith PronDef syllses defs) parts' mem
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
    parts' = take (length (head syllses)) $ parts ++ repeat "?"

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

procPartsT :: DT.Text -> [DT.Text]
procPartsT = filter (not . DT.null) . DT.splitOn "<br>" .
    DT.replace "<br />" "<br>" . DT.replace "</div>" "<br>" .
    DT.replace "<div>" ""

dbCmd q = ("sqlite3",
    ["/home/danl/.local/share/Anki2/Usuario 1/collection.anki2", q])

hshRunText :: (String, [String]) -> IO [DT.Text]
hshRunText p = DT.lines . DTE.decodeUtf8 <$> HSH.run p

noteToMap :: ZNote -> HMS.HashMap (S.Pair Char DT.Text) DT.Text
noteToMap (ZNote word pronDefs parts _) =
    HMS.fromList $
    zip (S.zip (DT.unpack word) (concatMap pSylls pronDefs)) parts

bestCharGloss g1 g2 = if DT.length g1 > DT.length g2 then g1 else g2

{-
wdSetCharGlosses
    :: DT.Text -> HMS.HashMap (S.Pair Char DT.Text) (DT.Text) -> IO ()
wdSetCharGlosses wd charGlossMap = do
    let whereStr = "where flds like '" ++ DT.unpack wd ++ "\US%'"
    [sqlLine] <- hshRunText $ dbCmd $
        "select flds from notes " ++ whereStr
    DTI.putStrLn sqlLine
    let fields = DT.split (== '\US') sqlLine
        _ : pyStr : def : oldCharGlossStrPlus = fields
    DTI.putStrLn $ "New str: " <> wd
    let Just ziPys = collateZiPy wd $
            DT.replace "'" "" $ DT.replace " " "" pyStr
        charGlosses = map (fromMaybe "?" . flip HMS.lookup charGlossMap) ziPys
        fldsStr = DT.intercalate "\US" $
            [wd, pyStr, def, DT.intercalate "<br>" charGlosses]
    when (oldCharGlossStrPlus == [""]) $ do
        DTI.putStrLn $ "Replacing " <> DT.pack (show oldCharGlossStrPlus) <>
            " with " <> fldsStr
        hshRun $ dbCmd $ "update notes set flds = \"" ++ DT.unpack fldsStr ++
            "\" " ++ whereStr ++ " limit 1"
        return ()
-}

fromRight (Right a) = a

improveParts :: HMS.HashMap (S.Pair Char DT.Text) DT.Text -> ZNote -> ZNote
improveParts ziMap z@(ZNote word pronDefs _ _) = z {zParts = newParts}
  where
    zis = DT.unpack word
    PronDef sylls _ = head pronDefs
    newParts = zipWith
        (\zi syll -> fromJust $ HMS.lookup (zi S.:!: syll) ziMap)
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
    zipWithM_ compareShow notes notes2
