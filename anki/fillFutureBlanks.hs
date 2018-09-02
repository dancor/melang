{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Tuple.Strict as S

#include <h>

pinyinStrToPinyins :: DT.Text -> [DT.Text]
pinyinStrToPinyins s
  | DT.null s = []
  | otherwise =
    if DT.null py1
      then []
      else (DT.map toLower py1 <> py2) : pinyinStrToPinyins sRest
    where
      (py1, s2) = DT.span isAlpha s
      (py2, sRest) = DT.span isDigit s2

procGlossStr :: DT.Text -> DT.Text
procGlossStr = 
    DT.replace "<br />" "<br>" . 
    DT.replace "</div>" "" . 
    DT.replace "<div>" "<br>" . 
    DT.replace "<br /><div>" "<br>" . 
    DT.replace "<div><div>" "<br>"

doSqlLine :: DT.Text -> HMS.HashMap (S.Pair Char DT.Text) DT.Text
doSqlLine sqlLine = 
    if length fields == 4 && not (DT.null charGlossStr)
        && isJust ziPysMb
        && charsL == length charGlosses
      then HMS.fromList $ zip ziPys charGlosses
      else HMS.empty
  where
    fields = DT.split (== '\US') sqlLine
    [chars, pinyinStr, _def, charGlossStr] = fields
    charGlosses = DT.splitOn "<br>" $ procGlossStr charGlossStr
    charsL = DT.length chars
    ziPysMb = collateZiPy chars pinyinStr
    Just ziPys = ziPysMb

bestCharGloss g1 g2 = if DT.length g1 > DT.length g2 then g1 else g2

hshRunText :: (String, [String]) -> IO [DT.Text]
hshRunText p = DT.lines . DTE.decodeUtf8 <$> HSH.run p

collateZiPy :: DT.Text -> DT.Text -> Maybe [S.Pair Char DT.Text]
collateZiPy zis pyStr =
  let pys = pinyinStrToPinyins pyStr
  in  if DT.length zis == length pys
        then Just $ S.zip (DT.unpack zis) pys
        else Nothing

dbCmd q = ("sqlite3",
    ["/home/danl/.local/share/Anki2/Usuario 1/collection.anki2", q])

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

main :: IO ()
main = do
    sqlLines <- hshRunText $ dbCmd "select flds from notes"
    DTI.putStrLn $ last sqlLines
    let charGlossMap = foldl' (HMS.unionWith bestCharGloss) HMS.empty $
            map doSqlLine sqlLines
    --mapM_ print $ sortBy (comparing (S.snd . fst)) $ HMS.toList charGlossMap
    
    {-
    nextZiPys <- concatMap
        (fromJust . (\(zis:pys) -> 
            collateZiPy zis (DT.replace "'" "" $ DT.concat pys)) .
            DT.words) .
        DT.lines <$> DTI.readFile "next-words"
    mapM_ (\ziPy@(zi S.:!: py) -> 
        DTI.putStr $ DT.singleton zi <> " " <> py <> "\n" <> maybe "" (<> "\n")
        (HMS.lookup ziPy charGlossMap)) nextZiPys
    -}
    nextWords <- map (head . DT.words) . DT.lines <$> DTI.readFile "next-words"
    mapM_ (flip wdSetCharGlosses charGlossMap) nextWords
