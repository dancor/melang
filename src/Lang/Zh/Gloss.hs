#include <hl>

module Lang.Zh.Gloss where

#include <hi>

type GlossMap = HashMap Text Text

readCedictGloss :: Text -> (Text, Text)
readCedictGloss t =
    if null slashParts || null toMinOver
    then error $ "readCedictGloss: " ++ T.unpack t
    else (simplifiedChinese, gloss)
  where
    _:simplifiedChinese:_ = T.words t
    slashParts = T.splitOn "/" t
    _:defs = slashParts
    toMinOver =
        map (\x -> if "to " `T.isPrefixOf` x then T.drop 3 x else x) .
        map (last . T.splitOn ") ") $
        map (head . T.splitOn " (") $
        filter (not . (== "\r")) $
        filter (not . ("CL:" `T.isPrefixOf`)) defs
    gloss = T.replace " " "-" $ minimumBy (compare `on` T.length) toMinOver

loadGlossMap :: IO GlossMap
loadGlossMap = do
    glossPairs <- map readCedictGloss .
        filter (not . ("#" `T.isPrefixOf`)) . T.lines <$> T.readFile
        "/home/danl/p/l/melang/lang/zh/cedict/cedict_1_0_ts_utf-8_mdbg.txt"
    return $ HM.fromList $ glossPairs ++
        [ ("了", "le")
        , ("的", "de")
        , ("个", "ge")
        , ("是", "is")
        , ("好", "good")
        , ("。", ".")
        , ("，", ",")
        , ("！", "!")
        ]
