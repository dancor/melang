{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Char
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.FilePath

import Cmn.Dict

type Misses = [DT.Text]

-- | Remove any prefix like "#1:", "#2:", ..
killNum :: DT.Text -> DT.Text
killNum x
  | "#" `DT.isPrefixOf` x =
    DT.dropWhile (== ':') . DT.dropWhile isDigit $ DT.drop 1 x
  | otherwise = x

kiloDeckDir :: FilePath
kiloDeckDir = "/home/danl/p/l/melang/data/cmn/kilo-deck"

intToDeckName :: Int -> FilePath
intToDeckName n = "mando-gloss-" ++ show n ++ "k.txt"

prepSubEntry :: WdDict -> DT.Text -> Writer Misses DT.Text
prepSubEntry wdDict word = (("<br>" <> word <> " ") <>) <$>
    case HMS.lookup word wdDict of
      Nothing -> tell [word] >> return "?"
      Just entry -> do
        let gloss = eGloss entry
        when (glossIsEmpty gloss) $ tell [word]
        return $ DT.intercalate " " [ePinyin entry, gloss,
            DT.replace "ADP" "PREP" (eSpParts entry), eSpPartFreqs entry]

prepEntry :: WdDict -> DictEntry -> Writer Misses DT.Text
prepEntry wdDict entry = do
    let word = eWord entry
    subEntries <- if DT.length word == 1
      then return ""
      else do
        DT.concat <$> mapM (prepSubEntry wdDict . DT.singleton)
            (DT.unpack word)
    return $
        eWord entry <> "\t" <> ePinyin entry <> "\t" <>
        eGloss entry <> "\t" <>
        DT.replace "ADP" "PREP" (eSpParts entry) <> " " <>
        eSpPartFreqs entry <> subEntries

writeKiloDeck :: FilePath -> WdDict -> Dict -> WriterT Misses IO ()
writeKiloDeck f wdDict dict = do
    let (res, misses) = runWriter $ mapM (prepEntry wdDict) dict
    tell misses
    lift . DTI.writeFile f $ DT.unlines res

writeKiloDecks :: FilePath -> WdDict -> Dict -> WriterT Misses IO ()
writeKiloDecks dir wdDict = zipWithM_ (\n ->
    writeKiloDeck (dir </> intToDeckName n) wdDict) [1..] . chunksOf 1000

prefNum :: Int -> DT.Text
prefNum n = DT.pack ('#' : show n ++ ":")

deDupePinyins :: Dict -> Dict
deDupePinyins = reverse . snd . foldl' f (Map.empty, [])
  where
    f (!seen, !dict) entry =
        ( Map.insertWith (+) pinyin (1 :: Int) seen
        , entry':dict
        )
      where
        pinyin = ePinyin entry
        entry' = if pinyin == "?" then entry else
            case Map.lookup pinyin seen of
              Just n -> entry {ePinyin = prefNum (n + 1) <> pinyin}
              _ -> entry

glossIsEmpty :: DT.Text -> Bool
glossIsEmpty x = x == "?" || ":" `DT.isSuffixOf` x

deDupeGlosses :: Dict -> Dict
deDupeGlosses = reverse . snd . foldl' f (Map.empty, [])
  where
    f (!seen, !dict) entry =
        ( Map.insertWith (+) (DT.takeWhile (/= '\\') gloss) (1 :: Int) seen
        , entry':dict
        )
      where
        gloss = eGloss entry
        entry' = if glossIsEmpty gloss then entry else
            case Map.lookup gloss seen of
              Just n -> entry {eGloss = prefNum (n + 1) <> gloss}
              _ -> entry

entryKillNums :: DictEntry -> DictEntry
entryKillNums entry = entry
    { eGloss = killNum $ eGloss entry
    , ePinyin = killNum $ ePinyin entry
    }

main :: IO ()
main = do
    oldDict <- loadDict
    let dict = deDupeGlosses . deDupePinyins $ map entryKillNums oldDict
        wdDict = dictToWdDict dict
    writeDict dict
    ((), misses) <- runWriterT . writeKiloDecks kiloDeckDir wdDict $
        takeWhile (not . glossIsEmpty . eGloss) dict
    DTI.putStr . DT.unlines . nub $ reverse misses
