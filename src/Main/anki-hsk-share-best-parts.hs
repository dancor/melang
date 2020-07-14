{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Tuple.Strict as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

import Lang.Zh.Anki

type ZiMap = HashMap (S.Pair Char Text) Text

noteToMap :: ZNote -> ZiMap
noteToMap (ZNote word _ pronDefs parts _ _) = HM.fromList $ zip
    (zipWith (S.:!:) (T.unpack word) 
        (map T.toLower $ concatMap pSylls pronDefs))
    parts

bestCharGloss g1 g2 = if T.length g1 > T.length g2 then g1 else g2

improveParts :: ZiMap -> ZNote -> ZNote
improveParts ziMap z@(ZNote word _ pronDefs _ _ _) = z {zParts = newParts}
  where
    zis = T.unpack word
    PronDef sylls _ = head pronDefs
    newParts = zipWith
        (\zi syll -> fromJust $ HM.lookup (zi S.:!: T.toLower syll) ziMap)
        zis sylls

tryImprove :: Bool -> ZiMap -> ZNote -> IO ()
tryImprove saveChanges ziMap note = do
    let note2 = improveParts ziMap note
        text = noteToText note
        text2 = noteToText note2
        noteDisp = T.intercalate "\n" . zParts
    when (note /= note2 && text /= text2) $ do
        T.putStrLn $ noteDisp note
        T.putStrLn " ---> "
        T.putStrLn $ noteDisp note2
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
    notes <- loadZhAnkiNotes
    let ziMap = foldl' (HM.unionWith bestCharGloss) HM.empty $
            map noteToMap notes
    mapM_ (tryImprove saveChanges ziMap) notes
