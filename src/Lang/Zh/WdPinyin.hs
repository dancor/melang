{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Zh.WdPinyin where

import Codec.Serialise
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import GHC.Generics
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.List.Split as Spl
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
import System.Process

data WdPinyin = WdPinyin !Text !Text deriving (Generic)

instance Serialise WdPinyin

wdPinyinGlossToAeson :: WdPinyinGloss -> Ae.Value
wdPinyinGlossToAeson (WdPinyinGloss wd pinyin gloss) =
    Ae.Array $ V.fromList $ map Ae.String [wd, pinyin, gloss]

wdPinyinGlossesToAeson :: [WdPinyinGloss] -> Ae.Value
wdPinyinGlossesToAeson = Ae.Array . V.fromList . map wdPinyinGlossToAeson

data WdPinyinGloss = WdPinyinGloss !Text !Text !Text

wdPinyinAddGloss :: HM.HashMap Text Text -> WdPinyin -> WdPinyinGloss 
wdPinyinAddGloss glossMap (WdPinyin wd py) = WdPinyinGloss wd py
    (fromMaybe wd $ HM.lookup wd glossMap)

pyPullNum :: String -> String -> Int -> (String, Int)
pyPullNum acc [] n = (acc, n)
pyPullNum acc ('ā':xs) _ = pyPullNum (acc ++ ['a']) xs 1
pyPullNum acc ('á':xs) _ = pyPullNum (acc ++ ['a']) xs 2
pyPullNum acc ('ǎ':xs) _ = pyPullNum (acc ++ ['a']) xs 3
pyPullNum acc ('à':xs) _ = pyPullNum (acc ++ ['a']) xs 4
pyPullNum acc ('ē':xs) _ = pyPullNum (acc ++ ['e']) xs 1
pyPullNum acc ('é':xs) _ = pyPullNum (acc ++ ['e']) xs 2
pyPullNum acc ('ě':xs) _ = pyPullNum (acc ++ ['e']) xs 3
pyPullNum acc ('è':xs) _ = pyPullNum (acc ++ ['e']) xs 4
pyPullNum acc ('ī':xs) _ = pyPullNum (acc ++ ['i']) xs 1
pyPullNum acc ('í':xs) _ = pyPullNum (acc ++ ['i']) xs 2
pyPullNum acc ('ǐ':xs) _ = pyPullNum (acc ++ ['i']) xs 3
pyPullNum acc ('ì':xs) _ = pyPullNum (acc ++ ['i']) xs 4
pyPullNum acc ('ō':xs) _ = pyPullNum (acc ++ ['o']) xs 1
pyPullNum acc ('ó':xs) _ = pyPullNum (acc ++ ['o']) xs 2
pyPullNum acc ('ǒ':xs) _ = pyPullNum (acc ++ ['o']) xs 3
pyPullNum acc ('ò':xs) _ = pyPullNum (acc ++ ['o']) xs 4
pyPullNum acc ('ū':xs) _ = pyPullNum (acc ++ ['u']) xs 1
pyPullNum acc ('ú':xs) _ = pyPullNum (acc ++ ['u']) xs 2
pyPullNum acc ('ǔ':xs) _ = pyPullNum (acc ++ ['u']) xs 3
pyPullNum acc ('ù':xs) _ = pyPullNum (acc ++ ['u']) xs 4
pyPullNum acc ('ǖ':xs) _ = pyPullNum (acc ++ ['v']) xs 1
pyPullNum acc ('ǘ':xs) _ = pyPullNum (acc ++ ['v']) xs 2
pyPullNum acc ('ǚ':xs) _ = pyPullNum (acc ++ ['v']) xs 3
pyPullNum acc ('ǜ':xs) _ = pyPullNum (acc ++ ['v']) xs 4
pyPullNum acc ('ü':xs) n = pyPullNum (acc ++ ['v']) xs n
pyPullNum acc (x:xs) n = pyPullNum (acc ++ [x]) xs n

pyToNum :: String -> String
pyToNum syllable = if any isAlpha syllable
  then
    let (syllable', n) = pyPullNum "" syllable 5
    in  syllable' ++ show n
  else syllable

procSentJsLine :: String -> WdPinyin
procSentJsLine l = WdPinyin (T.pack wd)
    (T.pack $ intercalate "" $ map pyToNum pinyins)
  where
    wd:pinyins = words l

js :: [Text] -> BL.ByteString
js zhSentences = BL.concat [
    "var pinyinify = require('pinyinify');",
    "var sentences = " <> Ae.encode zhSentences <> ";",
    "for (var i = 0; i < sentences.length; i++) {",
    "  var sentence = sentences[i];",
    "  var res = pinyinify(sentence, true);",
    "  words = res.segments;",
    "  pinyins = res.pinyinSegmentsSyllables;",
    "  for (var j = 0; j < words.length; j++) {",
    "    console.log(words[j] + ' ' + pinyins[j].join(' '));",
    "  }",
    "  console.log('');",
    "}"]

getWdPinyins :: [Text] -> IO [[WdPinyin]]
getWdPinyins zhSentences = do
    setCurrentDirectory "/home/danl/p/one-off/www/hanyu/node_modules/pinyinify"
    (_, out, _err) <- readProcessWithExitCode "nodejs" []
        (BL.toString $ js zhSentences)
    return $ map (map procSentJsLine) $ Spl.splitWhen null $ lines out

