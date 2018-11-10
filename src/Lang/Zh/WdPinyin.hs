#include <hl>

module Lang.Zh.WdPinyin where

#include <hi>

import Codec.Serialise

import Lang.Zh.Gloss

data WdPinyin = WdPinyin !DT.Text !DT.Text deriving (Generic)

instance Serialise WdPinyin

wdPinyinGlossToAeson (WdPinyinGloss wd pinyin gloss) =
    Ae.Array $ Vec.fromList $ map Ae.String [wd, pinyin, gloss]

wdPinyinGlossesToAeson = Ae.Array . Vec.fromList . map wdPinyinGlossToAeson

data WdPinyinGloss = WdPinyinGloss !DT.Text !DT.Text !DT.Text

wdPinyinAddGloss glossMap (WdPinyin wd py) = WdPinyinGloss wd py
    (fromMaybe wd $ HMS.lookup wd glossMap)

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

pyToNum syllable = if any isAlpha syllable
  then let (syllable', n) = pyPullNum "" syllable 5 in syllable' ++ show n
  else syllable

procSentJsLine l = WdPinyin (DT.pack wd)
    (DT.pack $ intercalate "" $ map pyToNum pinyins)
  where
    wd:pinyins = words l

js :: [DT.Text] -> BSL.ByteString
js zhSentences = BSL.concat [
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

getWdPinyins :: [DT.Text] -> IO [[WdPinyin]]
getWdPinyins zSents = do
    setCurrentDirectory "/home/danl/p/one-off/www/hanyu/node_modules/pinyinify"
    (_, out, _err) <- readProcessWithExitCode "nodejs" []
        (BSLU.toString $ js zhSentences)
    return $ map (map procSentJsLine) $ Spl.splitWhen null $ lines out

