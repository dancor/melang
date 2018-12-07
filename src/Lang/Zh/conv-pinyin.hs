#include <h>

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

conv l = w1 <> " " <> w2'
  where
    [w1, w2] = T.words l
    w2' = if "0" `T.isInfixOf` w2 then w2 else T.pack $ pyToNum $ T.unpack w2

main = T.interact $ T.unlines . map conv . T.lines
