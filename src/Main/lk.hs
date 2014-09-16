-- lookup scrabble words

import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process

data DictType = Scr | NaScr | WiktGer deriving Eq

data Opts = Opts
    { oWdsOnly :: Bool
    , oDict :: DictType
    }

defOpts :: Opts
defOpts = Opts False Scr

optList :: [OptDescr (Opts -> Opts)]
optList =
    [ Option "o" ["words-only"] (NoArg $ \o -> o {oWdsOnly = True})
        "Do not give definitions."
    , Option "d" ["wiktionary-german"] (NoArg $ \o -> o {oDict = WiktGer})
        "Use German entries from the English Wiktionary."
    {-
    , Option "g" ["enable"] (NoArg Twl)
        "Use the Enable word list."
    , Option "s" ["scrabble"] (NoArg Twl)
        "Use the international scrabble dictionary."
    -}
    , Option "n" ["north-american-scrabble"] (NoArg $ \o -> o {oDict = NaScr})
        "Use the smaller North American scrabble dictionary."
    ]

doReplacements :: (Eq a) => [(a, [a])] -> [a] -> [a]
doReplacements reps = concatMap $ \x -> case lookup x reps of
  Just y -> y
  Nothing -> [x]

lkR :: Int -> String -> IO ()
lkR depth line = do
    putStrLn $ replicate (3 * (depth - 1)) ' ' ++
        (if depth > 0 then "=> " else "") ++ line
    return ()

dictGrep :: Int -> String -> String -> [String] -> IO ()
dictGrep depth ptn dictF grepArgs = do
    (pIn, pOut, pErr, pId) <-
        runInteractiveProcess "grep" (ptn:dictF:grepArgs) Nothing Nothing
    hClose pIn
    cOut <- hGetContents pOut
    mapM_ (lkR depth) $ lines cOut
    cErr <- hGetContents pErr
    hPutStr stderr cErr
    _ <- waitForProcess pId
    return ()

wiktGrep :: Opts -> String -> String -> [String] -> IO ()
wiktGrep (Opts wdsOnly _) word dictF extraArgs = do
    let ptn = "^^" ++ word ++ "$"
        args = if wdsOnly
          then [ptn, dictF]
          else [ptn, "-m", "1", "-A", "10000", dictF]
    (_pIn, pOut, _pErr, _pId) <-
        runInteractiveProcess "grep" (args ++ extraArgs) Nothing Nothing
    cOut <- hGetContents pOut
    case lines cOut of
      ls@(l0:lRest) -> mapM_ putStrLn $ if wdsOnly
        then map tail ls
        else
          tail l0 : map ("  " ++)
          (wiktEntryExtract $ takeWhile (not . ("^" `isPrefixOf`)) lRest)
      _ -> return ()

wiktEntryExtract :: [String] -> [String]
wiktEntryExtract =

lk :: Opts -> [String] -> String -> IO ()
lk opts@(Opts wdsOnly dictType) grepArgs word =
    if dictType == WiktGer
      then
        wiktGrep opts word gerDictF grepArgs
      else
        dictGrep 0 scrPtn scrDictF $
        if wdsOnly then "-o":grepArgs else grepArgs
  where
    scrDictF = "/usr/share/dict/CSW12TWL06.txt"
    scrPtn = "^" ++
        (if dictType == NaScr then " " else ".") ++
        doReplacements [('.', "[^ ]")] (map toUpper word) ++ "\\b"
    gerDictF = "/home/danl/data/wikt/ger"

main :: IO ()
main = do
    args <- getArgs
    let usage = "usage: lk [options] word-pattern"
        doErrs errs = error $ concat errs ++ usageInfo usage optList
    (opts, wordL) <- case getOpt Permute optList args of
        (o, n, []) -> return (foldl (flip id) defOpts o, n)
        (_, _, errs) -> doErrs errs
    case wordL of
        word:grepArgs -> lk opts grepArgs word
        _ -> doErrs []
