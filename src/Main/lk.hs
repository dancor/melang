-- Look up words and possibly definitions
-- (in witionary, game word lists, etc.).

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BU
import Data.Char
import Data.Maybe
import Data.Monoid
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process

import Wikt.ProcDefs

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

lkR :: Int -> BS.ByteString -> IO ()
lkR depth line = do
    BSC.putStrLn $ BSC.replicate (3 * (depth - 1)) ' ' <>
        (if depth > 0 then "=> " else "") <> line
    return ()

dictGrep :: Int -> String -> String -> [String] -> IO ()
dictGrep depth ptn dictF grepArgs = do
    (pIn, pOut, pErr, pId) <-
        runInteractiveProcess "grep" (ptn:dictF:grepArgs) Nothing Nothing
    hClose pIn
    cOut <- BS.hGetContents pOut
    mapM_ (lkR depth) $ BSC.lines cOut
    cErr <- BS.hGetContents pErr
    BS.hPutStr stderr cErr
    _ <- waitForProcess pId
    return ()

wiktGrepWds :: String -> [String] -> String -> IO [BS.ByteString]
wiktGrepWds dictF extraArgs term = do
    (_pIn, pOut, _pErr, _pId) <-
        runInteractiveProcess "grep"
        (["^^" ++ term ++ "$", dictF, "-i"] ++ extraArgs)
        Nothing Nothing
    cOut <- BS.hGetContents pOut
    return $ case BSC.lines cOut of
      ls@(l0:lRest) -> map BS.tail ls
      _ -> []

wiktGrepOneDef :: String -> [String] -> String -> IO [BS.ByteString]
wiktGrepOneDef dictF extraArgs word = do
    (_pIn, pOut, _pErr, _pId) <-
        runInteractiveProcess "grep"
        (["^^" ++ word ++ "$", "-m", "1", "-A", "10000", dictF] ++
            extraArgs)
        Nothing Nothing
    cOut <- BS.hGetContents pOut
    return $ case BSC.lines cOut of
      ls@(l0:lRest) ->
          let entryLs = takeWhile (not . ("^" `BS.isPrefixOf`)) lRest
              (pronunciation, wiktLs) = wiktEntryExtract entryLs
              wiktLs2 = takeWhile (not . ("</text>" `BS.isInfixOf`)) wiktLs
              -- Exclude translations but include category markers:
              wiktLs3 = takeWhile (\x -> not ("[[" `BS.isPrefixOf` x) ||
                  BSC.all isUpper (BS.take 1 $ BS.drop 2 x)) wiktLs2
              wiktLs4 = map ("  " <>) wiktLs3
          in
          (BS.tail l0 <> ": " <> pronunciation) : wiktLs4 ++ [""]
      _ -> []

wiktGrep :: Opts -> String -> [String] -> String -> IO [BS.ByteString]
wiktGrep (Opts wdsOnly _) dictF extraArgs term = do
    wds <- wiktGrepWds dictF extraArgs term
    if wdsOnly
      then return wds
      else
        concat <$> mapM (wiktGrepOneDef dictF extraArgs) (map BU.toString wds)


wiktEntryExtract :: [Str] -> (Str, [Str])
wiktEntryExtract =
    second lkProcGoodSects .
    first (fromMaybe "/?/") .
    goodSectsPullPronunciation .
    langSectToGoodSects
  where
    lkProcGoodSects = concatMap (\(subHead, (_depth, block)) ->
        [subHead <> ":"] ++ block
        )

lk :: Opts -> [String] -> String -> IO ()
lk opts@(Opts wdsOnly dictType) grepArgs word =
    if dictType == WiktGer
      then
        wiktGrep opts gerDictF grepArgs word >>= mapM_ BSC.putStrLn
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
