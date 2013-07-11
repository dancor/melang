module Main where

import Data.List.Split
import System.Environment

-- | Example: Centuline "的" "de5" "PRT:of"
data Centuline = Centuline
    { clWord :: String
    , clPron :: String
    , clGloss :: String
    }

readCentuline :: String -> Centuline
readCentuline s =
    case splitOn "\t" s of
      [w, p, g] -> Centuline w p g
      x -> error "readCentuline: " ++ show x

type Centudeck = [Centuline]

readCentudeck :: FilePath -> IO Centudeck
readCentudeck f = map readCentuline . lines <$> readFile f

type CheckFails = [String]

checkGlossUniq :: Centudeck -> CheckFails
checkGlossUniq cDeck = snd . foldr f (Set.empty, []) $ map clGloss cDeck
  where
    f (seen, fails) gloss =
        ( Set.insert seen gloss
        , if Set.member gloss seen
            then ("Gloss dupe: " ++ gloss) : fails
            else fails
        )


checkCentudeck :: Centudeck -> IO ()
checkCentudeck cDeck =
    mapM_ (\(n, f) -> do
        putStr n
        case f cDeck of
          [] -> putStrLn " ok."
          fails -> putStrLn "" >> mapM_ (putStrLn . ("* " ++)) fails
    )
    [("glossUniq:", checkGlossUniq)]

main :: IO ()
main = do
    args <- getArgs
    mapM_ (checkCentudeck . readCentudeck) args
