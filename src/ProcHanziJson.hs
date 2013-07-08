{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Number
import Data.Char
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Vector as Vec
-- import Debug.Trace
import System.IO

import Util.Json

data CharPart = CharPart
    { cpCodePoint :: !Int
    , _cpVariantNum :: !Int
    }
    deriving (Show)

fromInt :: Value -> Int
fromInt (Number (I x)) = fromIntegral x
fromInt (String x) = read $ DT.unpack x
fromInt x = error $ "fromNumber: " ++ show x

fromArr :: Value -> Array
fromArr (Array x) = x
fromArr x = error $ "fromArr: " ++ show x

fromObj :: Value -> Object
fromObj (Object x) = x
fromObj x = error $ "fromObj: " ++ show x

grabInfo :: HMS.HashMap DT.Text Value -> Maybe (CharPart, [CharPart])
grabInfo j =
    fmap (flip (,) . catMaybes . map (grabPart . fromObj) . Vec.toList .
    fromArr . fromJust $ HMS.lookup "parts" j) $ grabPart j

grabPart :: HMS.HashMap DT.Text Value -> Maybe CharPart
grabPart j =
    killVar $
    case fromInt <$> HMS.lookup "uni" j of
      Just i ->
        Just . CharPart i . maybe 0 fromInt $ HMS.lookup "variant" j
      _ -> Nothing
  where
    killVar x@(Just (CharPart _ 0)) = x
    killVar _ = Nothing

showLine :: (CharPart, [CharPart]) -> String
showLine (cp, cps) = showCp cp : map showCp cps

showCp :: CharPart -> Char
showCp = chr . cpCodePoint

main :: IO ()
main = do
    ls <- catMaybes . map (grabInfo . parseJson) <$> hReadLines stdin
    mapM_ putStrLn $ map showLine ls
