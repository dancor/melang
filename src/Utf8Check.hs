{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as DTE

import BSUtil

checkLine :: Int -> BS.ByteString -> Maybe (BS.ByteString)
checkLine n line =
    case DTE.decodeUtf8' line of
        Left err -> Just $ BS.concat
            ["Error: Line ", BSC.pack $ show n, " :", BSC.pack $ show err]
        _ -> Nothing

main :: IO ()
main = bsInteractLErr (map Left . catMaybes . zipWith checkLine [1..])
