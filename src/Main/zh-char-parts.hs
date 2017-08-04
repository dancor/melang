{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Archive.Zim.Parser (getContent, Url(..)) 
import Data.Char
import Data.List.Split
import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.Search as BSLS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE
import System.Environment
import qualified HSH

import Util.Display

killBraced :: String -> String
killBraced "" = ""
killBraced ('<':xs) = killBraced $ tail $ dropWhile (/= '>') xs
killBraced (x:xs) = x : killBraced xs

doChar :: Int -> Char -> IO [[String]]
doChar indent c = do
    Just (_, zimHtml :: BSL.ByteString) <-
        getContent ("/home/danl/data/wikt/en.zim" :: String)
        (Url $ "A/" <> DTE.encodeUtf8 (DT.pack [c]) <> ".html")
    let (_, compositionHtml) = BSLS.breakAfter "composition" zimHtml
        parts =
            filter (`notElem` ("⿱⿰⿸⿻" :: String)) $
            filter (not . isSpace) $
            killBraced $
            DTL.unpack $
            DTLE.decodeUtf8 $
            BSLC.takeWhile (/= ')') $
            compositionHtml
    grepResult <- flip HSH.catchEC (\_ -> return "\t\t\t\t\t") $ HSH.runSL 
        ( "grep" :: String
        , [ "^" ++ [c] ++ "\t"
          , "/home/danl/p/l/melang/lang/zh/dict"
          ]
        )
    doCharResults <- mapM (doChar (indent + 1)) parts
    return $ 
        [ [ replicate (2 * indent) ' ' ++ "- " ++ [c] ] ++
          map (' ':) (tail $ splitWhen (== '\t') grepResult)
        ] ++
        concat doCharResults

main :: IO ()
main = do
    args <- getArgs
    doCharResults <- mapM (doChar 0) $ concat args
    mapM_ putStrLn $ spaceTable $ concat doCharResults
