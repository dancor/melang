{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..)) 
import Data.Char
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.Search as BSLS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE

killBraced :: String -> String
killBraced "" = ""
killBraced ('<':xs) = killBraced $ tail $ dropWhile (/= '>') xs
killBraced (x:xs) = x : killBraced xs

doChar :: Char -> IO ()
doChar c = do
    Just (_, zimHtml :: BSL.ByteString) <-
        getContent ("/home/danl/data/wikt/en.zim" :: String)
        (Url $ "A/" <> DTE.encodeUtf8 (DT.pack [c]) <> ".html")
    let (_, compositionHtml) = BSLS.breakAfter "composition" zimHtml
    putStrLn $
        filter (`notElem` ("⿱" :: String)) $
        filter (not . isSpace) $
        killBraced $
        DTL.unpack $
        DTLE.decodeUtf8 $
        BSLC.takeWhile (/= ')') $
        compositionHtml

main :: IO ()
main = do
    doChar '照'
