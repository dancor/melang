import Control.Applicative
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.ByteString as BS
import qualified Codec.Binary.UTF8.String as U8

tagTextMaybe (TagText s) = Just s
tagTextMaybe _ = Nothing

onTail f [] = f []
onTail f (a:b) = a : f b

f = do
  tags <- parseTags <$> readFile "3k.html"
  let
    rows = take 2400 . drop 4 $ splitWhen (== TagOpen "tr" []) tags
  return $ map ((\ (n:z:d) -> 
      z ++ "\t" ++ n ++ " " ++ concat d) . 
    catMaybes . map tagTextMaybe) rows

main = do
  r <- f
  putStr $ unlines $ map (concatMap esc . init) r

esc '\n' = "\\n"
esc '\\' = "\\\\"
esc c = [c]
