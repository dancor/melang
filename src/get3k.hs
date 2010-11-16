import Control.Applicative
import Data.Char
import Data.List.Split
import Data.Maybe
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.ByteString as BS

s = BS.pack . map (fromIntegral . ord)

tagTextMaybe (TagText s) = Just s
tagTextMaybe _ = Nothing

onTail f [] = f []
onTail f (a:b) = a : f b

f = do
  tags <- parseTags <$> BS.readFile "3000char.html"
  let
    rows = take 2400 . drop 4 $ splitWhen (== TagOpen (s "tr") []) tags
  return $ map (onTail (onTail ((:[]) . BS.concat)) . catMaybes . 
    map tagTextMaybe) rows

main = f >>= print
