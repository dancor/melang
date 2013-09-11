{-# LANGUAGE OverloadedStrings #-}

module ExtractWikiPages where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.DList as DL
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Text.XML.Expat.Proc
import Text.XML.Expat.Tree

validatePage :: (Maybe a, Maybe b) -> Maybe (a, b)
validatePage pageData = case pageData of
  (Just a, Just b) -> Just (a, b)
  (_, _) -> Nothing

scanChildren :: [UNode BS.ByteString] -> DL.DList BS.ByteString
scanChildren c = case c of
  h:t -> DL.append (getContent h) (scanChildren t)
  []  -> DL.fromList []

getContent :: UNode BS.ByteString -> DL.DList BS.ByteString
getContent treeElement = case treeElement of
  Element _name _attributes children -> scanChildren children
  Text text -> DL.fromList [text]

extractText
    :: NodeG [] BS.ByteString text
    -> Maybe (NodeG [] BS.ByteString text)
extractText page = findChild "text" =<< findChild "revision" page

getTitlesAndTexts
    :: NodeG [] BS.ByteString BS.ByteString
    -> [(DT.Text, BS.ByteString)]
getTitlesAndTexts tree =
    catMaybes $ map (validatePage . getPageData) pageNodes
  where
    pageNodes = filterChildren relevantChildren tree
    getPageData page =
        ( DTE.decodeUtf8 . DL.head . getContent <$> extractTitle page
        , BS.concat . DL.toList . getContent <$> extractText page
        )
    extractTitle = findChild "title"
    relevantChildren node = case node of
        Element "page" _attributes _children -> True
        _ -> False

-- Some Wiki dumps start with a line like:
-- ------> ../enwiktionary-20130907-pages-articles.xml.bz2 <------
killPossibleHeader :: BSL.ByteString -> BSL.ByteString
killPossibleHeader s = if "-" `BSLC.isPrefixOf` s
  then BSL.tail $ BSLC.dropWhile (/= '\n') s
  else s

-- Take Wiki dump XML and give title and content of each entry page.
--
-- Sadly, I see no way to treat an XML parsing error
-- (even after processing the resulting pages),
-- without having an unacceptable space leak. Who is to blame?
-- So in this case there will simply be no output.
extractPages :: BSL.ByteString -> [(DT.Text, BS.ByteString)]
extractPages xmlStr =
    filter (goodTitle . fst) $ getTitlesAndTexts tree
  where
    (tree, _mErr) = parse defaultParseOptions $ killPossibleHeader xmlStr
    -- There's probably a more accurate way to isolate entry pages.
    goodTitle t = not $ ":" `DT.isInfixOf` t || "/" `DT.isInfixOf` t
