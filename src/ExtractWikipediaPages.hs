{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.DList as DL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTI
import Data.Maybe
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
extractText page = do
    revision <- findChild "revision" page
    findChild "text" revision

pageDetails
    :: NodeG [] BS.ByteString BS.ByteString
    -> [(DT.Text, DL.DList BS.ByteString)]
pageDetails tree =
    catMaybes $ map (validatePage . getPageData) pageNodes
  where
    pageNodes = filterChildren relevantChildren tree
    getPageData page =
        ( DTE.decodeUtf8 . DL.head . getContent <$> extractTitle page
        , getContent <$> extractText page
        )
    extractTitle = findChild "title"
    relevantChildren node = case node of
        Element "page" _attributes _children -> True
        _ -> False

outputPages :: [DL.DList BS.ByteString] -> IO ()
outputPages pagesText = do
    let flattenedPages = map DL.toList pagesText
    mapM_ (mapM_ BS.putStr) flattenedPages

main :: IO ()
main = do
    xmlStr <- BSL.getContents
    let (tree, _mErr) = parse defaultParseOptions xmlStr
        pages = pageDetails tree
    forM_ pages $ \(title, text) ->
        unless (":" `DT.isInfixOf` title || "/" `DT.isInfixOf` title) $ do
            -- DTI.putStrLn title
            BS.writeFile (DT.unpack title) $ BS.concat $ DL.toList text
