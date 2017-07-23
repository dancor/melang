{-# LANGUAGE OverloadedStrings #-}

module MediaWiki
  ( bzxmlTitleTexts
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.Text as CT
import Data.Maybe
import Data.Monoid
import Data.Strict.Tuple
import GHC.IO.Handle
import System.Environment
import System.Process
import Text.HTML.TagSoup

import Util.BS

type Str = BS.ByteString

type Article = Pair Str [Str]

titlePrefix :: Str
titlePrefix = "    <title>"

titleSuffix :: Str
titleSuffix = "</title>"

bodyPrefix :: Str
bodyPrefix = "      <text xml:space=\"preserve\">"

bsLines :: Monad m => Conduit Str m Str
bsLines =
    loop id
  where
    loop front = await >>= maybe (finish front) (go front)

    finish front =
        let final = front BS.empty
         in unless (BS.null final) (yield final)

    go sofar more =
        case BS.uncons second of
            Just (_, second') -> yield (sofar first') >> go id second'
            Nothing ->
                let rest = sofar more
                 in loop $ BS.append rest
      where
        (first', second) = BSC.break (== '\n') more

bsKillLastN n s = BS.take (BS.length s - n) s

cOnHeadCont f c = do
    m <- await
    case m of
      Just a -> leftover (f a) >> c
      _ -> return ()

pullArticles :: Monad m => Conduit Str m Article
pullArticles = do
    CC.dropWhile (not . BS.isPrefixOf titlePrefix)
    mTitle <- await
    case mTitle of
      Just title -> do
        body <- (CC.dropWhile (not . BS.isPrefixOf bodyPrefix) >>
            cOnHeadCont (BS.drop (BS.length bodyPrefix)) (CC.take 2)) =$=
            CC.sinkList
        yield $
            bsKillLastN (BS.length titleSuffix)
            ((BS.drop (BS.length titlePrefix)) title) :!: body
        pullArticles
      _ -> return ()

bsTakeEnd n s = BS.drop (BS.length s - n) s

summarizeArticle :: Article -> Str
summarizeArticle (title :!: []) = title <> ": " <> "EMPTY"
summarizeArticle (title :!: body) = title <> ": " <> 
    if bodyLen <= summaryLen
      then bodyCat
      else
        BS.take (summaryLen - 105) bodyCat <> " ... " <> bsTakeEnd 100 bodyCat
  where
    summaryLen = 200
    bodyCat = BS.concat body
    bodyLen = BS.length bodyCat

-- bzxmlTitleTexts :: FilePath -> IO [Pair Str [Str]]
bzxmlTitleTexts :: FilePath -> IO ()
bzxmlTitleTexts fp = do
    let getBzHandle = do
            (_, Just hOut, _, _) <-
                createProcess (proc "bzcat" [fp]) {std_out = CreatePipe}
            return hOut
    res <- runResourceT $ CC.sourceIOHandle getBzHandle
        $= bsLines
        $= pullArticles
        $= CC.map summarizeArticle
        $= CC.take 80
        $$ CC.sinkList
    mapM_ BSC.putStrLn res
