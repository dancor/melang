-- We don't correctly parse XML. We just do fast and easy processing
-- that works for the mediawikis. We can always change later.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module MediaWiki
  ( Str
  , Article
  , bzxmlArticles
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.HashTable.IO as H
import Data.Monoid
import Data.Strict.Tuple
import System.Process

type Str = BS.ByteString

type Article = Pair Str [Str]

type HashTable k v = H.BasicHashTable k v

titlePrefix, titleSuffix, bodyPrefix, bodySuffix, postBodyPrefix :: Str
titlePrefix     = "    <title>"
titleSuffix     = "</title>"
bodyPrefix      = "      <text xml:space=\"preserve\">"
bodySuffix      = "</text>"
postBodyPrefix  = "      <sha1>"

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

bsDropEnd :: Int -> Str -> Str
bsDropEnd n s = BS.take (BS.length s - n) s

cOnHead :: Monad m => (a -> a) -> Conduit a m a
cOnHead f = do
    m <- await
    case m of
      Just a -> yield (f a) >> awaitForever yield
      _ -> return ()

cOnLast :: Monad m => (a -> a) -> Conduit a m a
cOnLast f = do
    m <- await
    case m of
      Just a -> do
        weAreLast <- CC.null
        yield $ if weAreLast then f a else a
        cOnLast f
      _ -> return ()

pullArticles :: Monad m => Conduit Str m Article
pullArticles = do
    CC.dropWhile (not . BS.isPrefixOf titlePrefix)
    mTitle <- await
    case mTitle of
      Just title -> do
        body <- (CC.dropWhile (not . BS.isPrefixOf bodyPrefix)
            >> cOnHead (BS.drop (BS.length bodyPrefix)))
            =$= CC.takeWhile (not . BS.isPrefixOf postBodyPrefix)
            =$= cOnLast (bsDropEnd (BS.length bodySuffix))
            =$= CC.sinkList
        yield $
            bsDropEnd (BS.length titleSuffix)
            ((BS.drop (BS.length titlePrefix)) title) :!: body
        pullArticles
      _ -> return ()

bsTakeEnd :: Int -> Str -> Str
bsTakeEnd n s = BS.drop (BS.length s - n) s

summarizeArticle :: Article -> Str
summarizeArticle (title :!: []) = title <> ": " <> "EMPTY"
summarizeArticle (title :!: body) = title <> ": " <> 
    if bodyLen <= summaryLen then bodyCat else
        BS.take (summaryLen - 105) bodyCat <> " ... " <> bsTakeEnd 100 bodyCat
  where
    summaryLen = 200
    bodyCat = BS.concat body
    bodyLen = BS.length bodyCat

-- articleTitle :: Article -> Str
-- articleTitle (

bzxmlArticles :: (MonadIO m, MonadResource m)
  => FilePath -> Consumer Article m r -> m r
--bzxmlArticles :: FilePath -> IO ()
bzxmlArticles fp c = do
    let getBzHandle = do
            -- (_, Just hOut, _, _) <- createProcess (proc "pbzip2" ["-cd", fp])
            (_, Just hOut, _, _) <- createProcess (proc "bzcat" [fp])
                {std_out = CreatePipe}
            return hOut
    runConduit $ CC.sourceIOHandle getBzHandle
        =$= bsLines
        =$= pullArticles
        =$= c
        -- CC.map (\(title :!: _) -> title)
        -- $$ awaitForever (liftIO . BSC.putStrLn)
