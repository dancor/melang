{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Strict.Tuple

import MediaWiki

myConduit :: Conduit Article (ResourceT IO) o
myConduit =
    CC.filter (\(title :!: body) -> any ("recurrir" `BS.isInfixOf`) body)
    =$= CC.map (\(title :!: body) -> title)
    =$= CC.take 1000
    =$= awaitForever (liftIO . BSC.putStrLn)

main :: IO ()
main = do
    runResourceT $ bzxmlArticles "/home/danl/data/wiki/es.xml.bz2" myConduit
