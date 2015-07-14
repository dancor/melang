{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Trie as BST
import qualified Data.Trie.Convenience as BSTC

type Str = BS.ByteString

type Index = BST.Trie Int

countWds :: FilePath -> IO Int
countWds p =
    runResourceT $ CB.sourceFile
        ("/home/danl/p/l/melang/lang/de/base/" ++ p ++ "-wds.txt")
        $$ CB.lines
        =$ CC.foldl countFold 0
  where
    countFold !size !line = size + count
      where
        (_wd:countStr:_) = BSC.split '\t' line
        Just (count, _) = BSC.readInt countStr

loadWds :: FilePath -> IO (Int, Index)
loadWds p =
    runResourceT $ CB.sourceFile
        ("/home/danl/p/l/melang/lang/de/base/" ++ p ++ "-wds.txt")
        $$ CB.lines
        =$ CC.foldl loadFold (0, BST.empty)
  where
    loadFold (!size, !index) !line =
        seq size2 $ seq index2
        (size2, index2)
      where
        (wd:countStr:_) = BSC.split '\t' line
        Just (count, _) = BSC.readInt countStr
        size2 = size + count
        wd2 = DTE.encodeUtf8 . DT.toLower $ DTE.decodeUtf8 wd
        index2 = seq wd2 $ BSTC.insertWith (+) wd2 count index

main :: IO ()
main = do
    gbSize <- countWds "gb"
    putStrLn $ "Did gb: " ++ show gbSize
    (wiktSize, wiktIndex) <- loadWds "wikt"
    putStrLn $ "Did wikt: " ++ show wiktSize
    (wikiSize, wikiIndex) <- loadWds "wiki"
    putStrLn $ "Did wiki: " ++ show wikiSize
    let myLookup :: Str -> Maybe (Str, Float)
        myLookup line =
            case (BST.lookup wd2 wiktIndex, BST.lookup wd2 wikiIndex) of
              (Just wiktCount, Just wikiCount) ->
                Just (wd, minimum [gbFrac, wiktFrac, wikiFrac])
                  where
                    wiktFrac = fromIntegral wiktCount / fromIntegral wiktSize
                    wikiFrac = fromIntegral wikiCount / fromIntegral wikiSize
              _ -> Nothing
          where
            (wd:countStr:_) = BSC.split '\t' line
            wd2 = DTE.encodeUtf8 . DT.toLower $ DTE.decodeUtf8 wd
            Just (count, _) = BSC.readInt countStr
            gbFrac = fromIntegral count / fromIntegral gbSize
        myMap :: (Str, Float) -> Str
        myMap (wd, count) = wd <> "\t" <>
            BSC.pack (show $ log (1 / count) / log 10) <> "\n"
    runResourceT $
        CC.sourceFile "/home/danl/p/l/melang/lang/de/base/gb-wds.txt"
        $$ CB.lines
        =$ CC.map myLookup
        =$ CC.filter isJust
        =$ CC.map (myMap . fromJust)
        =$ CC.stdout
