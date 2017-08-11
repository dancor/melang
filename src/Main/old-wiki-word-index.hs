{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import qualified Data.Trie as BST
import qualified Data.Trie.Convenience as BSTC
-- import qualified Data.ByteString.Char8 as BSC
import Data.Word

import Util.BS

type Str = BS.ByteString

type Pos = Int

type Index = BST.Trie Int

isWordByte :: Word8 -> Bool
isWordByte !b =
    b >= 97 && b <= 122 || -- lowercase
    b >= 65 && b <= 90 ||  -- uppercase
    b >= 128 -- unicode
    -- b >= 48 && b <= 57 -- numbers

growIndex :: (Index, (Pos, Str)) -> Word8 -> (Index, (Pos, Str))
growIndex (!i, (!p, !s)) b = if isWordByte b
  then (i, (p + 1, s <> BS.singleton b))
  else
    ( if BS.null s then i else BSTC.insertWith' (+) s 1 i
    , (p + 1, "")
    )

jInsertWith :: (JE a) => (a -> a -> a) -> Key -> a -> JudyL a -> IO ()
jInsertWith f k v j = do
    vCurMb <- J.lookup k j
    J.insert k 
    case vCurMb of
      Just vCur -> f 

indexAddLine :: Index -> Pos -> BS.ByteString -> IO Pos
indexAddLine !j !pos !s = if BS.null s then return pos else do
    let (pre, rest1) = BS.break isWordByte s
        (word, rest) = BS.span isWordByte rest1
        posWord = pos + BS.length pre
        posRest = posWord + BS.length word
    J.insert word j
    indexAddLine posRest rest

buildIndex :: LStr -> Index
buildIndex !x = fst $ BSL.foldl' growIndex (BST.empty, (0, "")) x

printIndex :: Index -> IO ()
printIndex j = do
    print "Done."
    return ()
    {-
    BSLC.unlines . map BSL.fromStrict $
    BST.toListBy (\ !word !n -> word <> "\t" <> BSC.pack (show n)) x
    -}

main :: IO ()
main = do
    j <- J.new
    index <- hLinesFold (indexAddLine j) 0 stdin
    printIndex index
