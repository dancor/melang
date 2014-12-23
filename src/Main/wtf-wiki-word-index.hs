{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Monoid
import qualified Data.Trie as BST
import qualified Data.Trie.Convenience as BSTC
-- import qualified Data.ByteString.Char8 as BSC
import Data.Word

type LStr = BSL.ByteString

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

buildIndex :: LStr -> Index
buildIndex !x = fst $ BSL.foldl' growIndex (BST.empty, (0, "")) x

showIndex :: Index -> LStr
showIndex !x = BSLC.unlines . map BSL.fromStrict $
    BST.toListBy (\ !word !n -> word <> "\t" <> BSC.pack (show n)) x

wat :: LStr -> LStr
wat ls = showIndex $!! buildIndex ls

main :: IO ()
main = BSL.interact wat
