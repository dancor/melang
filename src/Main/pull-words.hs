module Main where

import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import qualified Data.Text as DT

findWords :: Monad m => Conduit DT.Text m DT.Text
findWords = dropBad >> start
  where
    dropBad = CT.dropWhile (not . isAlpha)
    start = CC.peek >>= maybe (return ()) (const $ loop)
    loop = do
        CT.takeWhile isAlpha
        dropBad
        findWords

main :: IO ()
main = runResourceT $ CC.stdin
    $$ findWords
    =$ CC.unlines
    =$ CC.stdout
