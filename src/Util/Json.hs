module Util.Json where

import Control.Monad
import Data.Aeson
import Data.Attoparsec
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as DT
import qualified Data.Vector as Vec

data Tree a
    = Leaf a | Tree [Tree a]
    deriving (Show)

tree :: (a -> b) -> ([Tree a] -> b) -> Tree a -> b
tree f _ (Leaf x) = f x
tree _ g (Tree x) = g x

fromLeaf :: Show a => Tree a -> a
fromLeaf = tree id (error . ("fromLeaf: " ++) . show)

asLeaf :: Tree a -> Maybe a
asLeaf = tree Just (const Nothing)

asKids :: Tree a -> [Tree a]
asKids = tree (const []) id

valToTree :: Value -> Tree (Maybe DT.Text)
valToTree Null = Leaf Nothing
valToTree (String x) = Leaf $ Just x
valToTree (Array x) = Tree . Vec.toList $ Vec.map valToTree x
valToTree x = error $ "valToTree: " ++ show x

parseJson :: BS.ByteString -> HMS.HashMap DT.Text Value
parseJson l = result
  where
    Done _ (Object result) = parse json l
