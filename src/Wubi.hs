module Wubi where

data Stroke = P | N | H | S | G
  deriving (Eq, Ord, Show)
data WubiKey = WK Stroke Int
  deriving (Eq, Ord)
type Wubi = [WubiKey]

qToWK :: Char -> Maybe WubiKey
qToWK 't' = Just $ WK P 1
qToWK 'r' = Just $ WK P 2
qToWK 'e' = Just $ WK P 3
qToWK 'w' = Just $ WK P 4
qToWK 'q' = Just $ WK P 5
qToWK 'y' = Just $ WK N 1
qToWK 'u' = Just $ WK N 2
qToWK 'i' = Just $ WK N 3
qToWK 'o' = Just $ WK N 4
qToWK 'p' = Just $ WK N 5
qToWK 'g' = Just $ WK H 1
qToWK 'f' = Just $ WK H 2
qToWK 'd' = Just $ WK H 3
qToWK 's' = Just $ WK H 4
qToWK 'a' = Just $ WK H 5
qToWK 'h' = Just $ WK S 1
qToWK 'j' = Just $ WK S 2
qToWK 'k' = Just $ WK S 3
qToWK 'l' = Just $ WK S 4
qToWK 'm' = Just $ WK S 5
qToWK 'n' = Just $ WK G 1
qToWK 'b' = Just $ WK G 2
qToWK 'v' = Just $ WK G 3
qToWK 'c' = Just $ WK G 4
qToWK 'x' = Just $ WK G 5
qToWK _ = Nothing

showWK (WK s n) = show s ++ show n
