module Wubi where

data Stroke = P | N | H | S | G
  deriving (Enum, Eq, Ord, Show)
data WubiKey = WK Stroke Int
  deriving (Eq, Ord)
type Wubi = [WubiKey]

qToWK :: Char -> WubiKey
qToWK 't' = WK P 1
qToWK 'r' = WK P 2
qToWK 'e' = WK P 3
qToWK 'w' = WK P 4
qToWK 'q' = WK P 5
qToWK 'y' = WK N 1
qToWK 'u' = WK N 2
qToWK 'i' = WK N 3
qToWK 'o' = WK N 4
qToWK 'p' = WK N 5
qToWK 'g' = WK H 1
qToWK 'f' = WK H 2
qToWK 'd' = WK H 3
qToWK 's' = WK H 4
qToWK 'a' = WK H 5
qToWK 'h' = WK S 1
qToWK 'j' = WK S 2
qToWK 'k' = WK S 3
qToWK 'l' = WK S 4
qToWK 'm' = WK S 5
qToWK 'n' = WK G 1
qToWK 'b' = WK G 2
qToWK 'v' = WK G 3
qToWK 'c' = WK G 4
qToWK 'x' = WK G 5

showWK (WK s n) = show s ++ show n

