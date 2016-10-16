import Data.Char

letToInt c = ord c - ord 'a'

intToLet n = chr (ord 'a' + n)

shift n c
  | isLower c = intToLet ((letToInt c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n c | c <- xs]
