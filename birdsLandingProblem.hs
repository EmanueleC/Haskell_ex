type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs(left + n - right) < 4 = Just (left + n, right)
  | otherwise                 = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs(right + n - left) < 4 = Just (left, right + n)
  | otherwise                 = Nothing

sequenceLand :: [Pole -> Maybe Pole] -> Maybe Pole
sequenceLand ls = sequenceHelp (reverse ls)

sequenceHelp [] = Just (0,0)
sequenceHelp (x:xs) = sequenceHelp xs >>= x
