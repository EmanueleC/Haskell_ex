maximum' [a] = a
maximum' [] = error "maximum' on empty list"
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' n a
  | n <= 0 = []
  | otherwise = a:replicate' (n-1) a

take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs

quicksort [] = []
quicksort (x:xs) = quicksort smallEqual ++ [x] ++ quicksort greater
  where smallEqual = [v | v <- xs, v <= x]
        greater = [v | v <- xs, v > x]

flip' f y x = f x y
