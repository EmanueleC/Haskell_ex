foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

sum' = foldr' (+) 0
or' = foldr' (||) False
length' = foldr' (\_ x -> 1 + x) 0

-- this function converts a binary number into a decimal number
bitToInt xs = bitToInt' (reverse(xs))
bitToInt' = foldr' (\a n -> a + n*2) 0
