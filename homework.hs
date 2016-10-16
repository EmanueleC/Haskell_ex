toDigitsPositive n = if n < 10
                      then [n]
                      else toDigitsPositive (n `div` 10) ++ [n `mod` 10]

toDigits n = if n <= 0 then [] else toDigitsPositive n

toDigitsRev n = reverse (toDigits n)

doubleSecond (x:y:xs) = y*2

doubleEveryOther lst = if length lst < 2
                        then lst
                        else doubleEveryOther (take (length lst - 2) lst) ++
                          [doubleSecond (reverse lst), last lst]

sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate n = if (sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0)
              then True
              else False

hanoi n a b c = if n == 1
                  then [(a,b)]
                  else (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
