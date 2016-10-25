toDigitsPositive n = if n < 10
                      then [n]
                      else toDigitsPositive (n `div` 10) ++ [n `mod` 10]

-- split a number into digits
toDigits n = if n <= 0 then [] else toDigitsPositive n

toDigitsRev n = reverse (toDigits n)

doubleSecond (x:y:xs) = y*2

-- example: [1,2,3,4] -> [2,2,6,4]
doubleEveryOther lst = if length lst < 2
                        then lst
                        else doubleEveryOther (take (length lst - 2) lst) ++
                          [doubleSecond (reverse lst), last lst]

-- sum all numbers in a list
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate n = if (sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0)
              then True
              else False

-- hanoi problem (n disks, 3 pegs)
hanoi n a b c = if n == 1
                  then [(a,b)]
                  else (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
