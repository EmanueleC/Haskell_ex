import Control.Applicative

type Stack = [Char]

pop :: Stack -> (Char, Stack)
pop (x:xs) = (x,xs)

push :: Char -> Stack -> ((), Stack)
push x xs = ((),x:xs)

check_par :: [Char] -> Stack -> Bool
--takes a string and a Stack and says True/False whn the string is/is not correctly balanced
check_par [] [] = True
check_par [] _ = False
check_par (x:xs) st = if x == '(' then let ((),st') = push ')' st in check_par xs st'
                      else if x == ')' then let (y,st') = pop st in if y == ')' then check_par xs st'
                                                                    else False
                           else check_par xs st

type Stato = [Char]
newtype ST a = S(Stato -> (a, Stato))
app (S st) x = st x

instance Functor ST where
  fmap g st = S(\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S(\s -> (x,s))
  stf <*> stx = S(\s -> let (f,s1) = app stf s
                            (x,s2) = app stx s1
                        in (f x,s2))

instance Monad ST where
  return = pure
  st >>= f = S(\s -> let (x,s') = app st s in app (f x) s')

pop1 :: ST Char
-- S(Stack -> (Char, Stack))
pop1 = S(\(x:xs) -> (x,xs))

push1 :: Char -> ST ()
-- S(Stack -> ((),Stack))
push1 c = S(\s -> ((),c:s))

fresh1 = app (push1 ')')
fresh2 s = let (c,s') = app pop1 s in ((),s')

check_par1 :: [Char] -> ST Bool
check_par1 [] = S(\s -> (null s,s))
check_par1 (x:xs) = S(\s -> if x == '(' then fresh1 s else if x == ')' then fresh2 s else ((), s)) >>= \k -> (check_par1 xs) >>= \q -> if (k == () && q == True) then return True else return False

check_par2 [] = S(\s -> (null s,s))
check_par2 (x:xs) = do
                      k <- S(\s -> if x == '(' then fresh1 s else if x == ')' then fresh2 s else ((), s))
                      q <- check_par1 xs
                      if (k == () && q == True) then return True else return False
