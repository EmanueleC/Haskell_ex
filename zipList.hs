import Control.Applicative

type State = Int
newtype ST a = S(State -> (a,State))
app (S st) x = st x

instance Functor ST where
  fmap g st = S(\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S(\s -> (x,s))
  stf <*> stx = S(\s -> let (f,s1) = app stf s
                            (x,s2) = app stx s1
                        in (f x,s2))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

relabel (Leaf _) n = (Leaf n, n+1)
relabel (Node l r) n = (Node l' r', n') where
  (l',m) = relabel l n
  (r',n') = relabel r m

fresh = S(\n -> (n,n+1))
alabel (Leaf _) = pure Leaf <*> fresh
alabel (Node l r) = pure Node <*> alabel l <*> alabel r

instance Monad ST where
  return = pure
  st >>= f = S(\s -> let (x,s') = app st s in app (f x) s')

mlabel (Leaf _) = do
                    n <- fresh
                    return (Leaf n)
mlabel (Node l r) = do
                      l' <- mlabel l
                      r' <- mlabel r
                      return (Node l' r')

-- Exercises

f1 x y z = pure (\a b c -> [a,b,c]) <*> x <*> y <*> z

f2 x y z = getZipList (pure (\a b c -> [a,b,c]) <*> ZipList x <*> ZipList y <*> ZipList z)

f3  [] = return []
f3 (x:xs) = S(\s -> ((map (+s) x), s+1)) >>= \k -> (f3 xs) >>= \q -> return (k:q)
