-- ex2 monads
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
--fmap :: (a -> b) -> Expr a -> Expr b
  fmap f ex = case ex of
                Var x -> Var (f x)
                Add e1 e2 -> Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
--pure :: Expr a
  pure = Var
--(<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Var f) <*> ex = case ex of
                    Var x -> Var (f x)
                    Add e1 e2 -> Add (fmap f e1) (fmap f e2)

instance Monad Expr where
--(>>=) :: Expr a -> (a -> Expr b) -> Expr b
  ex >>= f = case ex of
              Var a -> f a
              Add e1 e2 -> Add (e1 >>= f) (e2 >>= f)

-- ex3 monads
type Stato = [Char]
newtype ST a = S(Stato -> (a, Stato))
app (S st) x = st x

instance Functor ST where
--fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
                x <- st
                return (g x)

instance Applicative ST where
--pure :: ST a
  pure x = S(\s -> (x,s))
--(<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
                  f <- stf
                  x <- stx
                  return (f x)

instance Monad ST where
--(>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S(\s -> let (x,s') = app st s in app (f x) s')
