import MyParser
-- ex 1
notEndLine = sat (/= '\n')

comment :: Parser()
comment = do
            string "--"
            many notEndLine
            char '\n'
            return ()

-- ex 2
data Expr = Add (Term) (Expr) | Term (Term) deriving Show
data Term = Mult (Factor) (Term) | Factor (Factor) deriving Show
data Factor = Par (Expr) | Nat deriving Show

expr' :: Parser Expr
expr' = do
  t <- term'
  do
    symbol "+"
    e <- expr'
    return (Add t e)
    <|> return (Term t)

term' :: Parser Term
term' = do
        f <- factor'
        do
          symbol "*"
          t <- term'
          return (Mult f t)
          <|> return (Factor f)

factor' :: Parser Factor
factor' = do
          symbol "("
          e <- expr'
          symbol ")"
          return (Par e)
        <|> natural'

natural' :: Parser Factor
natural' = do
            space
            v <- nat
            space
            return Nat

-- ex 4
fibs::[Integer]

fibs = [help x | x <- [0..]]

help 0 = 0
help 1 = 1
help n = help (n-1) + help (n-2)
