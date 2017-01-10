import MyParser
-- ex 1
notEndLine = sat (/= '\n')

comment :: Parser()
comment = do
            space          
            string "--"
            many notEndLine
            char '\n'
            return ()

-- ex 2
data Expr = Add (Term) (Expr) | Term (Term) deriving Show
data Term = Mult (Factor) (Term) | Factor (Factor) deriving Show
data Factor = Par (Expr) | Nat deriving Show

expr2 :: Parser Expr
expr2 = do
  t <- term2
  do
    symbol "+"
    e <- expr2
    return (Add t e)
    <|> return (Term t)

term2 :: Parser Term
term2 = do
        f <- factor2
        do
          symbol "*"
          t <- term2
          return (Mult f t)
          <|> return (Factor f)

factor2 :: Parser Factor
factor2 = do
          symbol "("
          e <- expr2
          symbol ")"
          return (Par e)
        <|> natural2

natural2 :: Parser Factor
natural2 = do
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
