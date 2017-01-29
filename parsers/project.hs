import MyParser

data LKC = VAR String | NUM Int | NULL | ADD LKC LKC | SUB LKC LKC | MULT LKC LKC
  | DIV LKC LKC | EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC | CONS LKC LKC
  | IF LKC LKC LKC | LAMBDA [LKC] LKC | CALL LKC [LKC] | LET LKC [(LKC,LKC)]
  | LETREC LKC [(LKC,LKC)] deriving (Show,Eq)

var :: Parser LKC
var = do
        v <- token ident
        return (VAR v)

opa :: Parser (LKC -> LKC -> LKC)
opa = do
        symbol "+"
        return ADD
      <|> do
            symbol "-"
            return SUB

opm :: Parser (LKC -> LKC -> LKC)
opm = do
        symbol "*"
        return MULT
      <|> do
            symbol "/"
            return DIV

opp :: Parser (LKC -> LKC -> LKC)
opp = do
        symbol "cons"
        return CONS
      <|> do
            symbol "eq"
            return Main.EQ
          <|> do
                symbol "leq"
                return LEQ

opp' :: Parser (LKC -> LKC)
opp' = do
        symbol "head"
        return H
      <|> do
            symbol "tail"
            return T

y :: Parser [LKC]
y = do
      symbol "()"
      return []
    <|> do
          symbol "("
          s <- seq_exp
          symbol ")"
          return s

expr :: Parser LKC
expr = do
        symbol "lambda"
        symbol "("
        sv <- seq_var
        symbol ")"
        e <- expr
        return (LAMBDA sv e)
      <|> do
            opp <- opp0
            return opp
          <|> do
                p <- prog
                return p
              <|> do
                    symbol "if"
                    cond <- expr
                    symbol "then"
                    if_true <- expr
                    symbol "else"
                    if_false <- expr
                    return (IF cond if_true if_false)
                  <|> do
                        ea <- expa
                        return ea

opp0 :: Parser LKC
opp0 = do
        opp' <- opp'
        symbol "("
        e <- expr
        symbol ")"
        return (opp' e)
      <|> do
            opp <- opp
            symbol "("
            e1 <- expr
            symbol ","
            e2 <- expr
            symbol ")"
            return (opp e1 e2)

prog :: Parser LKC
prog = do
        symbol "let"
        b <- bind
        symbol "in"
        e <- expr
        symbol "end"
        return (LET e b)
      <|> do
            symbol "letrec"
            b <- bind
            symbol "in"
            e <- expr
            symbol "end"
            return (LETREC e b)

bind :: Parser [(LKC,LKC)]
bind = do
        v <- var
        symbol "="
        e <- expr
        do
          symbol "and"
          b <- bind
          return ((v,e):b)
          <|> return [(v, e)]

expa :: Parser LKC
expa = do
        t <- term
        do
          o <- opa
          ea <- expa
          return (o t ea)
          <|> return t

term :: Parser LKC
term = do
        f <- factor
        do
          o <- opm
          t <- term
          return (o f t)
          <|> return f

factor :: Parser LKC
factor = do
          symbol "null"
          return NULL
        <|> do
              v <- var
              do
                y <- y
                return (CALL v y)
                <|> return v
            <|> do
                  num <- integer
                  return (NUM num)
                <|> do
                      symbol "("
                      ea <- expa
                      symbol ")"
                      return ea

seq_exp :: Parser [LKC]
seq_exp = do
            e <- expr
            do
              symbol ","
              s <- seq_exp
              return (e:s)
              <|> return [e]

seq_var :: Parser [LKC]
seq_var = do
            v <- var
            do
              s <- seq_var
              return (v:s)
              <|> return [v]
