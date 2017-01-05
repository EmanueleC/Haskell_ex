import MyParser

data Prog = LetBlock (Bind) (Exp) | LetRec (Bind) (Exp) deriving Show
data Bind = VarDec (Exp) | VarDecAnd (Exp) (Bind) deriving Show
data Exp = Prog (Prog) | Lambda (Seq_Var) (Exp) | Expa (Expa) | OPP (Seq_Exp) | IfThenElse (Exp) (Exp) (Exp) deriving Show
data Expa = Term (Term) | TermOp (Term) (OPA) (Expa) deriving Show
data Term = Factor (Factor) | FactorOp (Factor) (OPM) (Term) deriving Show
data Factor = VarFact (Var) | VarY (Var) (Y) | Integer | Null | ParFact (Expa) deriving Show
data Y = Call | ParY (Seq_Exp) deriving Show
data OPA = Add | Minus deriving Show
data OPM = Mult | Div deriving Show
data OPP = Cons | Head | Tail | Eq | Leq deriving Show
data Seq_Var = VarSeq (Var) | SeqVar (Var) (Seq_Var) deriving Show
data Seq_Exp = Exp (Exp) | ExpAndSeq (Exp) (Seq_Exp) deriving Show
data Var = Var deriving Show

var :: Parser Var
var = do
        token ident
        return Var

opa :: Parser OPA
opa = do
        symbol "+"
        return Add
      <|> do
            symbol "-"
            return Minus

opm :: Parser OPM
opm = do
        symbol "*"
        return Mult
      <|> do
            symbol "/"
            return Div

opp :: Parser OPP
opp = do
        symbol "cons"
        return Cons
      <|> do
            symbol "head"
            return Head
          <|> do
                symbol "tail"
                return Tail
              <|> do
                    symbol "eq"
                    return Eq
                  <|> do
                        symbol "leq"
                        return Leq

y :: Parser Y
y = do
      symbol "()"
      return Call
    <|> do
          symbol "("
          s <- seq_exp
          symbol ")"
          return (ParY s)

expr :: Parser Exp
expr = do
        p <- prog
        return (Prog p)
      <|> do
            symbol "lambda"
            symbol "("
            sv <- seq_var
            symbol ")"
            e <- expr
            return (Lambda sv e)
          <|> do
                symbol "if"
                cond <- expr
                symbol "then"
                if_true <- expr
                symbol "else"
                if_false <- expr
                return (IfThenElse cond if_true if_false)
              <|> do
                    ea <- expa
                    return (Expa ea)
                  <|> do
                        opp
                        symbol "("
                        se <- seq_exp
                        symbol ")"
                        return (OPP se)

prog :: Parser Prog
prog = do
        symbol "let"
        b <- bind
        symbol "in"
        e <- expr
        symbol "end"
        return (LetBlock b e)
      <|> do
            symbol "letrec"
            b <- bind
            symbol "in"
            e <- expr
            symbol "end"
            return (LetRec b e)

bind :: Parser Bind
bind = do
        var
        symbol "="
        e <- expr
        do
          symbol "and"
          b <- bind
          return (VarDecAnd e b)
          <|> return (VarDec e)

expa :: Parser Expa
expa = do
        t <- term
        do
          o <- opa
          ea <- expa
          return (TermOp t o ea)
          <|> return (Term t)

term :: Parser Term
term = do
        f <- factor
        do
          o <- opm
          t <- term
          return (FactorOp f o t)
          <|> return (Factor f)

factor :: Parser Factor
factor = do
          v <- var
          do
            ys <- y
            return (VarY v ys)
            <|> return (VarFact v)
        <|> do
              integer
              return Integer
            <|> do
                  symbol "null"
                  return Null
                <|> do
                      symbol "("
                      e <- expa
                      symbol ")"
                      return (ParFact e)

seq_exp :: Parser Seq_Exp
seq_exp = do
            e <- expr
            do
              symbol ","
              s <- seq_exp
              return (ExpAndSeq e s)
              <|> return (Exp e)

seq_var :: Parser Seq_Var
seq_var = do
            v <- var
            do
              s <- seq_var
              return (SeqVar v s)
              <|> return (VarSeq v)
