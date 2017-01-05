module MyParser where
import Data.Char

newtype Parser a = P(String -> [(a,String)])
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P(\inp -> case inp of
                            [] -> []
                            (x:xs) -> [(x,xs)])

instance Functor Parser where
  fmap f p = P(\inp -> case (parse p inp) of
                                            [] -> []
                                            [(v,out)] -> [(f v,out)])

instance Applicative Parser where
  pure v = P(\inp -> [(v,inp)])
  pg <*> px = P(\inp -> case (parse pg inp) of
                                            [] -> []
                                            [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P(\inp -> case parse p inp of
                                        [] -> []
                                        [(v,out)] -> parse (f v) out)

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  many x = some x <|> pure []
  some :: f a -> f [a]
  some x = pure (:) <*> x <*> many x

instance Alternative Parser where
  empty = P(\inp -> [])
  p <|> q = P(\inp -> case parse p inp of
                      [] -> parse q inp
                      [(v,out)] -> [(v,out)])


sat :: (Char -> Bool) -> Parser Char
sat p = do
          x <- item
          if (p x) then return x
          else empty

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphaNum = sat isAlphaNum
char x = sat (== x)
string [] = return []
string (x:xs) = do
                  char x
                  string xs
                  return (x:xs)

ident :: Parser String
ident = do
          x <- lower
          xs <- many alphaNum
          return (x:xs)

nat :: Parser Int
nat = do
        xs <- some digit
        return (read xs)

space :: Parser ()
space = do
          many (sat isSpace)
          return ()

int :: Parser Int
int = do
        char '-'
        n <- nat
        return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do
            space
            v <- p
            space
            return v

identifier :: Parser String
identifier = token ident
natural :: Parser Int
natural = token nat
integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
        symbol "["
        n <- natural
        ns <- many (do
                      symbol ","
                      natural)
        symbol "]"
        return (n:ns)

expr' :: Parser Int
expr' = do
  t <- term'
  do
    symbol "+"
    e <- expr'
    return (t + e)
    <|> return t

term' :: Parser Int
term' = do
        f <- factor'
        do
          symbol "*"
          t <- term'
          return (f * t)
          <|> return f

factor' :: Parser Int
factor' = do
          symbol "("
          e <- expr'
          symbol ")"
          return e
        <|> natural
