module Solutions(vars, Prop(..)) where
import Data.List

-- exercise to be completed...

-- custom data type Prop represents a logical proposition
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop deriving (Show)

-- vars function returns a list with all variables
vars (Const _) = []
vars (Var a) = [a]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ ((vars p2) \\ (vars p1))
vars (Imply p1 p2) = vars p1 ++ ((vars p2) \\ (vars p1))

-- ttables function returns a list of lists of possible assignments to the variables of a proposition
ttables [] = [[]]
ttables (x:xs)
  | null xs = [[(x,True)],[(x,False)]]
  | otherwise = [ [(x,b)] ++ r | r <- ttables xs, b <- [True, False]]

-- returns true iff prop is a tautology. table is obtained by doing: (ttables (vars prop))
solutions prop table = and [valuta prop v | v <- table]

-- given a possible assignment to the variables of a proposition, valuta calculates the logical result
valuta (Const c) l = if (c == True) then True else False
valuta (Var a) l = snd (head (filter (\x -> fst x == a) l))
valuta (Not p) l = not (valuta p l)
valuta (And p1 p2) l = (valuta p1 l) && (valuta p2 l)
valuta (Imply p1 p2) l = if ((valuta p1 l) && (not (valuta p2 l))) then False else True
