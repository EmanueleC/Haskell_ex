module Solutions(vars, Prop(..)) where
import Data.List

-- custom data type Prop represents a logical proposition

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop deriving (Show)

-- first function vars returns a list with all variables

-- To be completed...
vars (Const _) = []
vars (Var a) = [a]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ ((vars p2) \\ (vars p1))
vars (Imply p1 p2) = vars p1 ++ ((vars p2) \\ (vars p1))

-- Exercise to be completed...
