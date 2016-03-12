module MicroKanren
    ( (≡),
      equiv,
    ) where

type Variable = Integer
type Label = String
data Term = Val Label | Var Variable | Pair Term Term 
    deriving (Show, Eq)
type Strm = [(Variable, Term)]
type State = (Strm, Variable)
type Expr = State -> [State]
data TrampList a = Bounce (TrampList a) | Nil  | Cons a (TrampList a)
    deriving (Show, Eq)

walk :: Term -> Strm -> Term
walk t@(Var u) s = case lookup u s of 
                    Just x  -> walk x s
                    Nothing -> t
walk u _ = u

unit x = x:mzero
mzero = []

extend :: Term -> Variable -> Strm -> Strm
extend x v s = (v,x):s

(≡) :: Term -> Term -> Expr
l ≡ r = f   where
    f state@(strm, var) = 
        case unify l r strm of
            Just x -> return (strm:state)
            Nothing -> mzero
       
unify :: Term -> Term -> Strm -> Maybe Strm
unify u v s = f ()
