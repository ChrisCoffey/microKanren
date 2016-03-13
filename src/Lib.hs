module Lib where

import Control.Monad
import Control.Applicative (Alternative(..))

type Variable = Integer
type Label = String
data Term = Val Label | Var Variable | Pair Term Term 
    deriving (Show, Eq)
type Strm = [(Variable, Term)]
type State = (Strm, Variable)
data TrampList a = Bounce (TrampList a) | Nil  | Cons a (TrampList a)
    deriving (Show, Eq)
type Expr = State -> TrampList State

instance Functor TrampList where
    fmap = liftM

instance Applicative TrampList where
    pure a = Cons a Nil
    (<*>) = ap

instance Alternative TrampList where
    (<|>) = mplus
    empty = mzero

instance Monad TrampList where
    return a = Cons a Nil
    Nil >>= f = mzero
    Bounce as >>= f = Bounce (as >>= f)
    Cons a as >>= f = f a `mplus` (as >>= f)

instance MonadPlus TrampList where
    mzero = Nil
    Nil `mplus` as = as
    Bounce as `mplus` bs = Bounce (bs `mplus` as)
    (Cons a as) `mplus` bs = Cons a (bs `mplus` as)

toOutput Nil = []
toOutput (Cons a as) = a:(toOutput as)
toOutput (Bounce as) = toOutput as

walk :: Term -> Strm -> Term
walk t@(Var u) s = case lookup u s of 
                    Just x  -> walk x s
                    Nothing -> t
walk u _ = u

unit x = x:mzero

extend :: Term -> Variable -> Strm -> Strm
extend x v s = (v,x):s

(≡) :: Term -> Term -> Expr
l ≡ r = f   where
    f state@(strm, var) = 
        case unify l r strm of
            Just x -> return (x, var)
            Nothing -> mzero
equiv = (≡)
       
unify :: Term -> Term -> Strm -> Maybe Strm
unify u v s = f (walk u s) (walk v s) where
    f (Var a) (Var b) | a == b = return s
    f (Val a) (Val b) | a == b = return s
    f (Var a) v                = return $ extend v a s
    f u (Var b)                = return $ extend u b s
    f (Pair a b) (Pair a' b')  = do
                                  s' <- unify a a' s
                                  unify b b' s'
    f _ _                      = mzero

conj :: Expr -> Expr -> Expr
conj l r = f where f sc = l sc >>= r

disj :: Expr -> Expr -> Expr
disj l r = f where f sc = (l sc) `mplus` (r sc)

callFresh :: (Term -> Expr) -> Expr
callFresh f = g where g (s,c) = f (Var c) (s, c + 1)

wait :: Expr -> Expr
wait f = g where g sc = Bounce (f sc)
