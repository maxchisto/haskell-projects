module Calc where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalM :: ExprT -> Maybe Integer
evalM x = Just (eval x)

evalStr :: String -> Maybe Integer
evalStr s = (parseExp Lit Add Mul s) >>= evalM

reify :: ExprT -> ExprT
reify = id

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y
     


