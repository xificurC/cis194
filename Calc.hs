module Calc where
    
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr s = case parsed of
              Nothing -> Nothing
              Just x -> Just $ eval x
    where parsed = parseExp Lit Add Mul s 
