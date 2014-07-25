{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where
    
import ExprT
import Parser
import qualified StackVM as St

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr s = case parsed of
              Nothing -> Nothing
              Just x -> Just $ eval x
    where parsed = parseExp Lit Add Mul s 

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)
          
instance Expr Bool where
    lit x
        | x <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)
          
newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min
          
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
testProg = testExp :: Maybe St.Program

-- ex. 5

instance Expr St.Program where
    lit x = [St.PushI x]
    add x y = [St.Add] ++ x ++ y
    mul x y= [St.Mul] ++ x ++ y

compile :: String -> Maybe St.Program
compile = parseExp lit add mul
