{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

data Stream a = Cons a (Stream a)
              
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs
                           
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)
                                            
rulerGenerator :: Integer -> Stream Integer
rulerGenerator x = interleaveStreams (streamRepeat x) (rulerGenerator (x+1))

ruler :: Stream Integer
ruler = rulerGenerator 0

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger x = Cons x (streamRepeat 0)
    negate (Cons x xs) = Cons (-x) (negate xs)
    (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs+ys)
    (*) (Cons x0 xs) y@(Cons y0 ys) = Cons (x0*y0) (streamMap (x0*) ys + (xs*y))

instance Fractional (Stream Integer) where
    (/) (Cons x xs) (Cons y ys) = q
        where q = Cons (x `div` y) (streamMap (`div` y) (xs - q*ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
