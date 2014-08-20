module Lecture where

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (flip const)

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr ((liftA2 (:)) . f) (pure [])

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = liftA . replicate
