{-# Language FlexibleInstances #-}

module JoinList where
    
import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)
                             
foldJ :: b -> (m -> a -> b) -> (m -> b -> b -> b) -> JoinList m a -> b
foldJ e _ _ Empty = e
foldJ _ s _ (Single m a) = s m a
foldJ e s p (Append m j1 j2) = p m (foldJ e s p j1) (foldJ e s p j2)
                               
listJ :: JoinList m a -> [a]
listJ = foldJ [] (\_ a -> [a]) (\_ j1 j2 -> j1 ++ j2)

countJ :: JoinList m a -> Int
countJ = foldJ 0 (\_ _ -> 1) (\_ j1 j2 -> j1 + j2)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b
            
tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag
          
jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a')))
     (Single (Size 1) 'h')

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 j@(Single _ v) = Just v


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ n j | n <= 0 = j
dropJ _ Empty = Empty
dropJ _ j@(Single _ _) = j
