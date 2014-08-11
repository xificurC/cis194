module JoinList where
    
import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b
            
tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n j | tagSize j > n = Nothing
indexJ _ j@(Single _ v) = Just v
indexJ n (Append _ j1 j2)
       | tagSize j1 > n = indexJ n j1
       | otherwise = indexJ (n-(tagSize j1)) j2
