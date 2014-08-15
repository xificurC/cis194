{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where
    
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor
import Data.List (foldl')

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
        
concJ :: JoinList m [a] -> [a]
concJ = foldJ [] (\_ a -> a) (\_ j1 j2 -> j1 ++ j2)

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
          
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n j | tagSize j <= n = Nothing
indexJ _ j@(Single _ v) = Just v
indexJ n (Append _ j1 j2)
       | tagSize j1 > n = indexJ n j1
       | otherwise = indexJ (n-(tagSize j1)) j2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ n j | n <= 0 = j
          | tagSize j <= n = Empty
dropJ _ Empty = Empty
dropJ _ j@(Single _ _) = Empty
dropJ n (Append _ j1 j2)
      | tagSize j1 < n = dropJ (n-(tagSize j1)) j2
      | tagSize j1 == n = j2
      | otherwise = Append (tag newJ1 <> tag j2) newJ1 j2
      where newJ1 = dropJ n j1
                    
takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n j | tagSize j <= n = j
takeJ _ Empty = Empty
takeJ _ j@(Single _ _) = j
takeJ n (Append _ j1 j2)
      | tagSize j1 > n = takeJ n j1
      | tagSize j1 == n = j1
      | otherwise = Append (tag j1 <> tag newJ2) j1 newJ2
      where newJ2 = takeJ (n-(tagSize j1)) j2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
    toString = concJ
    fromString = (foldl' (\j s -> let newJ = Single (scoreString s, Size 1) s
                                in Append (tag j <> tag newJ) j newJ)
                 Empty)
                 . lines
    line n j = indexJ n j
    replaceLine n s j
                    | tagSize j <= n = j
                    | otherwise = takeJ (n-1) j
                                  +++ Single (scoreString s, Size 1) s
                                  +++ dropJ n j
    numLines = getSize . snd . tag
    value = getScore . fst . tag

main = runEditor editor
       (fromString (unlines
                    [ "This buffer is for notes you don't want to save, and for"
                    , "evaluation of steam valve coefficients."
                    , "To load a different file, type the character L followed"
                    , "by the name of the file."
                    ]
                   ) :: JoinList (Score, Size) String)
