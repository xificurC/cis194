module Hw4 where
    
import Data.List
import Data.Ord

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even
        
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- fold a tree

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

addNode :: a -> Tree a -> Tree a
addNode x tree = nextTree x tree (nextLeaf tree)

data Direction = DLeft | DRight

nextLeaf :: Tree a -> [Direction]
nextLeaf Leaf = []
nextLeaf (Node _ Leaf _ _) = [DLeft]
nextLeaf (Node _ _ _ Leaf) = [DRight]
nextLeaf (Node _ left _ right) = minimumBy (comparing length) [DLeft : (nextLeaf left), DRight : (nextLeaf right)]
                                 
nextTree :: a -> Tree a -> [Direction] -> Tree a
nextTree x _ [] = Node 0 Leaf x Leaf
nextTree x (Node h left v right) dir@(d:ds) = case d of
                                                DLeft -> Node newH (nextTree x left ds) v right
                                                DRight -> Node newH left v (nextTree x right ds)
    where newH = h + (if length dir > (fromInteger h) then 1 else 0)
