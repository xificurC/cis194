module Party where

import Employee
import Data.Monoid
import Data.Tree
    
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL emps funs) = GL (emp:emps) (fun+funs)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 funs1) (GL emps2 funs2) = GL (emps1 ++ emps2)
                                                (funs1 + funs2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeToList :: Tree a -> [a]
treeToList (Node v s) = v : (concatMap treeToList s)
                        
-- treeFold :: (a -> b -> b) -> b -> Tree a -> b
-- treeFold f z t = foldr f z (treeToList t)

treeFold :: (a -> b -> b) -> Tree a -> b
treeFold f (Node v s) = f v (concatMap (treeFold f) s)
                             
oneSubtree :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
oneSubtree emp@(Emp _ fun) (gl1, gl2@(GL emps2 funs2)) 
    = ( (GL (emp:emps2) (fun+funs2)) , moreFun gl1 gl2)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gll = foldr1 (\(a1,b1) (a2,b2) -> (max a1 a2, max b1 b2))
                    (zipWith oneSubtree (repeat emp) gll)

-- maxFun :: Tree Employee -> GuestList
-- maxFun [] = GL [] 0
-- maxFun empTree = moreFun g1 g2
--     where (g1, g2) = treeFold nextLevel [(GL [] 0, GL [] 0)] empTree
