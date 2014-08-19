module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List (sortBy)
import Data.Ord (comparing)
    
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL emps funs) = GL (emp:emps) (fun+funs)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 funs1) (GL emps2 funs2)
        = GL (emps1 ++ emps2) (funs1 + funs2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: Monoid b => (a -> b -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (mconcat (map (treeFold f) (subForest t)))
               
oneSubtree :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
oneSubtree emp@(Emp _ fun) (gl1, gl2@(GL emps2 funs2)) 
    = (GL (emp:emps2) (fun+funs2), moreFun gl1 gl2)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gll = foldr (\(a1,b1) (a2,b2) -> (max a1 a2, max b1 b2))
                    (mempty, mempty)
                    (zipWith oneSubtree (repeat emp) gll)
                    
foldForFun :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
foldForFun emp glt = nextLevel emp [glt]


maxFun :: Tree Employee -> GuestList
maxFun t = moreFun g1 g2
    where (g1,g2) = treeFold foldForFun t
                    
sortEmployeeList :: [Employee] -> [Employee]
sortEmployeeList = sortBy (comparing empName)
                   
listEmployeesSorted :: [Employee] -> String
listEmployeesSorted = unlines . (map empName) . sortEmployeeList
                    
reportGuestList :: GuestList -> String
reportGuestList (GL l f) = "Total fun: " ++ show f ++ "\n" ++ listEmployeesSorted l

main :: IO ()
main = do
  s <- readFile "company.txt"
  let t :: Tree Employee
      t = read s
  putStr . reportGuestList . maxFun $ t
