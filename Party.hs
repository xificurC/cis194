module Party where

import Employee
import Data.Monoid
    
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL emps funs) = GL (emp:emps) (fun+funs)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 funs1) (GL emps2 funs2) = GL (emps1 ++ emps2)
                                                (funs1 + funs2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max
