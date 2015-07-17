module Party where


import Employee
import Data.Monoid
import Data.Tree
import Data.List (foldr1)


bob = Emp { empName = "Bob", empFun = 1}
alice = Emp { empName = "Alice", empFun = 2}
eve = Emp { empName = "Eve", empFun=3}
carol = Emp {empName = "Carol", empFun = 4}
dave = Emp {empName="Dave", empFun = 5}
trudy = Emp {empName="Trudy", empFun = 6}

empList1 = [bob, alice, eve]
empList2 = [carol, dave, trudy]

gl1 = GL empList1 (sum $ map empFun empList1)
gl2 = GL empList2 (sum $ map empFun empList2)

-- Exercise 1 

-- | A function which adds an Employee to the GuestList (updating the cached Fun
-- score appropriately) Of course, in general this is impossible:
-- the updated fun score should depend on whether the Employee
-- being added is already in the list, or if any of their direct subordinates
-- are in the list, and so on. For our purposes, though, you
-- may assume that none of these special cases will hold: that is,
-- glCons should simply add the new Employee and add their fun
-- score without doing any kind of checks.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

-- A monoid instace for GuestList
-- Concatenate the lists and add the fun
instance Monoid GuestList where 
  mempty = GL [] 0
  (GL xs f1) `mappend` (GL ys f2) = GL (xs ++ ys) (f1 + f2)

-- A function which takes two GuestLists and returns whichever one of them
-- is more fun, i.e. has the higher fun score. (If the scores are equal it
-- does not matter which is returned.)
-- Since GuestList is an instance of Ord type class, and they are ordered by fun
-- then the max function can be used
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

-- Implement the fold function for Data.Tree
treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f acc (Node x []) = f x acc
treeFold f acc (Node x (y)) = f x (map (treeFold f acc) y)

-- Exercise 3

-- A function that takes two arguments - the first is the boss of the current
-- subtree. The second is a list of the results for each subtree under the boss.
-- Each result is a pair of GuestLists: the first GL in the pair is the best
-- possible guest list with the boss of that subtree; the second is the best 
-- possible guest list without the boss of that subtree. nextLevel should then 
-- compute the overall best guest list that includes boss and the overall best 
-- guest list that doesn't include boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gl = (glCons emp woBoss, wBoss)
  where (wBoss, woBoss) = mconcat gl
        
-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel []


