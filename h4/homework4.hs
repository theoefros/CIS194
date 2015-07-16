import Data.List
{-
Reimplement each of the following functions in a more idiomatic Haskell style. 
Use wholemeal programming practices, breaking each function into a pipeline of 
incremental transformations to an entire data structure. Name your functions
fun1' and fun2' respectively.
-}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs
    
fun1' :: [Integer] -> Integer
fun1' = foldr (\e a -> if even e then (e - 2)*a else a) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . 
    iterate (\a -> if even a then a `div` 2 else (3 * a + 1))
    
{-
Folding with trees
Recall the definition of a binary tree data structure. The height of a binary
tree is the length of a path from the root to the deepest node. For example, 
the height of a tree with a single node is 0; the height of a tree with three
nodes, whose root has two children, is 1; and so on. 
A binary tree is balanced if the height of its left and right subtrees differ by
no more than 1, and its left and right subtrees are also balanced. You should 
use the following data structure to represent binary trees. Note that each node
stores an extra Integer representing the height at that node.
For this exercise, write a function
foldTree :: [a] -> Tree a
foldTree = ...
which generates a balanced binary tree from a list of values using foldr
-}
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

singletonTree :: a -> Tree a
singletonTree a = Node 1 Leaf a Leaf

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

height' :: Tree a -> Integer
height' Leaf = 0
height' (Node _ l _ r) = 1 + max (height' l) (height' r)

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) =  
    abs ((height l) - (height r)) <= 1 && isBalanced l && isBalanced r
    
isBalanced' :: Tree a -> Bool
isBalanced' Leaf = True
isBalanced' (Node _ l _ r) =  
    abs ((height' l) - (height' r)) <= 1 && isBalanced' l && isBalanced' r
    
listToTree :: [a] -> Tree a
listToTree = foldr treeIns Leaf 

treeIns :: a -> Tree a -> Tree a
treeIns a Leaf = singletonTree a
treeIns a (Node h Leaf b Leaf) = Node (h + 1) (singletonTree a) b Leaf
treeIns a (Node h l b Leaf) = Node h l b (singletonTree a)
treeIns a (Node h Leaf b r) = Node h (singletonTree a) b r
treeIns a (Node h l b r)
    | leftH == rightH = Node (1 + height newL) newL b r
    | leftH > rightH  = Node h l b newR
    | leftH < rightH  = Node h newR b r
    where leftH = height l
          rightH = height r
          newL = treeIns a l
          newR = treeIns a r
          
{-
Implement a function
    xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True values 
contained in the input list. It does not matter how many False values the input
list contains
-}
xor :: [Bool] -> Bool 
xor = foldr (\e a -> if e then not a else a) False

{-
Implement map as fold
-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e a -> f e : a) []

-- foldl implemet using foldr
-- foldr f acc es = f e1 (f e2 (f en acc))
-- (1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+0)))))))))))))
-- foldl f acc es = f (f (f acc e1) e2) en
-- (((((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)+11)+12)+13)

{-
Read about the Sieve of Sundaram. 
Implement the algorithm using function composition. Given an integer n, your 
function should generate all the odd prime numbers up to 2 n + 2.

Algorithm:
Start with a list of the integers from 1 to n. From this list, remove all 
numbers of the form i + j + 2ij where:

    i,j in {N},  1 <= i <= j
    i + j + 2*i*j <= n

The remaining numbers are doubled and incremented by one, giving a list of the odd prime numbers (i.e., all primes except 2) below 2n + 2.
-}

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (map ((+1).(*2)) $ [1..n] \\ removed)
    where removed = [ i + j + 2*i*j | i <- [1..10], j <- [i..(2*n + 2)], (i + j + 2*i*j) <= n ]