{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

-- | The intent of this data structure is to directly represent append 
-- operations as data constructors. This has the advantage of making append an
-- O(1) operation - sticking two JoinLists together simply involves applying the
-- Append data constructor.
-- The m parameter will be used to track monoidal annotations to the structure.
-- The idea is that the annotation at the root of a JoinList will always be 
-- equal to the combination of all the annotations on the Single nodes
-- Empty nodes do not explicitly store an annotation but we consider them to
-- have an annotation of mempty (that is the identity element for the given 
-- monoid).
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived
-- from those of the two arguments
-- It uses mappend (<>) to combine the annotations
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- indexJ finds the JoinList element at the specified index. If the
-- index is out of bounds, the function returns Nothing. By an index
-- in a JoinList we mean the index in the list ([]) that it represents.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ n (Single _ _) = Nothing
indexJ i (Append _ l r)
  | ls > i = indexJ i l
  | otherwise = indexJ (i - ls) r
  where ls = getSize . size $ tag l
  
-- The dropJ function drops the first n elements from a JoinList.
-- This is analogous to the standard drop function on lists.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n _
  | n <= 0 = Empty
dropJ _ Empty = Empty
dropJ 0 (Single m a) = Single m a
dropJ n (Single _  _) = Empty
dropJ n jl@(Append m l r)
  | n <= 0  = jl
  | n >= (getSize $ size m) = Empty
  | n <= ls = dropJ n l +++ r
  | n > ls  = dropJ (n - ls) r
  where ls = getSize . size $ tag l
  
-- The takeJ function returns the first n elements of a JoinList,
-- dropping all other elements. Again, this function works similarly
-- to the standard library take function
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _
  | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ n (Single m a) = Single m a 
takeJ n jl@(Append m l r)
  | n >= (getSize $ size m) = jl
  | n <= ls = takeJ n l
  | n > ls  = l +++ takeJ (n - ls) r
  where ls = getSize . size $ tag l

-- Calculate the score of a line, then make a JoinList Score String from that
-- line
scoreLine :: String -> JoinList Score String
scoreLine e = Single (scoreString e) e


-- A Buffer instance for the type JoinList (Score, Size) String
instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  
  fromString s  = foldr (+++) Empty $ map single $ lines s
    where single str = Single (scoreString str, 1) str
  
  line n b     = indexJ n b
-- 0 1 2
  -- replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
  replaceLine n l b 
    | n < 0 || n >= numLines b = b
    | otherwise = takeJ n b +++ single l +++ dropJ (n + 1) b
      where single str = Single (scoreString str, 1) str
  --replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
      --where replaceLine' pre [] = pre
            --replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = getSize . snd . tag 
  
  value        = getScore . fst . tag

  
-- | Safe indexing function for regular list, for testing purposes
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- | Convert a JoinList m a to a regular list [a]
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- | A JoinList for testing
jl :: JoinList Size Char
jl = Append (Size 4)
    (Append (Size 3)
    (Single (Size 1) 'y')
  (Append (Size 2)
  (Single (Size 1) 'e')
  (Single (Size 1) 'a')))
  (Single (Size 1) 'h')

