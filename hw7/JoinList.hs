{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

------------------
-- EXERCISE ONE --
------------------

-- data type that allows us to reduce the problem of metadata tracking
--  to log n time
data JoinList m a 
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- helper function for (+++)
-- gets the annotation at the root of the joinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- append function for JoinList that updates monoidal annotation
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x1 x2 = Append (mappend (tag x1) (tag x2)) x1 x2

------------------
-- EXERCISE TWO --
------------------

-- Returns the JoinList elem at the specified index if it exists
-- I'm not really sure how Sized works, but I think each node, not
-- just leaves, have indices
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
-- CASE: Somehow got past Single, which isn't supposed to happen
indexJ _ Empty        = Nothing
-- CASE: If I end up at a Single with the correct index
indexJ 1 (Single _ x) = Just x
-- CASE: If I end up at a Single with any other index
indexJ _ (Single _ _) = Nothing
-- CASE: If not a leaf, go to right if index is greater than left
--  else go left
indexJ n (Append _ x1 x2) 
    | n <= 0                         = Nothing
    | n <= getSize (size (tag x1)) = indexJ n x1
    | otherwise                      = indexJ (n - (getSize . size $ tag x1)) x2

-- FUNCTIONS FROM HW FOR TESTING

-- safe list indexing function
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

-- Converts JL to list
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- transforms a tree into a JL
stringToJL :: String -> JoinList Size Char
stringToJL []     = Empty
stringToJL [x]    = Single (Size 1) x
stringToJL (x:xs) = stringToJL [x] +++ stringToJL xs

-- Takes a string and its index and returns the char at that index
findChar :: Int -> String -> Maybe Char
findChar n x = indexJ n (stringToJL x)

--------------------
-- EXERCISE THREE --
--------------------

-- Implementation of drop function
-- I'm going to assume that if root has a size of n, there exists
--  for every i < n some Node that has that size
dropJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> JoinList b a
dropJ n x
    | n <= 0                        = x
    | n >= getSize (size (tag x)) = x
dropJ n (Append _ _ x2) = dropJ n x2
dropJ _ _ = Empty

-- STEPS: size is value of Size at root, n is input
-- IF size < n:
--      RETURN Empty
-- IF size == n:
--      RETURN root
-- IF size > n:
-- return (dropJ' n x1) +++ (dropJ' (n-size(x1) x2)
-- essentially, take as much from the left child and take the 
--      rest from the right one

-- Divide and Conquer Way
dropJ' :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> JoinList b a
dropJ' n x@(Append m x1 x2)
    | n < getSize (size m) = x
    | otherwise              = Empty
dropJ' n x@(Single _ _) = x
dropJ' _ _              = Empty