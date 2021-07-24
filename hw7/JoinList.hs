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
    | n <= 0           = Nothing
    | n <= (getSize (size (tag x1))) = indexJ n x1
    | otherwise        = indexJ (n - (getSize . size $ tag x1)) x2
