{-# OPTIONS_GHC -Wall #-}

module Wholemeal where

-----------------------------
-- EXERCISE ONE:
--  Reimplement the functions with wholemeal programming
--  Principles
-----------------------------

-- multiply two less than all even numbers
fun1 :: [Integer] -> Integer

fun1 []         = 1
fun1 (x:xs) 
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even

---------------------------------------------------------------------

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- ideas:
--  use takewhile even iterate 3*x+1 x to automatically do the recursion
fun2' :: Integer -> Integer
fun2' = 
    foldr (+) 0 
    . filter even
    . takeWhile (/= 1)
    . iterate (\x -> if even x 
                     then x `div` 2
                     else 3 * x + 1) 

----------------------------------
-- EXERCISE TWO:
-- foldTree generates a balanced binary tree
--      from a list of values using foldr
----------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
--foldTree :: [a] -> Tree a

-----------------------------------
-- EXERCISE THREE:
-- a bunch of functions defined using fold
-----------------------------------

xor :: [Bool] -> Bool
xor = foldr 
        (\x y ->    if x /= y
                    then True
                    else False)
        False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

