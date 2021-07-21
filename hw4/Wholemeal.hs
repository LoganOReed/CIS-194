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