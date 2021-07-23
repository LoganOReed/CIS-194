{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

------------------
-- EXERCISE ONE --
------------------

-- recursive definition to find nth fib term
fib :: Integer -> Integer
fib n 
    | n == 0 || n == 1 = n
    | otherwise        =
        fib (n - 1) + fib (n - 2)

-- lazy evaluation of the list, exponential time
fibs1 :: [Integer]
fibs1 = map fib [0 ..]

------------------
-- EXERCISE TWO --
------------------

-- takes the index and the most recent val and returns
--  the value at the index 
fib2 :: Integer -> Integer -> [Integer]
fib2 a b = a : fib2 b (a + b)


-- a lazy evaluation of fib, has to run in linear time
fibs2 :: [Integer]
fibs2 = fib2 0 1 