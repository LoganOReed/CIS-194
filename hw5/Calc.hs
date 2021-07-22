{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

------------------
-- EXERCISE ONE --
------------------

-- Takes an ExprT and returns the int it
--  evaluates to
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

------------------
-- EXERCISE TWO --
------------------

-- Maybe = Just a | Nothing

-- Takes a arithmatic string and returns the 
--  calculated expression
evalStr :: String -> Maybe Integer
evalStr str = case strExp of
    Just x ->  Just (eval x)
    Nothing  -> Nothing
    where strExp = parseExp Lit Add Mul str

--------------------
-- EXERCISE THREE --
--------------------

class (Num a) => Expr a where
    lit :: a -> a
    lit x = x
    add :: a -> a -> a
    add x y = x + y
    mul :: a -> a -> a
    mul x y = x * y

