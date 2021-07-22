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
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

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
    where strExp = parseExp ExprT.Lit ExprT.Add ExprT.Mul str

--------------------
-- EXERCISE THREE --
--------------------

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-------------------
-- EXERCISE FOUR --
-------------------

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x
        | x > 0     = True
        | otherwise = False
    add = (||)
    mul = (&&)

check :: Integer -> Bool -> Bool
check n b = mul (lit n) b