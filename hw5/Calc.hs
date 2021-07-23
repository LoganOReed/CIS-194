{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import VarExprT
import qualified Data.Map as M
import Data.Maybe

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

newtype MinMax = 
    MinMax Integer 
    deriving (Eq, Show)

newtype Mod7 = 
    Mod7 Integer 
    deriving (Eq, Show)

instance Expr Integer where
    lit x = x
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x
        | x > 0     = True
        | otherwise = False
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool    = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM      = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat     = testExp :: Maybe Mod7

------------------
-- EXERCISE SIX --
------------------

-- type class to contain variables
class HasVars a where
    var :: String -> a

-- instance of Expr that uses varexprt
instance Expr VarExprT where
    lit = VarExprT.Lit
    add = VarExprT.Add
    mul = VarExprT.Mul

instance HasVars VarExprT where
    var = VarExprT.Var

type MapExprT = M.Map String Integer -> Maybe Integer

instance Expr MapExprT where
    lit x = (\_ -> Just x)
    add f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                    True -> Nothing
                    _    -> Just (fromJust (f m) + fromJust (g m))
    mul f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                    True -> Nothing
                    _    -> Just (fromJust (f m) * fromJust (g m))
    
-- says variables can be interpreted as 
--  functions from a mapping of var to Ints 
--  to, possibly, integer values
instance HasVars MapExprT where
    var = M.lookup  -- a member function from Data.Map

-- from assignment for testing
withVars :: [(String, Integer)] -> MapExprT -> Maybe Integer
withVars vs expr = expr $ M.fromList vs