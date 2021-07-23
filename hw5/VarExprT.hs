module VarExprT where

import ExprT

-- extension of ExprT with variables added
data VarExprT   = Lit Integer
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT
                | Var String
  deriving (Show, Eq)
