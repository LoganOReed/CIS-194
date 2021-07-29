{-# OPTIONS_GHC -Wall #-}

module Party where

import qualified Employee as E
-- GL [Employee] Fun

-- Adds an Employee to the GuestList
glCons :: E.Employee -> E.GuestList -> E.GuestList
glCons emp (E.GL gs f) = E.GL (emp:gs) (f + (E.empFun emp))