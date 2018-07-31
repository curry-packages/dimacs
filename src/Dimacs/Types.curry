------------------------------------------------------------------------------
--- This module defines the data type to represent Boolean formulas
--- together with some auxiliary operations.
---
--- @author  Michael Hanus, Sven Hueser
--- @version September 2017
------------------------------------------------------------------------------

module Dimacs.Types where

import Data.List ( maximum )

--- The type of Boolean formulas.
--- Not that variables should be numbered from 1.
data Boolean = Var Int
             | Not Boolean
             | And [Boolean]
             | Or  [Boolean]
  deriving (Eq, Show)

--- Returns the maximal variable index in a Boolean formula.
maxVar :: Boolean -> Int
maxVar (Var n)  = n
maxVar (Not b)  = maxVar b
maxVar (And bs) = if null bs then 0 else maximum (map maxVar bs)
maxVar (Or  bs) = if null bs then 0 else maximum (map maxVar bs)
