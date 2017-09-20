------------------------------------------------------------------------------
--- This module defines a simple parser for the output of a DIMACS solver.
---
--- @author Sven Hueser
--- @version September 2017
------------------------------------------------------------------------------

module Dimacs.Parser where

import DetParser
import Dimacs.Types
import Dimacs.Scanner


parse :: String -> Either ParseError [Boolean]
parse s = case parseDimacs $ scan s of
  Left e        -> Left e
  Right ([], x) -> Right x
  Right _       -> Left "incomplete parse"

parseDimacs :: Parser Token [Boolean]
parseDimacs [] = eof []
parseDimacs (t:ts) = case t of
  KW_sat   -> some parseVar <* terminal EOF $ ts
  KW_unsat -> terminal EOF *> yield [] $ ts
  _        -> unexpected t ts

parseVar :: Parser Token Boolean
parseVar [] = eof []
parseVar (t:ts) = case t of
  VarNot   -> Not <$> parseVar $ ts
  VarNum i -> yield (Var i) ts
  _        -> unexpected t ts
