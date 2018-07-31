------------------------------------------------------------------------------
--- This module defines a simple scanner for the output of a DIMACS solver.
---
--- @author Sven Hueser
--- @version September 2017
------------------------------------------------------------------------------

module Dimacs.Scanner where

import Numeric
import Data.Char (isAlpha, isDigit, toLower)

data Token
  -- keywords
  = KW_sat
  | KW_unsat
  -- variables
  | VarNum Int
  | VarNot
  -- other
  | EOF
  deriving (Eq, Show)

keywords :: [(String, Token)]
keywords =
  [ ("sat"            , KW_sat)
  , ("satisfiable"    , KW_sat)
  , ("unsat"          , KW_unsat)
  , ("unsatisfiable"  , KW_unsat)
  ]

keyword :: String -> Token
keyword k = maybe (error $ "unknown keyword: " ++ k) id (lookup (map toLower k) keywords)

scan :: String -> [Token]
scan str = case str of
    ""      -> [EOF]
    ('-':s) -> VarNot : scan s
    ('0':s) -> scan s           -- minisat puts a 0 at the end of the variables, z3 doesn't; ignore it
    ('s':' ':s) -> scan s           -- lingeling puts 's' at start of sat/unsat line
    ('v':' ':s) -> scan s           -- lingeling puts 'v' at start of solution line
    ('c':' ':s) -> scan $ tail $ dropWhile (\c -> c /= '\n') s
    _       -> scan' str
  where
    scan' :: String -> [Token]
    scan' "" = [EOF] -- should not be possible, but removes missing pattern warning
    scan' x@(c:cs) | isDigit c = scanNum x
                   | isAlpha c = scanKeyword x
                   | otherwise  = scan cs

scanKeyword :: String -> [Token]
scanKeyword cs =
  let (key, rest) = span isAlpha cs
  in  keyword key : scan rest

scanNum :: String -> [Token]
scanNum cs =
  case readInt cs of
    [(num, rest)]  -> VarNum num : scan rest
    _              -> error "should not be possible"
