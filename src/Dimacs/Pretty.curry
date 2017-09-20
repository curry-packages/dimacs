------------------------------------------------------------------------------
--- This module defines operations to show Boolean formulas in DIMACS format.
---
--- @author  Michael Hanus, Sven Hueser
--- @version September 2017
------------------------------------------------------------------------------

module Dimacs.Pretty ( showDimacs, prettyDimacs, prettySolution )
 where

import Pretty

import Dimacs.Types
import Dimacs.Build

--- Shows a Boolean formula in DIMACS format.
showDimacs :: Boolean -> String
showDimacs b = prettyDimacs (maxVar b) b

--- Pretty print a Boolean formula (second argument)
--- with a given number of variables (first argument) in DIMACS format.
prettyDimacs :: Int -> Boolean -> String
prettyDimacs nv = pPrint . (ppDimacs nv) . toCNF

--- Pretty print a solution of a Boolean formula.
prettySolution :: Boolean -> String
prettySolution = pPrint . (line <>) . ppCNF

ppDimacs :: Int -> Boolean -> Doc
ppDimacs nv b =
  let nd = case b of
            Or  _ -> 1
            And l -> length l
            _     -> error "Dimacs.Pretty: need formula in CNF"
  in (text "p cnf" <+> int nv <+> int nd) $$ (ppCNF b)

ppCNF :: Boolean -> Doc
ppCNF b = case b of
  Var  i -> int i
  Not  n -> text "-" <> ppCNF n
  And bs -> vsep $ map ppCNF bs
  Or  bs -> (hsep $ map ppCNF bs) <+> text "0"
