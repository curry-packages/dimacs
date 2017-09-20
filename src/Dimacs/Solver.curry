------------------------------------------------------------------------------
--- This module defines an operation to solve a Boolean formula
--- with some SAT solver supporting the DIMACS format.
--- It also defines configuration for various solvers.
---
--- @author  Michael Hanus
--- @version September 2017
------------------------------------------------------------------------------

module Dimacs.Solver where

import IO
import IOExts

import Dimacs.Parser ( parse )
import Dimacs.Pretty ( showDimacs )
import Dimacs.Types

------------------------------------------------------------------------------
--- Type of solvers configuration with fields for the name of the executable
--- and command line flags.
data SolverConfig = Config
                      { executable  :: String
                      , flags       :: [String]
                      }

--- The configuration of the Z3 solver for DIMACS.
z3Dimacs :: SolverConfig
z3Dimacs = Config { executable = "z3"
                  , flags = ["-in", "-dimacs"]
                  }

--- The configuration of the lingeling solver.
lingeling :: SolverConfig
lingeling = Config { executable = "lingeling"
                   , flags = ["-q"]
                   }

------------------------------------------------------------------------------
--- Checks the satisfiability of a Boolean formula with a SAT solver
--- supporting DIMACS format.
--- A list associating the variable indices to Boolean values so that
--- the formula is satisfied is returned.
--- If the formula is unsatisfiable, the returned list is empty.
solveWithDimacs :: SolverConfig -> Boolean -> IO [(Int,Bool)]
solveWithDimacs scfg boolean = do
  let satcmd = unwords $ executable scfg : flags scfg
  (inH, outH, _) <- execCmd satcmd
  hPutStr inH $ showDimacs boolean
  hFlush inH
  hClose inH
  response <- hGetContents outH
  case parse response of
    Left  e -> error e
    Right b -> return (boolVars2AssocList b)

--- Translates a list of Boolean variables into a list associating
--- the variable indices to Boolean values.
boolVars2AssocList :: [Boolean] -> [(Int,Bool)]
boolVars2AssocList = map bvar2assoc
 where
  bvar2assoc bv = case bv of
    Var i       -> (i,True)
    Not (Var i) -> (i,False)
    _           -> error $ "boolVars2AssocList: not a variable: " ++ show bv

------------------------------------------------------------------------------