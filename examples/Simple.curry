-- Examples for testing the satisfiability of Boolean formulas.

import Dimacs.Build
import Dimacs.Solver
import Dimacs.Types

-- A satisfiable formula.
b1 :: Boolean
b1 = (va 1 .\/ va 2) ./\ no (va 3)

solveb1 :: IO [(Int,Bool)]
solveb1 = solveWithDimacs z3Dimacs b1

-- An unsatisfiable formula.
b2 :: Boolean
b2 = (va 1 .\/ va 2) ./\ (va 1 .\/ no (va 2)) ./\
     (no (va 1) .\/ va 2) ./\ (no (va 1) .\/ no (va 2))

solveb2 :: IO [(Int,Bool)]
solveb2 = solveWithDimacs z3Dimacs b2

