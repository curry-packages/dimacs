dimacs
======

This package provides an interface to SAT solvers supporting
the DIMACS format.

Boolean formulas can be defined in Curry using the type
`Dimacs.Types.Boolean` and the constructor operations defined in
module `Dimacs.Build`.

Formulas can be checked for satisfiabilty using the operation
`Dimacs.Solver.solveWithDimacs`.

Examples can be found in the directory `examples`.
