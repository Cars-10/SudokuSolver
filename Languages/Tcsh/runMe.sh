#!/bin/bash
# Languages/Tcsh/runMe.sh

LANGUAGE="Tcsh"
SOLVER_BINARY="tcsh Sudoku.tcsh"

# Source shared functions
source ../common.sh

# Execute benchmarks
main "$@"
