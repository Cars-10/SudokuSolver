#!/bin/bash
# Language configuration
LANGUAGE="CommonLisp"
SOLVER_BINARY="./sudoku_lisp"
COMPILE_CMD="sbcl --no-userinit --disable-debugger --eval '(compile-file \"Sudoku.lisp\")' --eval '(load \"Sudoku.fasl\")' --eval '(sb-ext:save-lisp-and-die \"sudoku_lisp\" :toplevel #'\''main :executable t)'"

# Source common benchmark functions
source ../../common.sh

# Check if SBCL is available
check_toolchain sbcl

# Run benchmarks
main "$@"
