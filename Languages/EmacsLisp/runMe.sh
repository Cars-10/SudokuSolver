#!/bin/bash
# Languages/EmacsLisp/runMe.sh

LANGUAGE="EmacsLisp"
SOLVER_BINARY="emacs --script sudoku.el"

source ../common.sh
main "$@"