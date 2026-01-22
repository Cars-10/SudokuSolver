#!/bin/bash
set -e
SOLVER_BINARY="./fennel"
$SOLVER_BINARY Sudoku.fnl "$@"
