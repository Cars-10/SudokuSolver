#!/bin/bash
# Algorithms/BruteForce/Rexx/runMe.sh
# Rexx (Regina) Sudoku Solver Runner
#
# Rexx is an IBM scripting language from 1979, known for human-readable syntax.
# Regina is the open-source implementation.
#
# Usage: ./runMe.sh [matrix_files...]

LANGUAGE="Rexx"
cd "$(dirname "$0")"

# Check for regina
if ! command -v regina &> /dev/null; then
    echo "ERROR: regina not found in PATH" >&2
    exit 1
fi

# Regina runs scripts with full path
SOLVER_BINARY="regina $(pwd)/Sudoku.rexx"

source ../../common.sh

# Main execution
main "$@"
