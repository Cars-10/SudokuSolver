#!/bin/bash
# Languages/Haxe/runMe.sh
# Haxe Sudoku Solver Runner
#
# Haxe is a multi-target compiler that can produce JavaScript, C++, Java, C#, Python, PHP, Lua, etc.
# We use the interpreter mode (--interp) for simplicity - no compilation step needed.
#
# Usage: ./runMe.sh [matrix_files...]

LANGUAGE="Haxe"
cd "$(dirname "$0")"

# Check for haxe
if ! command -v haxe &> /dev/null; then
    echo "ERROR: haxe not found in PATH" >&2
    exit 1
fi

# Haxe runs in interpreter mode - the solver is invoked directly via haxe
# SOLVER_BINARY needs to be a single command that the shell will execute
SOLVER_BINARY="haxe --cwd $(pwd) --run Sudoku"

source ../../common.sh

# Main execution
main "$@"
