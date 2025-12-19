#!/bin/bash
# Languages/BASIC/runMe.sh

LANGUAGE="BASIC"
SOLVER_BINARY="./Sudoku"

# Source shared functions
source ../common.sh

compile() {
    # Check if fbc is available (prefer /usr/local/bin/fbc as specified by user)
    FBC="/usr/local/bin/fbc"
    if [ ! -x "$FBC" ]; then
        if command -v fbc &> /dev/null; then
            FBC="fbc"
        else
            report_env_error "FreeBASIC compiler (fbc) not found at /usr/local/bin/fbc or in PATH"
        fi
    fi

    echo "Compiling BASIC solver..."
    "$FBC" Sudoku.bas -x Sudoku
    
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"
