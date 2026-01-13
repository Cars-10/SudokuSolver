#!/bin/bash
# Languages/Cobol/runMe.sh

LANGUAGE="Cobol"
SOLVER_BINARY="./Sudoku"

# Source shared functions
source ../common.sh

compile() {
    # Prefer /usr/bin/cobc as specified
    COBC="/usr/bin/cobc"
    if [ ! -x "$COBC" ]; then
        if command -v cobc &> /dev/null; then
            COBC="cobc"
        else
            report_env_error "GnuCOBOL compiler (cobc) not found"
        fi
    fi

    echo "Compiling COBOL solver..."
    # -x: Build executable
    # -free: Free format source
    "$COBC" -x -free -o Sudoku sudoku.cob
    
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"