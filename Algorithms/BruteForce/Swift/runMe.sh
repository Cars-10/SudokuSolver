#!/bin/bash
# Algorithms/BruteForce/Swift/runMe.sh

LANGUAGE="Swift"
SOLVER_BINARY="./Sudoku"

# Source shared functions
source ../../common.sh

compile() {
    # Check if swiftc is available
    if command -v swiftc &> /dev/null; then
        echo "Compiling Swift solver with swiftc..."
        swiftc -O -o Sudoku Sudoku.swift
    elif [ -x "/root/.local/share/swiftly/bin/swiftc" ]; then
         echo "Compiling Swift solver with /root/.local/share/swiftly/bin/swiftc..."
        /root/.local/share/swiftly/bin/swiftc -O -o Sudoku Sudoku.swift
    else
        report_env_error "Swift compiler (swiftc) not found"
    fi
    
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"