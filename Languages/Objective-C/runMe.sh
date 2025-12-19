#!/bin/bash
# Languages/Objective-C/runMe.sh

LANGUAGE="Objective-C"
SOLVER_BINARY="./sudoku"

# Source shared functions
source ../common.sh

compile() {
    echo "Compiling Objective-C solver..."
    
    # Use gnustep-config to get flags and libs
    OBJC_FLAGS=$(gnustep-config --objc-flags)
    OBJC_LIBS=$(gnustep-config --objc-libs)
    
    # Add -std=c99 for for-loop declarations and other modern C features
    gcc -o sudoku sudoku.m $OBJC_FLAGS $OBJC_LIBS -lgnustep-base
    
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"