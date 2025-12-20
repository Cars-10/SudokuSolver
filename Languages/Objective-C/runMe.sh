#!/bin/bash
# Languages/Objective-C/runMe.sh

LANGUAGE="Objective-C"
SOLVER_BINARY="./sudoku"

# Source shared functions
source ../common.sh

compile() {
    echo "Compiling Objective-C solver..."
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS native compilation
        clang -O3 -o sudoku sudoku.m -framework Foundation
    else
        # Linux/GNUStep fallback
        OBJC_FLAGS=$(gnustep-config --objc-flags)
        OBJC_LIBS=$(gnustep-config --objc-libs)
        gcc -o sudoku sudoku.m $OBJC_FLAGS $OBJC_LIBS -lgnustep-base
    fi
    
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"