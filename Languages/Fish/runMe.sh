#!/bin/bash
# Languages/Fish/runMe.sh

LANGUAGE="Fish"
FISH="/opt/homebrew/bin/fish"

# Source shared functions
source ../common.sh

compile() {
    if [ ! -x "$FISH" ]; then
        if command -v fish &> /dev/null; then
            FISH="fish"
        else
            report_env_error "fish not found at $FISH or in PATH"
        fi
    fi
}

main() {
    compile
    
    # SOLVER_BINARY must be set after compile has potentially updated FISH path
    SOLVER_BINARY="$FISH Sudoku.fish"
    
    # Run benchmarks
    run_benchmarks "$@"
}

# Execute benchmarks
main "$@"
