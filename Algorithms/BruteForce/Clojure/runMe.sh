#!/bin/bash
# runMe.sh - Clojure Sudoku Solver Benchmark
# Created: 2025-12-18

# Language configuration
LANGUAGE="Clojure"

# Setup Java environment (macOS Homebrew)
if [ -d "/opt/homebrew/opt/openjdk" ]; then
    export JAVA_HOME="/opt/homebrew/opt/openjdk"
    export PATH="$JAVA_HOME/bin:$PATH"
fi

SOLVER_BINARY="clojure -M Sudoku.clj"
COMPILE_CMD=""  # No compilation needed for Clojure

# Source common benchmark functions
source ../../common.sh

# Check if Clojure is available
check_toolchain clojure

# Run benchmarks
main "$@"
