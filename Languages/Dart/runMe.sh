#!/bin/bash
# Dart Sudoku Solver Benchmark Runner
# Uses common.sh for shared benchmark functionality

LANGUAGE="Dart"
SOLVER_BINARY="dart Sudoku.dart"

# Source common functions
source ../common.sh

# Check for Dart
check_toolchain "dart"

# Override compile (Dart JIT compilation, no explicit compile needed)
compile() {
    echo "Dart uses JIT compilation, no explicit compilation needed" >&2
}

# Run main
main "$@"
