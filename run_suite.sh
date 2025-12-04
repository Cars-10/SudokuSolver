#!/bin/bash
# Wrapper script to run the Sudoku Benchmark Suite
# Usage: ./run_suite.sh [language] [matrix]
# Example: ./run_suite.sh Python 1
# Example: ./run_suite.sh all all
# Example: ./run_suite.sh C 1,2

cd "$(dirname "$0")/Metrics"
npx ts-node --esm run_suite.ts "$@"
