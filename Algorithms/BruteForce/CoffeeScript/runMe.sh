#!/bin/bash
# CoffeeScript Sudoku Solver Benchmark Runner

cd "$(dirname "$0")"

# Add CoffeeScript to PATH (macOS Homebrew)
[ -d /opt/homebrew/Cellar/coffeescript/2.7.0/bin ] && export PATH="/opt/homebrew/Cellar/coffeescript/2.7.0/bin:$PATH"

LANGUAGE="CoffeeScript"
SOLVER_BINARY="coffee Sudoku.coffee"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source common functions
source ../../common.sh

compile() {
    check_toolchain coffee
}

# Run benchmarks
main "$@"
