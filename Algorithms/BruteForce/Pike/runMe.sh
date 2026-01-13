#!/bin/bash
# Algorithms/BruteForce/Pike/runMe.sh

cd "$(dirname "$0")"

# Add Pike to PATH (macOS Homebrew)
[ -d /opt/homebrew/Cellar/pike/8.0.1956/bin ] && export PATH="/opt/homebrew/Cellar/pike/8.0.1956/bin:$PATH"

LANGUAGE="Pike"
SOLVER_BINARY="pike Sudoku.pike"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain pike
}

main "$@"
