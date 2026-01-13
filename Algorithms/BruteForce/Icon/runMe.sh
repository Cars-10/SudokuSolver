#!/bin/bash
# Algorithms/BruteForce/Icon/runMe.sh

cd "$(dirname "$0")"

# Add Icon to PATH if available (Docker or macOS Homebrew)
[ -d /app/server/icon-master/bin ] && export PATH="/app/server/icon-master/bin:$PATH"
[ -d /opt/homebrew/Cellar/icon/9.5.25a/bin ] && export PATH="/opt/homebrew/Cellar/icon/9.5.25a/bin:$PATH"

LANGUAGE="Icon"
SOLVER_BINARY="./sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain icont

    echo "Compiling Icon solver..."
    icont -o sudoku Sudoku.icn
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

main "$@"
