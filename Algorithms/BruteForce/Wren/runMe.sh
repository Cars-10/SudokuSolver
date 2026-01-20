#!/bin/bash
# Algorithms/BruteForce/Wren/runMe.sh

cd "$(dirname "$0")"

# Add Wren CLI to PATH (macOS Homebrew uses wren_cli, Docker uses wren)
if [ -x /opt/homebrew/Cellar/wren-cli/0.4.0/bin/wren_cli ]; then
    export PATH="/opt/homebrew/Cellar/wren-cli/0.4.0/bin:$PATH"
    WREN_CMD="wren_cli"
elif command -v wren &> /dev/null; then
    WREN_CMD="wren"
else
    WREN_CMD="wren"  # Will fail with toolchain check
fi

LANGUAGE="Wren"
SOLVER_BINARY="$WREN_CMD Sudoku.wren"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain "$WREN_CMD"
}

main "$@"
