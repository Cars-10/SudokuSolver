#!/bin/bash
# Languages/Wren/runMe.sh
# NOTE: Wren CLI only provides x86_64 binaries

cd "$(dirname "$0")"

# Add Wren CLI to PATH if available
[ -d /usr/local/bin/wren ] && export PATH="/usr/local/bin/wren:$PATH"

LANGUAGE="Wren"
SOLVER_BINARY="wren Sudoku.wren"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../common.sh

compile() {
    check_toolchain wren
}

main "$@"
