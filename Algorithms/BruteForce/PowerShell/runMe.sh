#!/bin/bash
# Languages/PowerShell/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="PowerShell"
SOLVER_BINARY="pwsh Sudoku.ps1"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../common.sh

compile() {
    check_toolchain pwsh
}

main "$@"
