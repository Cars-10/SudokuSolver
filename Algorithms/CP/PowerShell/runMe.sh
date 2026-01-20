#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="PowerShell"
SOLVER_BINARY="pwsh cp.ps1"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"
source ../../common.sh

# Fix for CP directory structure: use ../../../Matrices instead of ../../Matrices
if [ $# -eq 0 ]; then
    set -- ../../../Matrices/*.matrix
fi

compile() {
    check_toolchain pwsh
    return $?
}

main "$@"
