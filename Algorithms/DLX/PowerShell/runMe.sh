#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="PowerShell"
SOLVER_BINARY="pwsh dlx.ps1"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain pwsh
    return $?
}

main "$@"
