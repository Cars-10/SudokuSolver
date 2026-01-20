#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="F_Sharp"
SOLVER_BINARY="dotnet dlx.dll"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain dotnet

    # Use dotnet build with project file
    dotnet build dlx.fsproj -c Release -o . >/dev/null 2>&1 || return 1

    # Make the binary executable
    chmod +x dlx 2>/dev/null || true
    return 0
}

main "$@"
