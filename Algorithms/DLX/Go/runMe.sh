#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Go"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain go
    go build -o dlx_solver dlx.go
}

main "$@"
