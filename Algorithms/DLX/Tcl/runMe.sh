#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Tcl"
SOLVER_BINARY="./dlx.tcl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain tclsh
    chmod +x dlx.tcl
}

main "$@"
