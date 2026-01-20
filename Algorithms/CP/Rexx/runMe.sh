#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Rexx"
SOLVER_BINARY="rexx cp.rexx"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain rexx
    chmod +x cp.rexx
}

main "$@"
