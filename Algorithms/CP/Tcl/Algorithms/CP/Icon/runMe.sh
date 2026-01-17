#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Icon"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain icont
    icont -o cp cp.icn
}

main "$@"
