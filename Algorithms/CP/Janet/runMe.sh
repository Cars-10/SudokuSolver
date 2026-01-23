#!/bin/bash
# Algorithms/CP/Janet/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Janet"
SOLVER_BINARY="janet cp.janet"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain janet
}

main "$@"
