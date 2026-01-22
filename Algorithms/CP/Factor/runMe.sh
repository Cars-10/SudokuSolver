#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Factor"
SOLVER_BINARY="/Users/vibe/Downloads/factor/Factor.app/Contents/MacOS/factor cp.factor"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain factor
}

main "$@"
