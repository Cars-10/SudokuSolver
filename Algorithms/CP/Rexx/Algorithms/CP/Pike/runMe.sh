#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Pike"
SOLVER_BINARY="pike cp.pike"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain pike
    chmod +x cp.pike
}

main "$@"
