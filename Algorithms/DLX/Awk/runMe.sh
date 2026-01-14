#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Awk"
SOLVER_BINARY="awk -f dlx.awk"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain awk
    return $?
}

main "$@"
