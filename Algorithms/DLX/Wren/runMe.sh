#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Wren"
SOLVER_BINARY="wren_cli dlx.wren"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"
source ../../common.sh

compile() {
    check_toolchain wren_cli
    # Wren is interpreted, no compilation needed
    return 0
}

main "$@"
