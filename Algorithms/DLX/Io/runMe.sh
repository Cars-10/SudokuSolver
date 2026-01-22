#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Io"
SOLVER_BINARY="io dlx.io"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain io
}

main "$@"
