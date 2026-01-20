#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Racket"
ALGORITHM="DLX"
SOLVER_BINARY="racket dlx.rkt"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain racket
}

main "$@"
