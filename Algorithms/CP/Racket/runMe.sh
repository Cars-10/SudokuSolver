#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Racket"
ALGORITHM="CP"
SOLVER_BINARY="racket cp.rkt"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain racket
}

main "$@"
