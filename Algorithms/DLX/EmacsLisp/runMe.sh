#!/bin/bash
# Algorithms/DLX/EmacsLisp/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="EmacsLisp"
ALGORITHM="DLX"
SOLVER_BINARY="emacs --batch --script dlx.el"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain emacs
}

main "$@"
