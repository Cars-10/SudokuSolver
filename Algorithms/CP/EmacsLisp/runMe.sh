#!/bin/bash
# Algorithms/CP/EmacsLisp/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="EmacsLisp"
ALGORITHM="CP"
SOLVER_BINARY="emacs --batch --script cp.el"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain emacs
}

main "$@"
