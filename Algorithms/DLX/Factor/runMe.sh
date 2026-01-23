#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Factor"
SOLVER_BINARY="./factor_wrapper.sh dlx.factor"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain /Applications/factor/factor
}

main "$@"
