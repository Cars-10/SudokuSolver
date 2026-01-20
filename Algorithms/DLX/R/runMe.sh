#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="R"
SOLVER_BINARY="Rscript dlx.R"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain Rscript
}

main "$@"
