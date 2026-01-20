#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Prolog"
SOLVER_BINARY="swipl -q -t main -s cp.pl --"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain swipl
    if [ ! -f "cp.pl" ]; then
        report_env_error "Source file cp.pl not found"
    fi
    echo "No compilation needed for Prolog (interpreted)"
}

main "$@"
