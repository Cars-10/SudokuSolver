#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Perl"
SOLVER_BINARY="perl dlx.pl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

# No compile function needed for interpreted language

main "$@"
