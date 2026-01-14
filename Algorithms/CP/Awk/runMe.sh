#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Awk"
SOLVER_BINARY="awk -f cp.awk"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain awk
    # Check if awk supports bitwise operations (gawk/nawk)
    if ! awk 'BEGIN { if (and(1,1) != 1) exit 1 }' 2>/dev/null; then
        echo "WARNING: AWK version may not support bitwise operations (and/or/xor)" >&2
        echo "         gawk or nawk recommended for CP algorithm" >&2
    fi
    return $?
}

main "$@"
