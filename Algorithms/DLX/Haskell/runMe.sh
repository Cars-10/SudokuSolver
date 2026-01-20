#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Haskell"
SOLVER_BINARY="./dlx"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain ghc
    echo "Compiling Haskell DLX solver..." >&2
    ghc -O2 -o dlx dlx.hs
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

main "$@"
