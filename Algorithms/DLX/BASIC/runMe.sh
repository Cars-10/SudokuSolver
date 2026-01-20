#!/bin/bash
# Algorithms/DLX/BASIC/runMe.sh

cd "$(dirname "$0")"

# Add FreeBASIC to PATH
[ -x /usr/local/bin/fbc ] && export PATH="/usr/local/bin:$PATH"
[ -x /Users/vibe/Downloads/FB-Mojave/local/bin/fbc ] && export PATH="/Users/vibe/Downloads/FB-Mojave/local/bin:$PATH"

LANGUAGE="BASIC"
ALGORITHM="DLX"
SOLVER_BINARY="arch -x86_64 ./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source shared functions
source ../../common.sh

compile() {
    check_toolchain fbc

    echo "Compiling BASIC DLX solver..."
    # Ensure Intel libraries and system SDK are found for linking on Apple Silicon
    export PATH="/usr/local/bin:$PATH"
    export LIBRARY_PATH="/usr/local/lib/gcc/current/gcc/x86_64-apple-darwin25/15:/usr/local/lib/gcc/current:$LIBRARY_PATH"
    export SDK_PATH=$(xcrun --show-sdk-path)
    arch -x86_64 fbc -v -gen gcc DLX.bas -x dlx_solver \
        -p /usr/local/lib/gcc/current/gcc/x86_64-apple-darwin25/15 \
        -p /usr/local/lib/gcc/current \
        -Wl "-syslibroot" -Wl "$SDK_PATH"

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

main "$@"
