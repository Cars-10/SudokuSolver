#!/bin/bash
cd "$(dirname "$0")"

# Add FreeBASIC to PATH if in Docker or macOS custom location
[ -x /usr/local/bin/fbc ] && export PATH="/usr/local/bin:$PATH"
[ -x /Users/vibe/Downloads/FB-Mojave/local/bin/fbc ] && export PATH="/Users/vibe/Downloads/FB-Mojave/local/bin:$PATH"

LANGUAGE="BASIC"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain fbc

    echo "Compiling BASIC CP solver..."
    # Ensure Intel libraries and system SDK are found for linking on Apple Silicon
    export PATH="/usr/local/bin:$PATH"
    export LIBRARY_PATH="/usr/local/lib/gcc/current/gcc/x86_64-apple-darwin25/15:/usr/local/lib/gcc/current:$LIBRARY_PATH"
    export SDK_PATH=$(xcrun --show-sdk-path)
    arch -x86_64 fbc -v -gen gcc cp.bas -x cp_solver \
        -p /usr/local/lib/gcc/current/gcc/x86_64-apple-darwin25/15 \
        -p /usr/local/lib/gcc/current \
        -Wl "-syslibroot" -Wl "$SDK_PATH"

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

main "$@"
