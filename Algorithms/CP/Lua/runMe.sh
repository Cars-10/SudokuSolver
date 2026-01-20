#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Lua"
SOLVER_BINARY="lua cp.lua"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain lua
    # Lua is interpreted, just verify syntax
    lua -e "assert(loadfile('cp.lua'))" 2>&1
    if [ $? -ne 0 ]; then
        echo "Lua syntax error in cp.lua"
        return 1
    fi
    return 0
}

main "$@"
