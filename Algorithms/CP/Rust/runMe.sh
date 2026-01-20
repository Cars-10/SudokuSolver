#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Rust"
SOLVER_BINARY="./target/release/cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain cargo
    cargo build --release --quiet
}

main "$@"
