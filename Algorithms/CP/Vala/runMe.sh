#!/bin/bash
cd "$(dirname "$0")"
LANGUAGE="Vala"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300
source ../../common.sh

compile() {
    check_toolchain valac
    valac --pkg glib-2.0 --pkg gio-2.0 cp.vala -o cp_solver
}

main "$@"
