#!/bin/bash
cd "$(dirname "$0")"

LANGUAGE="Fennel"
SOLVER_BINARY="./fennel Sudoku.fnl"
METRICS_FILE="metrics.json"

source ../../common.sh

main "$@"
