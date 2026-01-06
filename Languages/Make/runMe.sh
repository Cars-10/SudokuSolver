#!/bin/bash
# Languages/Make/runMe.sh

LANGUAGE="Make"

# Source shared functions
source ../common.sh

# Custom run_matrix for Make because it needs to generate matrix_data.mk

# Execute benchmarks
main "$@"
