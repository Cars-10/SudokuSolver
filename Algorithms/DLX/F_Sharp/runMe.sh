#!/bin/bash
# Algorithms/DLX/F_Sharp/runMe.sh

LANGUAGE="F#"
# We use 'dotnet run' which handles restore/build/run
# Optimization: build release once
SOLVER_DIR="$(dirname "$0")"

# Source shared functions
source ../../common.sh

compile() {
    # Check for dotnet
    if ! command -v dotnet &> /dev/null; then
        report_env_error "dotnet not found"
        return 1
    fi
    
    # Pre-build to avoid noise during timing
    dotnet build -c Release "$SOLVER_DIR" > /dev/null
}

# Set solver command logic
# We use dotnet run with the project file
SOLVER_BINARY="dotnet run -c Release --no-build --project $SOLVER_DIR/dlx.fsproj"

# Execute benchmarks
main "$@"
