#!/bin/bash
# Clojure CP Solver Benchmark Runner

cd "$(dirname "$0")"

# Configuration
LANGUAGE="Clojure"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source common benchmark functions
source ../../common.sh

# Convert arguments: if no args given and running default matrices,
# use the correct path for CP implementations (../../../Matrices instead of ../../Matrices)
if [ $# -eq 0 ]; then
    set -- ../../../Matrices/*.matrix
fi

# Compilation/setup function
compile() {
    check_toolchain clojure

    # Create wrapper script with path resolution
    cat > cp_solver <<'EOF'
#!/bin/bash
# Save original working directory before cd
ORIG_PWD="$(pwd)"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Convert relative paths to absolute (relative to original working directory)
MATRIX_FILE="$1"
if [[ ! "$MATRIX_FILE" = /* ]]; then
    # Resolve relative to original working directory
    MATRIX_FILE="$ORIG_PWD/$MATRIX_FILE"
fi

clojure -M cp.clj "$MATRIX_FILE"
EOF
    chmod +x cp_solver

    echo "Clojure CP solver ready" >&2
}

# Run main benchmark
main "$@"
