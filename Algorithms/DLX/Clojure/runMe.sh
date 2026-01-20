#!/bin/bash
# Clojure DLX Solver Benchmark Runner

cd "$(dirname "$0")"

# Configuration
LANGUAGE="Clojure"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source common benchmark functions
source ../../common.sh

# Compilation/setup function
compile() {
    check_toolchain clojure

    # Create wrapper script
    cat > dlx_solver <<'EOF'
#!/bin/bash
clojure -M dlx.clj "$@"
EOF
    chmod +x dlx_solver

    echo "Clojure DLX solver ready" >&2
}

# Run main benchmark
main "$@"
