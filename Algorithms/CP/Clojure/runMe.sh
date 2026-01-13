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

# Compilation/setup function
compile() {
    check_toolchain clojure

    # Create wrapper script
    cat > cp_solver <<'EOF'
#!/bin/bash
clojure -M cp.clj "$@"
EOF
    chmod +x cp_solver

    echo "Clojure CP solver ready" >&2
}

# Run main benchmark
main "$@"
