#!/bin/bash
cd "$(dirname "$0")"

# Configuration
LANGUAGE="Kotlin"
SOLVER_BINARY="./cp_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source common benchmark functions
source ../../common.sh

# Compile function
compile() {
    check_toolchain kotlinc

    echo "Compiling CP.kt..."
    kotlinc CP.kt -include-runtime -d cp_solver.jar

    # Create executable wrapper script
    cat > cp_solver << 'EOF'
#!/bin/bash
java -jar "$(dirname "$0")/cp_solver.jar" "$@"
EOF
    chmod +x cp_solver

    echo "Compilation complete"
}

# Run main benchmark process
main "$@"
