#!/bin/bash
cd "$(dirname "$0")"

# Configuration
LANGUAGE="Kotlin"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source common benchmark functions
source ../../common.sh

# Compile function
compile() {
    check_toolchain kotlinc

    echo "Compiling DLX.kt..."
    kotlinc DLX.kt -include-runtime -d dlx_solver.jar

    # Create executable wrapper script
    cat > dlx_solver << 'EOF'
#!/bin/bash
java -jar "$(dirname "$0")/dlx_solver.jar" "$@"
EOF
    chmod +x dlx_solver

    echo "Compilation complete"
}

# Run main benchmark process
main "$@"
