#!/bin/bash
# Languages/TypeScript/runMe.sh - TypeScript Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="TypeScript"
SOLVER_BINARY="node out/Sudoku.js"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check Node.js and npm availability
    check_toolchain node
    check_toolchain npm

    # Check source file exists
    if [ ! -f "Sudoku.ts" ]; then
        report_env_error "Sudoku.ts not found"
    fi

    # Check if package.json exists
    if [ ! -f "package.json" ]; then
        report_env_error "package.json not found"
    fi

    # Install dependencies if node_modules doesn't exist or is outdated
    if [ ! -d "node_modules" ] || [ "package.json" -nt "node_modules" ]; then
        echo "Installing dependencies..."
        npm install > /dev/null 2>&1
        if [ $? -ne 0 ]; then
            report_env_error "npm install failed"
        fi
    fi

    # Compile TypeScript
    echo "Compiling TypeScript..."
    npm run build > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "TypeScript compilation failed"
    fi

    # Verify output exists
    if [ ! -f "out/Sudoku.js" ]; then
        report_env_error "Compiled output not found: out/Sudoku.js"
    fi

    echo "TypeScript solver ready: out/Sudoku.js (Node.js $(node --version), TypeScript $(npx tsc --version | awk '{print $2}'))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
