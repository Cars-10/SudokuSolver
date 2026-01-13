#!/bin/bash
# Languages/Brainfuck/runMe.sh - Brainfuck Sudoku solver benchmark script
# Uses modular common.sh pattern
#
# SPECIAL NOTE: The Brainfuck solver has the puzzle hardcoded into the source.
# Unlike other language implementations, it does NOT read from matrix files.
# The benchmark runs the solver once with the hardcoded Matrix 1 puzzle.

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Brainfuck"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check source file exists
    if [ ! -f "Sudoku.bf" ]; then
        report_env_error "Sudoku.bf not found"
    fi

    # Try bfc first (compiles to native code - fastest)
    if command -v bfc &> /dev/null; then
        echo "Using bfc compiler for native execution..."
        bfc Sudoku.bf -o Sudoku
        if [ $? -ne 0 ]; then
            report_env_error "bfc compilation failed"
        fi
        echo "Compiled to native executable: ./Sudoku"
        return
    fi

    # Fallback: use local bf_interpreter
    if [ -f "bf_interpreter.c" ]; then
        echo "bfc not found, building bf_interpreter..."
        check_toolchain gcc

        if [ ! -f "bf_interpreter" ] || [ "bf_interpreter.c" -nt "bf_interpreter" ]; then
            gcc -O3 -o bf_interpreter bf_interpreter.c
            if [ $? -ne 0 ]; then
                report_env_error "Failed to compile bf_interpreter"
            fi
        fi

        # Create wrapper script that runs interpreter
        echo "Creating interpreter wrapper..."
        cat > Sudoku <<'WRAPPER'
#!/bin/bash
cd "$(dirname "$0")"
./bf_interpreter Sudoku.bf "$@"
WRAPPER
        chmod +x Sudoku
        echo "Using bf_interpreter (interpreted mode)"
        return
    fi

    # Check for beef interpreter
    if command -v beef &> /dev/null; then
        echo "Using beef interpreter..."
        cat > Sudoku <<'WRAPPER'
#!/bin/bash
cd "$(dirname "$0")"
beef Sudoku.bf "$@"
WRAPPER
        chmod +x Sudoku
        return
    fi

    report_env_error "No Brainfuck toolchain found. Install bfc, beef, or ensure bf_interpreter.c exists."
}

# ============================================================================
# CUSTOM BENCHMARK (hardcoded puzzle - no matrix file input)
# ============================================================================
# Override main to handle the hardcoded puzzle case
main() {
    # Compile first
    compile

    # The Brainfuck solver has Matrix 1 hardcoded - it doesn't read from files.
    # We run it once and report results for "matrix 1" (hardcoded).
    #
    # If a matrix argument is provided, we check if it's Matrix 1 and run,
    # otherwise report that only Matrix 1 is supported.

    local matrix_arg="$1"

    if [ -n "$matrix_arg" ] && [[ ! "$matrix_arg" =~ 1\.matrix$ ]]; then
        echo "WARNING: Brainfuck solver only supports Matrix 1 (hardcoded)" >&2
        echo "Requested matrix: $matrix_arg" >&2
        echo "Running with hardcoded Matrix 1 instead..." >&2
    fi

    # Run the hardcoded benchmark
    run_benchmarks "../../Matrices/1.matrix"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
