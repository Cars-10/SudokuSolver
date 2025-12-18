#!/bin/bash
# Languages/Julia/runMe.sh - Julia Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="Julia"
SOLVER_BINARY="julia Sudoku.jl"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION (not needed for Julia - JIT compiled)
# ============================================================================
compile() {
    # Check Julia availability
    check_toolchain julia

    # Check source file exists
    if [ ! -f "Sudoku.jl" ]; then
        report_env_error "Sudoku.jl not found"
    fi

    echo "Julia solver ready: Sudoku.jl ($(julia --version 2>&1))"
}

# ============================================================================
# OVERRIDE run_matrix to handle multi-word SOLVER_BINARY
# ============================================================================
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)

    # Run with timeout and capture timing
    if [ -n "$TIMEOUT_CMD" ]; then
        $TIMEOUT_CMD "$TIMEOUT_SECONDS" $TIME_CMD -f "%e %M %U %S" julia Sudoku.jl "$matrix_path" > "$temp_output" 2> "$temp_timing"
    else
        $TIME_CMD -f "%e %M %U %S" julia Sudoku.jl "$matrix_path" > "$temp_output" 2> "$temp_timing"
    fi
    local exit_code=$?

    local output=$(cat "$temp_output")
    local timing_line=$(tail -1 "$temp_timing")

    local status="success"
    if [ $exit_code -eq 124 ]; then
        status="timeout"
    elif [ $exit_code -ne 0 ]; then
        status="error"
    fi

    local time_seconds=$(echo "$timing_line" | awk '{print $1}')
    local memory_kb=$(echo "$timing_line" | awk '{print $2}')
    local cpu_user=$(echo "$timing_line" | awk '{print $3}')
    local cpu_sys=$(echo "$timing_line" | awk '{print $4}')
    local iterations=$(extract_iterations "$output")

    time_seconds=${time_seconds:-0}
    memory_kb=${memory_kb:-0}
    cpu_user=${cpu_user:-0}
    cpu_sys=${cpu_sys:-0}
    iterations=${iterations:-0}

    local memory_bytes=$((memory_kb * 1024))
    local escaped_output=$(echo "$output" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g' | awk '{printf "%s\\n", $0}' | sed '$ s/\\n$//')

    cat <<EOF
    {
      "matrix": "$matrix_num",
      "time": $time_seconds,
      "iterations": $iterations,
      "memory": $memory_bytes,
      "cpu_user": $cpu_user,
      "cpu_sys": $cpu_sys,
      "status": "$status",
      "output": "$escaped_output"
    }
EOF

    rm -f "$temp_output" "$temp_timing"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================
main "$@"
