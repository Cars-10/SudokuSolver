#!/bin/bash
# Languages/F_Sharp/runMe.sh - F# Sudoku solver benchmark script
# Uses modular common.sh pattern

cd "$(dirname "$0")"

# ============================================================================
# CONFIGURATION
# ============================================================================
LANGUAGE="F#"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300  # 5 minutes

# Source shared functions from common.sh
source ../common.sh

# ============================================================================
# COMPILATION
# ============================================================================
compile() {
    # Check dotnet availability
    check_toolchain dotnet

    echo "Building F# project..." >&2
    dotnet build --configuration Release -verbosity:quiet 2>&1
    if [ $? -ne 0 ]; then
        report_env_error "F# build failed"
    fi
    echo "F# build successful" >&2
}

# ============================================================================
# CUSTOM MATRIX EXECUTION (override common.sh for dotnet execution)
# ============================================================================
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    # Create temporary files for output and timing
    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)

    # Run with timeout and capture timing
    # Format: %e (elapsed seconds) %M (max RSS in KB) %U (user CPU) %S (system CPU)
    if [ -n "$TIMEOUT_CMD" ]; then
        $TIMEOUT_CMD "$TIMEOUT_SECONDS" $TIME_CMD -f "%e %M %U %S" dotnet run --configuration Release -- "$matrix_path" > "$temp_output" 2> "$temp_timing"
    else
        # No timeout command available, run without timeout
        $TIME_CMD -f "%e %M %U %S" dotnet run --configuration Release -- "$matrix_path" > "$temp_output" 2> "$temp_timing"
    fi
    local exit_code=$?

    # Read outputs
    local output=$(cat "$temp_output")
    local timing_line=$(tail -1 "$temp_timing")  # Last line has timing data

    # Determine status
    local status="success"
    if [ $exit_code -eq 124 ]; then
        status="timeout"
    elif [ $exit_code -ne 0 ]; then
        status="error"
    fi

    # Parse timing data (format: time_seconds memory_kb cpu_user cpu_sys)
    local time_seconds=$(echo "$timing_line" | awk '{print $1}')
    local memory_kb=$(echo "$timing_line" | awk '{print $2}')
    local cpu_user=$(echo "$timing_line" | awk '{print $3}')
    local cpu_sys=$(echo "$timing_line" | awk '{print $4}')

    # Extract iterations from output
    local iterations=$(extract_iterations "$output")

    # Default values if parsing failed
    time_seconds=${time_seconds:-0}
    memory_kb=${memory_kb:-0}
    cpu_user=${cpu_user:-0}
    cpu_sys=${cpu_sys:-0}
    iterations=${iterations:-0}

    # Convert memory from KB to bytes (HTMLGenerator expects bytes)
    local memory_bytes=$((memory_kb * 1024))

    # Escape output for JSON (replace newlines, escape quotes and backslashes)
    local escaped_output=$(echo "$output" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g' | awk '{printf "%s\\n", $0}' | sed '$ s/\\n$//')

    # Write JSON result object (caller handles array wrapping)
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

    # Cleanup temp files
    rm -f "$temp_output" "$temp_timing"
}

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================
main "$@"
