#!/bin/bash
# Languages/common.sh - Shared functions for benchmark scripts
# Created: 2025-12-16
#
# This library provides reusable functions for all language implementations.
# Source this file in your language-specific runMe.sh script.
#
# Usage:
#   source ../common.sh
#   main "$@"

# ============================================================================
# CONFIGURATION (override in runMe.sh before sourcing)
# ============================================================================
LANGUAGE="${LANGUAGE:-Unknown}"
SOLVER_BINARY="${SOLVER_BINARY:-./Sudoku}"
COMPILE_CMD="${COMPILE_CMD:-}"
METRICS_FILE="${METRICS_FILE:-metrics.json}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes default

# ============================================================================
# ENVIRONMENT DETECTION
# ============================================================================

# Detect OS and set appropriate time command
detect_time_cmd() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS uses GNU time via gtime (install with: brew install gnu-time)
        if command -v gtime &> /dev/null; then
            TIME_CMD="gtime"
        else
            echo "WARNING: gtime not found on macOS. Install with: brew install gnu-time" >&2
            TIME_CMD="/usr/bin/time"
        fi
    else
        # Linux uses /usr/bin/time
        TIME_CMD="/usr/bin/time"
    fi
}

# ============================================================================
# ERROR HANDLING
# ============================================================================

# Report environment error (missing compiler, dependencies, etc.)
# Generates error metrics.json and exits
report_env_error() {
    local error_msg="$1"
    local timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    cat > "$METRICS_FILE" <<EOF
[{
  "solver": "$LANGUAGE",
  "runType": "automated",
  "timestamp": "$timestamp",
  "results": [{
    "matrix": "N/A",
    "time": 0,
    "iterations": 0,
    "memory": 0,
    "cpu_user": 0,
    "cpu_sys": 0,
    "status": "env_error",
    "output": "$error_msg"
  }]
}]
EOF

    echo "ERROR: $error_msg" >&2
    exit 1
}

# ============================================================================
# OUTPUT PARSING
# ============================================================================

# Extract iteration count from solver output
# Expects format: "Solved in Iterations=656"
extract_iterations() {
    local output="$1"
    local iterations=$(echo "$output" | grep "Solved in Iterations=" | sed 's/.*Iterations=//')

    if [ -z "$iterations" ]; then
        echo "0"
    else
        echo "$iterations"
    fi
}

# ============================================================================
# MATRIX EXECUTION
# ============================================================================

# Run solver on a single matrix with timing and metrics capture
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    # Create temporary files for output and timing
    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)
    local temp_combined=$(mktemp)

    # Run with timeout and capture timing
    # Note: timeout command exists, time output goes to stderr
    # Format: %e (elapsed seconds) %M (max RSS in KB) %U (user CPU) %S (system CPU)
    timeout "$TIMEOUT_SECONDS" $TIME_CMD -f "%e %M %U %S" "$SOLVER_BINARY" "$matrix_path" > "$temp_output" 2> "$temp_timing"
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

    # Escape output for JSON (replace newlines, escape quotes and backslashes)
    local escaped_output=$(echo "$output" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g' | awk '{printf "%s\\n", $0}' | sed '$ s/\\n$//')

    # Write JSON result object (caller handles array wrapping)
    cat <<EOF
    {
      "matrix": "$matrix_num",
      "time": $time_seconds,
      "iterations": $iterations,
      "memory": $memory_kb,
      "cpu_user": $cpu_user,
      "cpu_sys": $cpu_sys,
      "status": "$status",
      "output": "$escaped_output"
    }
EOF

    # Cleanup temp files
    rm -f "$temp_output" "$temp_timing" "$temp_combined"
}

# ============================================================================
# MAIN EXECUTION WRAPPER
# ============================================================================

# Run benchmarks for all specified matrices
# Usage: run_benchmarks [matrix_paths...]
# If no paths given, runs all matrices from ../../../Matrices/*.matrix
run_benchmarks() {
    local matrices="$@"
    local timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # If no matrices specified, use all available
    if [ -z "$matrices" ]; then
        matrices="../../../Matrices/*.matrix"
    fi

    # Expand glob pattern
    matrices=$(ls $matrices 2>/dev/null)

    if [ -z "$matrices" ]; then
        report_env_error "No matrix files found"
    fi

    # Detect time command for this platform
    detect_time_cmd

    # Start JSON structure
    cat > "$METRICS_FILE" <<EOF
[
  {
    "solver": "$LANGUAGE",
    "runType": "automated",
    "timestamp": "$timestamp",
    "results": [
EOF

    # Run each matrix
    local first=true
    for matrix in $matrices; do
        if [ ! -f "$matrix" ]; then
            echo "WARNING: Matrix file not found: $matrix" >&2
            continue
        fi

        echo "Running $LANGUAGE on $(basename $matrix)..." >&2

        # Add comma separator if not first result
        if [ "$first" = true ]; then
            first=false
        else
            echo "," >> "$METRICS_FILE"
        fi

        # Run matrix and append result
        run_matrix "$matrix" >> "$METRICS_FILE"
    done

    # Close JSON structure
    cat >> "$METRICS_FILE" <<EOF

    ]
  }
]
EOF

    echo "Metrics written to $METRICS_FILE" >&2
}

# ============================================================================
# COMPILATION HELPERS (optional, override in language scripts)
# ============================================================================

# Default compilation function (can be overridden)
compile() {
    if [ -n "$COMPILE_CMD" ]; then
        echo "Compiling $LANGUAGE..." >&2
        eval "$COMPILE_CMD"

        if [ $? -ne 0 ]; then
            report_env_error "Compilation failed"
        fi

        echo "Compilation successful" >&2
    fi
}

# Check if compiler/interpreter is available
check_toolchain() {
    local tool="$1"

    if ! command -v "$tool" &> /dev/null; then
        report_env_error "$tool not found in PATH"
    fi
}

# ============================================================================
# MAIN ENTRY POINT (called from language scripts)
# ============================================================================

# Default main function - can be overridden or called from language script
main() {
    # Compile if needed
    compile

    # Run benchmarks
    run_benchmarks "$@"
}

# ============================================================================
# EXPORTS
# ============================================================================
# All functions are available when this script is sourced

echo "common.sh loaded for language: $LANGUAGE" >&2
