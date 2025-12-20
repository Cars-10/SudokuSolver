#!/bin/bash
# Languages/Make/runMe.sh

LANGUAGE="Make"

# Source shared functions
source ../common.sh

# Custom run_matrix for Make because it needs to generate matrix_data.mk
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    # Generate matrix_data.mk (from original setupAndRunMe.sh)
    echo "# Matrix Data" > matrix_data.mk
    local row=0
    grep -v '^#' "$matrix_path" | while read -r line; do
        local col=0
        for val in $line; do
            echo "cell_${row}_${col} := $val" >> matrix_data.mk
            col=$((col + 1))
        done
        row=$((row + 1))
    done

    # Now use the standard SOLVER_BINARY and common.sh logic
    SOLVER_BINARY="make solve INPUT=$matrix_path"
    
    # Capture metrics using the internal helper from common.sh
    # We call the logic from common.sh but with our temporary SOLVER_BINARY
    
    # Create temporary files for output and timing
    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)

    detect_time_cmd
    
    if [ -n "$TIMEOUT_CMD" ]; then
        $TIMEOUT_CMD "$TIMEOUT_SECONDS" $TIME_CMD -f "%e %M %U %S" $SOLVER_BINARY > "$temp_output" 2> "$temp_timing"
    else
        $TIME_CMD -f "%e %M %U %S" $SOLVER_BINARY > "$temp_output" 2> "$temp_timing"
    fi
    local exit_code=$?

    # Read outputs
    local output=$(cat "$temp_output")
    local timing_line=$(tail -1 "$temp_timing")

    # Determine status
    local status="success"
    if [ $exit_code -eq 124 ]; then status="timeout"
elif [ $exit_code -ne 0 ]; then status="error"; fi

    # Parse timing data
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
    local escaped_output=$(echo "$output" | sed 's/\\/\\\\/g' | sed 's/"/\"/g' | awk '{printf "%s\\n", $0}' | sed '$ s/\\n$//')

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

# Execute benchmarks
main "$@"
