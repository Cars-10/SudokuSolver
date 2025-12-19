#!/bin/bash
# Languages/Verilog/runMe.sh

LANGUAGE="Verilog"
SOLVER_BINARY="./sudoku"

# Source shared functions
source ../common.sh

compile() {
    echo "Compiling Verilog solver..."
    iverilog -o sudoku Sudoku.v
    
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Override run_matrix to handle vvp command
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)

    if [ -n "$TIMEOUT_CMD" ]; then
        $TIMEOUT_CMD "$TIMEOUT_SECONDS" $TIME_CMD -f "%e %M %U %S" vvp -n sudoku +FILE="$matrix_path" > "$temp_output" 2> "$temp_timing"
    else
        $TIME_CMD -f "%e %M %U %S" vvp -n sudoku +FILE="$matrix_path" > "$temp_output" 2> "$temp_timing"
    fi
    local exit_code=$?

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
    
    # Use python to extract iterations and generate valid JSON safely
    python3 -c "
import json, sys, re
raw_output = sys.stdin.read()
# Clean output of null bytes and other non-printable control chars
output = ''.join(c for c in raw_output if c.isprintable() or c in '\n\r\t')

# Extract iterations
match = re.search(r'Solved in Iterations=(\\d+)', output)
iterations = int(match.group(1)) if match else 0

data = {
    'matrix': sys.argv[1],
    'time': float(sys.argv[2]),
    'iterations': iterations,
    'memory': int(sys.argv[3]) * 1024,
    'cpu_user': float(sys.argv[4]),
    'cpu_sys': float(sys.argv[5]),
    'status': sys.argv[6],
    'output': output
}
print(json.dumps(data))
" "$matrix_num" "${time_seconds:-0}" "${memory_kb:-0}" "${cpu_user:-0}" "${cpu_sys:-0}" "$status" < "$temp_output"

    rm -f "$temp_output" "$temp_timing"
}

# Execute benchmarks
main "$@"