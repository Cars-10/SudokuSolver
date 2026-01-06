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
print(json.dumps(data))
" "$matrix_num" "${time_seconds:-0}" "${memory_kb:-0}" "${cpu_user:-0}" "${cpu_sys:-0}" "$status" < "$temp_output"

    rm -f "$temp_output" "$temp_timing"
}

# Execute benchmarks
main "$@"