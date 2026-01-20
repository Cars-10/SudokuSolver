#!/bin/bash
# Algorithms/BruteForce/Verilog/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Verilog"
SOLVER_BINARY="./run_verilog.sh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain iverilog

    echo "Compiling Verilog solver..."
    iverilog -o sudoku_sim Sudoku.v
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi

    # Create wrapper to run vvp simulation
    cat > run_verilog.sh << 'WRAPPER'
#!/bin/bash
vvp ./sudoku_sim +FILE="$1"
WRAPPER
    chmod +x run_verilog.sh
}

main "$@"
