#!/bin/bash
# Algorithms/DLX/Fennel/runMe.sh

LANGUAGE="Fennel"

# Source shared functions
source ../../common.sh

# Create wrapper
cat << 'EOF' > fennel_wrapper.sh
#!/bin/bash
MATRIX="$1"
./fennel Sudoku.fnl "$MATRIX"
EOF
chmod +x fennel_wrapper.sh

SOLVER_BINARY="./fennel_wrapper.sh"

compile() {
    if [ ! -f "./fennel" ]; then
        report_env_error "./fennel binary not found"
    fi
    chmod +x ./fennel
}

# Execute benchmarks
main "$@"
