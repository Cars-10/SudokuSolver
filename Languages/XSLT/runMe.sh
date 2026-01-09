#!/bin/bash
# Languages/XSLT/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="XSLT"
SOLVER_BINARY="./xslt_wrapper.sh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../common.sh

# Create dummy XML if not exists
echo "<dummy/>" > dummy.xml

cat << 'EOF' > xslt_wrapper.sh
#!/bin/bash
MATRIX="$1"
# 1. Path
echo "$MATRIX"
echo ""

# 2. Extract digits
DIGITS=$(cat "$MATRIX" | tr -cd '0-9')

# 3. Print initial board
echo "Puzzle:"
echo "$DIGITS" | sed 's/./& /g' | sed 's/.
{18}/&\n/g'
echo ""

# 4. Run XSLT
xsltproc --maxdepth 10000 --stringparam board "$DIGITS" Sudoku.xslt dummy.xml
EOF
chmod +x xslt_wrapper.sh

compile() {
    check_toolchain xsltproc
}

# Execute benchmarks
main "$@"