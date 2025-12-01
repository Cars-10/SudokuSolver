#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"

first=true
PATTERN="${1:-../../Matrices/*.matrix}"

# Check if octave is available
if ! command -v octave &> /dev/null; then
    echo "Error: octave is not installed." >&2
    exit 1
fi

for matrix in $PATTERN; do
    if [ "$first" = true ]; then
        first=false
    else
        echo "," >> "$METRICS_FILE"
    fi

    matrix_name=$(basename "$matrix")
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    
    start_time=$(date +%s.%N)
    
    # Execute Octave script
    # -q: quiet (no startup message)
    # --no-gui: run without GUI
    /usr/bin/time -l octave -q --no-gui sudoku.m "$matrix" > "$tmp_out" 2> "$tmp_err"
    
    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    
    iterations=$(grep "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2)
    if [ -z "$iterations" ]; then
        iterations=0
        status="fail"
    else
        status="pass"
    fi
    
    memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
    if [ -z "$memory" ]; then memory=0; fi
    
    cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
    if [ -z "$cpu_user" ]; then cpu_user=0; fi
    
    cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    if [ -z "$cpu_sys" ]; then cpu_sys=0; fi
    
    echo "  {" >> "$METRICS_FILE"
    echo "    \"matrix\": \"$matrix_name\"," >> "$METRICS_FILE"
    echo "    \"time\": $duration," >> "$METRICS_FILE"
    echo "    \"iterations\": $iterations," >> "$METRICS_FILE"
    echo "    \"memory\": $memory," >> "$METRICS_FILE"
    echo "    \"cpu_user\": $cpu_user," >> "$METRICS_FILE"
    echo "    \"cpu_sys\": $cpu_sys," >> "$METRICS_FILE"
    echo "    \"status\": \"$status\"" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"

cat "$METRICS_FILE"
