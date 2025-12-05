#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"

# Function to report environment error
report_env_error() {
    echo "[" > "$METRICS_FILE"
    echo "  {" >> "$METRICS_FILE"
    echo "    \"solver\": \"Python\", " >> "$METRICS_FILE"
    echo "    \"status\": \"error\", " >> "$METRICS_FILE"
    echo "    \"output\": \"Environment Not Ready: $1\"" >> "$METRICS_FILE"
    echo "  }" >> "$METRICS_FILE"
    echo "]" >> "$METRICS_FILE"
    cat "$METRICS_FILE"
    exit 1
}

# --- SETUP PHASE ---

# No compilation needed
if [ ! -f "Sudoku.py" ]; then
    report_env_error "Sudoku.py not found"
fi

# --- EXECUTION PHASE ---
echo "[" > "$METRICS_FILE"

first=true
MATRICES="$@"

if [ -z "$MATRICES" ]; then
    MATRICES="../../../Matrices/*.matrix"
fi

for matrix in $MATRICES; do
    if [ "$first" = true ]; then
        first=false
    else
        echo "," >> "$METRICS_FILE"
    fi

    matrix_name=$(basename "$matrix")
    
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    
    # Python timestamp for precision
    # Detect OS for time command
    OS="$(uname)"
    if [ "$OS" = "Darwin" ]; then
        /usr/bin/time -l python3 Sudoku.py "$matrix" > "$tmp_out" 2> "$tmp_err"
        memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
        # Extract real time from /usr/bin/time output (format: "0.01 real 0.00 user 0.00 sys")
        duration=$(grep "real" "$tmp_err" | awk '{print $1}')
        cpu_user=$(grep "real" "$tmp_err" | awk '{print $3}')
        cpu_sys=$(grep "real" "$tmp_err" | awk '{print $5}')
    else
        /usr/bin/time -v python3 Sudoku.py "$matrix" > "$tmp_out" 2> "$tmp_err"
        memory=$(grep "Maximum resident set size" "$tmp_err" | awk -F': ' '{print $2}')
        # Convert KB to Bytes
        memory=$((memory * 1024))
        # Extract elapsed time (format: "0:00.01" or "h:mm:ss")
        elapsed=$(grep "Elapsed (wall clock)" "$tmp_err" | awk -F': ' '{print $2}')
        # Convert to seconds
        duration=$(echo "$elapsed" | awk -F: '{if (NF==2) print $1*60+$2; else print $1*3600+$2*60+$3}')
        cpu_user=$(grep "User time" "$tmp_err" | awk -F': ' '{print $2}')
        cpu_sys=$(grep "System time" "$tmp_err" | awk -F': ' '{print $2}')
    fi

    # Validation
    iterations=$(grep -a "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2 | tr -d '[:space:]')
    if [ -z "$iterations" ]; then
        iterations=0
        status="fail"
    else
        status="pass"
    fi
     
    # Result
    echo "  {" >> "$METRICS_FILE"
    echo "    \"matrix\": \"$matrix_name\"," >> "$METRICS_FILE"
    echo "    \"time\": $duration," >> "$METRICS_FILE"
    echo "    \"iterations\": $iterations," >> "$METRICS_FILE"
    echo "    \"memory\": $memory," >> "$METRICS_FILE"
    echo "    \"cpu_user\": $cpu_user," >> "$METRICS_FILE"
    echo "    \"cpu_sys\": $cpu_sys," >> "$METRICS_FILE"
    echo "    \"status\": \"$status\"," >> "$METRICS_FILE"
    
    # Safe JSON output capture
    json_output=$(cat "$tmp_out" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')
    echo "    \"output\": $json_output" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"
cat "$METRICS_FILE"
