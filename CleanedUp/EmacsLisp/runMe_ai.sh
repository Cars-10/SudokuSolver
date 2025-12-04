#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"

first=true
PATTERN="${1:-../../Matrices/*.matrix}"

# Check if emacs is available
# If not found in path, try common locations
if ! command -v emacs &> /dev/null; then
    if [ -f "/Applications/Emacs.app/Contents/MacOS/Emacs" ]; then
        EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
    elif [ -f "/opt/homebrew/bin/emacs" ]; then
        EMACS="/opt/homebrew/bin/emacs"
    elif [ -f "/usr/bin/emacs" ]; then
        EMACS="/usr/bin/emacs"
    else
        echo "Error: emacs is not installed." >&2
        exit 1
    fi
else
    EMACS="emacs"
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
    
    # Execute Emacs Lisp
    # --batch: Run in batch mode (no GUI, exit after processing)
    # --script: Run the script
    /usr/bin/time -l "$EMACS" --batch --script sudoku.el "$matrix" > "$tmp_out" 2> "$tmp_err"
    
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
        echo "    \"status\": \"$status\"," >> "$METRICS_FILE"
    # Capture output safely
    json_output=$(cat "$tmp_out" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')
    echo "    \"output\": $json_output" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"

cat "$METRICS_FILE"
