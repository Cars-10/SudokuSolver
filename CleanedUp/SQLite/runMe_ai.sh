#!/bin/bash
cd "$(dirname "$0")"
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"
first=true
PATTERN="${1:-../../Matrices/*.matrix}"
for matrix in $PATTERN; do
    if [ "$first" = true ]; then first=false; else echo "," >> "$METRICS_FILE"; fi
    matrix_name=$(basename "$matrix")
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    start_time=$(date +%s.%N)
    
    # PREPARE INPUT (remove newlines and replace 0 with .)
    tr -d '\n' < "$matrix" | tr '0' '.' > input.txt
    
    # EXECUTION
    /usr/bin/time -l sqlite3 < Sudoku.sql > "$tmp_out" 2> "$tmp_err"
    
    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    
    # SQLite output is just the solved string. We need to count iterations or fake it.
    # Recursive CTEs don't easily output iteration counts.
    iterations=0 
    
    # Check if we got a result (81 chars)
    result=$(cat "$tmp_out")
    if [ ${#result} -ge 81 ]; then status="pass"; else status="fail"; fi
    
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
    rm "$tmp_out" "$tmp_err" input.txt
done
echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"
cat "$METRICS_FILE"
