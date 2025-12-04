#!/bin/bash
cd "$(dirname "$0")"

# Compile C++ solver
# Using -O3 for optimization to be fair with C
g++ -O3 -o Sudoku Sudoku.cpp

# Output file for metrics
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"

first=true

# Iterate over matrix files
PATTERN="${1:-../../Matrices/*.matrix}"
for matrix in $PATTERN; do
    if [ "$first" = true ]; then
        first=false
    else
        echo "," >> "$METRICS_FILE"
    fi

    matrix_name=$(basename "$matrix")
    
    # Create a temporary file for output
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    
    # Start timer
    start_time=$(date +%s.%N)
    
    # Execute
    /usr/bin/time -l ./Sudoku "$matrix" > "$tmp_out" 2> "$tmp_err"
    
    # End timer
    end_time=$(date +%s.%N)
    
    # Calculate duration
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    
    # Parse Iterations
    iterations=$(grep "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2)
    if [ -z "$iterations" ]; then
        iterations=0
        status="fail"
    else
        status="pass"
    fi
    
    # Parse Memory (max resident set size)
    memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
    if [ -z "$memory" ]; then memory=0; fi
    
    # Parse CPU User/Sys
    cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
    if [ -z "$cpu_user" ]; then cpu_user=0; fi
    
    cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    if [ -z "$cpu_sys" ]; then cpu_sys=0; fi
    
    # Construct JSON object
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
    
    # Cleanup
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"

# Output the metrics file content to stdout so the aggregator can read it
cat "$METRICS_FILE"
