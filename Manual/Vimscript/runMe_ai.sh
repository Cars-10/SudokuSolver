#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"

first=true
PATTERN="${1:-../../Matrices/*.matrix}"

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
    
    # Execute Vimscript
    # -E: Ex mode
    # -s: Silent (batch) mode
    # -S: Source file
    # We write output to a file within the vim script to ensure we capture it.
    # But first, let's try capturing stdout/stderr with -E -s and verbose.
    
    # Actually, let's modify sudoku.vim to write to a file 'out.txt' and read that.
    # But for now, let's try to see if we can get output to stdout.
    # Vim in Ex mode doesn't easily print to stdout.
    # We can use /dev/stdout if available, or just a temp file.
    
    # Let's try running without -s to see if we get output, but that might be interactive.
    # Better approach: Modify sudoku.vim to write to a log file.
    
    # For now, let's try capturing stderr which sometimes works for 'echo'.
    # Or use --not-a-term which we did.
    
    # Let's try a different invocation:
    # vim -E -s -S sudoku.vim -c "qa!" -- "$matrix" > "$tmp_out" 2> "$tmp_err"
    
    /usr/bin/time -l vim -E -s -S sudoku.vim -c "qa!" -- "$matrix" > /dev/null 2> "$tmp_err"
    
    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    
    # Read output from file
    if [ -f "out.txt" ]; then
        cat "out.txt" > "$tmp_out"
        rm "out.txt"
    fi
    
    iterations=$(grep "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2)
    # If not in stdout, check stderr (Vim sometimes prints there)
    if [ -z "$iterations" ]; then
        iterations=$(grep "Solved in Iterations=" "$tmp_err" | cut -d'=' -f2)
    fi
    
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
