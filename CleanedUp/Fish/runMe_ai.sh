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
    
    # EXECUTION
    # Fish might be installed in various places, try to find it or assume it's in path
    if command -v fish &> /dev/null; then
        FISH_CMD="fish"
    elif [ -f "/opt/homebrew/bin/fish" ]; then
        FISH_CMD="/opt/homebrew/bin/fish"
    elif [ -f "/usr/local/bin/fish" ]; then
        FISH_CMD="/usr/local/bin/fish"
    else
        echo "Fish not found" >&2
        exit 1
    fi

    /usr/bin/time -l "$FISH_CMD" Sudoku.fish "$matrix" > "$tmp_out" 2> "$tmp_err"
    
    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    iterations=$(grep "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2 | tr -d ' ')
    if [ -z "$iterations" ]; then iterations=0; status="fail"; else status="pass"; fi
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
