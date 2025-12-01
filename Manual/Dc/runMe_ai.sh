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
    
    # PREPARE INPUT
    awk '
    BEGIN { idx=0 }
    {
        len = length($0)
        if (len > 0) {
            for (c=0; c<len; c++) {
                char = substr($0, c+1, 1)
                if (char ~ /[0-9]/) {
                    print char " " idx " :b"
                } else {
                    print "0 " idx " :b"
                }
                idx++
            }
        }
    }
    END {
        print "0 lM x" # Call solve
        print "[Solved in Iterations= ]P lI n [ ]P"
        print "lP x" # Print board
    }
    ' "$matrix" > input.dc
    
    # EXECUTION
    # Use cat to pipe input files to dc
    /usr/bin/time -l sh -c "cat Sudoku.dc input.dc | dc" > "$tmp_out" 2> "$tmp_err"
    
    end_time=$(date +%s.%N)
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    iterations=$(grep "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2)
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
    echo "    \"status\": \"$status\"" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    rm "$tmp_out" "$tmp_err" input.dc
done
echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"
cat "$METRICS_FILE"
