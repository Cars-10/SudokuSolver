#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"

# Function to report environment error
report_env_error() {
    echo "[" > "$METRICS_FILE"
    echo "  {" >> "$METRICS_FILE"
    echo "    \"solver\": \"C++\"," >> "$METRICS_FILE"
    echo "    \"status\": \"error\"," >> "$METRICS_FILE"
    echo "    \"output\": \"Environment Not Ready: $1\"" >> "$METRICS_FILE"
    echo "  }" >> "$METRICS_FILE"
    echo "]" >> "$METRICS_FILE"
    cat "$METRICS_FILE"
    exit 1
}

# --- SETUP PHASE ---
# Check if Sudoku executable exists
if [ ! -f "./Sudoku" ]; then
    echo "Executable not found. Attempting to compile..."
    if [ -f "Sudoku.cpp" ]; then
        g++ -O3 -o Sudoku Sudoku.cpp
        if [ $? -ne 0 ]; then
            report_env_error "Compilation failed"
        fi
    else
        report_env_error "Source file Sudoku.cpp not found"
    fi
fi

# Verify executable is runnable
if [ ! -x "./Sudoku" ]; then
    chmod +x ./Sudoku
    if [ ! -x "./Sudoku" ]; then
        report_env_error "Cannot execute ./Sudoku"
    fi
fi

# --- EXECUTION PHASE ---
echo "[" > "$METRICS_FILE"

first=true
MATRICES="$@"

if [ -z "$MATRICES" ]; then
    # Fallback if no arguments passed
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
    
    start_time=$(date +%s.%N)
    
    # Run with timeout (handled by parent or here? Parent handles timeout of this script, 
    # but individual runs might need it too. For now, rely on parent timeout or simple execution)
    # Detect OS
    OS="$(uname)"
    if [ "$OS" = "Darwin" ]; then
        /usr/bin/time -l ./Sudoku "$matrix" > "$tmp_out" 2> "$tmp_err"
        memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
        cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
        cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    else
        # Linux (GNU time)
        # -v gives verbose output similar to BSD -l
        /usr/bin/time -v ./Sudoku "$matrix" > "$tmp_out" 2> "$tmp_err"
        # Linux output: "Maximum resident set size (kbytes): 1234"
        memory=$(grep "Maximum resident set size" "$tmp_err" | awk -F': ' '{print $2}')
        # Convert kbytes to bytes to match macOS (if macOS is bytes? macOS -l is bytes)
        # Wait, macOS `time -l` "maximum resident set size" is in BYTES.
        # Linux `time -v` "Maximum resident set size (kbytes)" is in KILOBYTES.
        # We should standardize. Let's standardize on KILOBYTES for the metric if possible, or bytes.
        # The existing script just takes the number.
        # macOS: 1234567 (bytes) -> 1234 KB
        # Let's keep it raw for now or convert? The user didn't specify unit standardization, but for "CleanedUp" it's good.
        # However, to minimize risk of breaking existing charts, I will just try to match the raw number magnitude if possible,
        # OR just extract the number.
        # If I change units, I might break the charts.
        # Let's check what the previous code did: `memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')`
        # On macOS this is bytes.
        # On Linux, it is KB.
        # To be safe, I will convert Linux KB to Bytes to match macOS behavior.
        memory=$((memory * 1024))
        
        # User time (seconds): 0.00
        cpu_user=$(grep "User time" "$tmp_err" | awk -F': ' '{print $2}')
        # System time (seconds): 0.00
        cpu_sys=$(grep "System time" "$tmp_err" | awk -F': ' '{print $2}')
    fi
    
    end_time=$(date +%s.%N)
    duration=$(awk -v start=$start_time -v end=$end_time 'BEGIN { printf "%.6f", end - start }')
    
    iterations=$(grep -a "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2 | tr -d '[:space:]')
    if [ -z "$iterations" ]; then
        iterations=0
        status="fail"
    else
        status="pass"
    fi

    # --- Output Verification ---
    REF_FILE="../../../Matrices/ReferenceForAllMatrixRun.txt"
    if [ -f "$REF_FILE" ]; then
        # Extract expected output block for this matrix
        # We look for the section starting with "../Matrices/$matrix_name"
        # and ending before the next "../Matrices/" or EOF.
        
        expected_block=$(awk -v target="../Matrices/$matrix_name" '
            $0 ~ target { found=1; next }
            found && $0 ~ /^\.\.\/Matrices\// { found=0; exit }
            found { print }
        ' "$REF_FILE")
        
        # Filter actual: remove first line, remove "Seconds to process"
        actual_content=$(tail -n +2 "$tmp_out" | grep -v "Seconds to process")
        
        # Filter expected: remove "Seconds to process" (it is in reference file)
        expected_content=$(echo "$expected_block" | grep -v "Seconds to process")
        
        # Diff them
        diff_output=$(diff <(echo "$actual_content") <(echo "$expected_content"))
        if [ $? -ne 0 ]; then
            status="suspect"
        fi
    else
        echo "Warning: Reference file not found at $REF_FILE" >&2
    fi

    if [ -z "$memory" ]; then memory=0; fi
    if [ -z "$cpu_user" ]; then cpu_user=0; fi
    if [ -z "$cpu_sys" ]; then cpu_sys=0; fi
    
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
