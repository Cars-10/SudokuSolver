#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"

# Function to report environment error
report_env_error() {
    echo "[" > "$METRICS_FILE"
    echo "  {" >> "$METRICS_FILE"
    echo "    \"solver\": \"Awk\"," >> "$METRICS_FILE"
    echo "    \"status\": \"error\"," >> "$METRICS_FILE"
    echo "    \"output\": \"Environment Not Ready: $1\"" >> "$METRICS_FILE"
    echo "  }" >> "$METRICS_FILE"
    echo "]" >> "$METRICS_FILE"
    cat "$METRICS_FILE"
    exit 1
}

# --- SETUP PHASE ---
# AWK is interpreted, so no compilation needed.
# We just check if the script exists.
if [ ! -f "./Sudoku.awk" ]; then
    report_env_error "Script Sudoku.awk not found"
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
        /usr/bin/time -l awk -f Sudoku.awk "$matrix" > "$tmp_out" 2> "$tmp_err"
        memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
        # Calculate duration using awk to avoid bc dependency issues (though we installed bc, awk is safer)
        end_time=$(date +%s.%N)
        duration=$(awk -v start=$start_time -v end=$end_time 'BEGIN { printf "%.6f", end - start }')
        cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
        cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    else
        # Linux (GNU time)
        # -v gives verbose output similar to BSD -l
        /usr/bin/time -v awk -f Sudoku.awk "$matrix" > "$tmp_out" 2> "$tmp_err"
        end_time=$(date +%s.%N)
        duration=$(awk -v start=$start_time -v end=$end_time 'BEGIN { printf "%.6f", end - start }')
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
        # We also need to handle the fact that the reference file has "Seconds to process" which we want to ignore in strict comparison
        # but the user said "Matches bit by bit except for the the computer timings".
        
        expected_block=$(awk -v target="../Matrices/$matrix_name" '
            $0 ~ target { found=1; next }
            found && $0 ~ /^\.\.\/Matrices\// { found=0; exit }
            found { print }
        ' "$REF_FILE")
        
        # Clean up "Seconds to process" from actual output for comparison
        # The actual output is in $tmp_out.
        # We also need to remove the first line if it's the filename (Sudoku.c prints filename first? Let's check Sudoku.c)
        # Sudoku.c: printf("%s\n", filename); at line 34.
        # The reference file starts with "../Matrices/1.matrix".
        # The awk script above skips the target line.
        # So expected_block starts after the filename line.
        
        # Let's prepare actual output for comparison.
        # 1. Remove the first line (filename) - Wait, Sudoku.c prints it.
        # AWK script prints "\nProcessing filename" at the start.
        # Let's check Sudoku.awk:
        # print "\nProcessing " filename
        # if (read_board(filename)) ...
        
        # So output is:
        # 
        # Processing ../Matrices/1.matrix
        # Puzzle:
        # ...
        
        # We need to strip the "Processing ..." line and the leading newline?
        # Let's just grep -v "Processing" and "Seconds to process"
        # 2. Remove "Seconds to process" line.
        
        # Actually, let's look at what Sudoku.c outputs vs Reference.
        # Reference:
        # ../Matrices/1.matrix
        # <blank line>
        # Puzzle: ...
        
        # Sudoku.c:
        # /Matrices/1.matrix (or whatever path passed)
        # <blank line> (maybe? Sudoku.c doesn't seem to print a blank line after filename immediately, but printPuzzle starts with \nPuzzle:)
        # printPuzzle: printf("\nPuzzle:\n"); -> So yes, newline then Puzzle.
        
        # So Sudoku.c output:
        # filename
        # 
        # Puzzle:
        
        # Reference output (from awk above, skipping target line):
        # 
        # Puzzle:
        
        # So they should match if we strip the filename from actual output.
        # And strip "Seconds to process" from both.
        
        # Normalize Actual: Remove first line (filename) and "Seconds to process"
        # We also need to handle that the reference file might have slightly different whitespace or newlines.
        # But user said "Matches bit by bit".
        
        # Let's try to match the body.
        # Actual output in $tmp_out contains filename at top.
        
        # Filter actual: remove "Processing" line and "Seconds to process"
        # Also remove the first empty line if it exists to match reference which has:
        # ../Matrices/1.matrix
        # <blank>
        # Puzzle:
        
        # Our actual output (after grep -v Processing):
        # Puzzle:
        # ...
        
        # The reference expected_content (after grep -v Seconds):
        # <blank>
        # Puzzle:
        # ...
        
        # So we need to ensure we don't have a mismatch on that blank line.
        # Let's just strip all blank lines from both for comparison to be robust?
        # Or just match the Puzzle part.
        
        actual_content=$(grep -v "Processing" "$tmp_out" | grep -v "Seconds to process")
        expected_content=$(echo "$expected_block" | grep -v "Seconds to process")
        
        # Filter expected: remove "Seconds to process" (it is in reference file)
        expected_content=$(echo "$expected_block" | grep -v "Seconds to process")
        
        # Trim leading/trailing whitespace to be safe? Or strict?
        # User said "Matches bit by bit".
        # Let's try strict comparison of the filtered content.
        
        # We need to be careful about the filename line in actual output.
        # Sudoku.c prints `printf("%s\n", filename);`
        # If we run with `/Matrices/1.matrix`, it prints `/Matrices/1.matrix`.
        # Reference has `../Matrices/1.matrix`.
        # So definitely ignore first line.
        
        # Diff them
        diff_output=$(diff <(echo "$actual_content") <(echo "$expected_content"))
        if [ $? -ne 0 ]; then
            # echo "DEBUG: Mismatch for $matrix_name" >&2
            # echo "DEBUG: Actual: $actual_content" >&2
            # echo "DEBUG: Expected: $expected_content" >&2
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
