#!/bin/bash
# Languages/common.sh - Shared functions for benchmark scripts
# Created: 2025-12-16
#
# This library provides reusable functions for all language implementations.
# Source this file in your language-specific runMe.sh script.
#
# Usage:
#   source ../common.sh
#   main "$@"

# ============================================================================
# CONFIGURATION (override in runMe.sh before sourcing)
# ============================================================================
LANGUAGE="${LANGUAGE:-Unknown}"
SOLVER_BINARY="${SOLVER_BINARY:-./Sudoku}"
COMPILE_CMD="${COMPILE_CMD:-}"
METRICS_FILE="${METRICS_FILE:-metrics.json}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"  # 5 minutes default

# ============================================================================
# ENVIRONMENT DETECTION
# ============================================================================

# Detect OS and set appropriate time command
detect_time_cmd() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS uses GNU time via gtime (install with: brew install gnu-time)
        if command -v gtime &> /dev/null; then
            TIME_CMD="gtime"
        else
            echo "WARNING: gtime not found on macOS. Install with: brew install gnu-time" >&2
            TIME_CMD="/usr/bin/time"
        fi
        # macOS uses gtimeout from coreutils (install with: brew install coreutils)
        if command -v gtimeout &> /dev/null; then
            TIMEOUT_CMD="gtimeout"
        else
            echo "WARNING: gtimeout not found on macOS. Install with: brew install coreutils" >&2
            TIMEOUT_CMD=""
        fi
    else
        # Linux uses /usr/bin/time and timeout
        TIME_CMD="/usr/bin/time"
        TIMEOUT_CMD="timeout"
    fi
}

# ============================================================================
# ERROR HANDLING
# ============================================================================

# Report environment error (missing compiler, dependencies, etc.)
# Logs error to benchmark_issues.json and exits
report_env_error() {
    local error_msg="$1"
    local timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    local issues_file="../../../benchmark_issues.json"

    # Use Python to append to the issues file safely
    python3 -c "
import json, os

issues_file = '$issues_file'
new_entry = {
    'solver': '$LANGUAGE',
    'runType': 'automated',
    'timestamp': '$timestamp',
    'status': 'env_error',
    'output': '''$error_msg'''
}

data = []
if os.path.exists(issues_file):
    try:
        with open(issues_file, 'r') as f:
            content = f.read().strip()
            if content:
                data = json.loads(content)
    except Exception as e:
        # If file is corrupt, start fresh but maybe log warning?
        print(f'Warning: Could not read {issues_file}: {e}')
        data = []

# Ensure data is a list
if not isinstance(data, list):
    data = []

# Append new entry
data.append(new_entry)

try:
    with open(issues_file, 'w') as f:
        json.dump(data, f, indent=2)
except Exception as e:
    print(f'Error writing to {issues_file}: {e}')
    exit(1)
"

    echo "ERROR: $error_msg" >&2
    exit 1
}

# ============================================================================
# OUTPUT PARSING
# ============================================================================

# Extract iteration count from solver output
# Expects format: "Solved in Iterations=656"
extract_iterations() {
    local output="$1"
    local iterations=$(echo "$output" | grep "Solved in Iterations=" | sed 's/.*Iterations=//')

    if [ -z "$iterations" ]; then
        echo "0"
    else
        echo "$iterations"
    fi
}

# ============================================================================
# MATRIX EXECUTION
# ============================================================================

# Run solver on a single matrix with timing and metrics capture
run_matrix() {
    local matrix_path="$1"
    local matrix_name=$(basename "$matrix_path")
    local matrix_num="${matrix_name%.matrix}"

    # Create temporary files for output and timing
    local temp_output=$(mktemp)
    local temp_timing=$(mktemp)

    # Python script to run command and measure metrics
    # Args: output_file, cmd...
    # We use python to ensure high precision timing (milliseconds) and standardized resource usage
    local python_cmd="
import sys, subprocess, time, resource, platform

output_file = sys.argv[1]
cmd = sys.argv[2:]

start = time.perf_counter()
try:
    # Run process and pipe output to file
    with open(output_file, 'w') as f:
        # We process stdout primarily, stderr can be passed through or captured if needed. 
        # Benchmark assumes solver output is on stdout. 
        # We explicitly don't pipe stderr to the file, to keep separate. 
        # But wait, original code > temp_output 2> temp_timing. 
        # Solver output goes to temp_output. 
        p = subprocess.run(cmd, stdout=f, stderr=sys.stderr)
        exit_code = p.returncode
except Exception as e:
    sys.stderr.write(str(e))
    exit_code = 1

end = time.perf_counter()
duration_ms = (end - start) * 1000

usage = resource.getrusage(resource.RUSAGE_CHILDREN)

# Normalize Max RSS to KB
# macOS: bytes, Linux: KB
max_rss = usage.ru_maxrss
if platform.system() == 'Darwin':
    max_rss = max_rss / 1024

cpu_user_ms = usage.ru_utime * 1000
cpu_sys_ms = usage.ru_stime * 1000

if (cpu_user_ms + cpu_sys_ms) == 0 and duration_ms > 0:
    cpu_user_ms = duration_ms

print(f'{duration_ms:.4f} {max_rss:.0f} {cpu_user_ms:.4f} {cpu_sys_ms:.4f} {usage.ru_majflt} {usage.ru_minflt} {usage.ru_nvcsw} {usage.ru_nivcsw} {usage.ru_inblock} {usage.ru_oublock} {exit_code}')
"

    # Construct the command list
    # Use array to handle spaces correctly
    local cmd_array=($SOLVER_BINARY "$matrix_path") 
    
    # Handle timeout if set
    # Note: Python subprocess doesn't easily handle 'timeout' wrapper behavior with rusage of the child *of* the timeout command?
    # Actually, if we use 'timeout' command, 'timeout' is the child. 
    # If the solver is killed, 'timeout' exits with 124.
    # We should wrap the whole thing.
    
    local full_cmd=()
    if [ -n "$TIMEOUT_CMD" ]; then
        full_cmd=("$TIMEOUT_CMD" "$TIMEOUT_SECONDS" "${cmd_array[@]}")
    else
        full_cmd=("${cmd_array[@]}")
    fi

    # Run via Python wrapper
    # We pass the temp_output path as first arg, then the command
    python3 -c "$python_cmd" "$temp_output" "${full_cmd[@]}" > "$temp_timing" 2>&1
    
    # Python script prints metrics to stdout (redirected to temp_timing)
    # It might print other stderr from the command too? 
    # The python script redirects child stderr to sys.stderr (which goes to temp_timing).
    # So temp_timing will contain stderr + our metrics line at the end? 
    # No, our script prints metrics to stdout. 
    # We redirected python stdout to temp_timing.
    # Child stderr went to python stderr -> shell stderr. We captured 2>&1 into temp_timing.
    # So temp_timing has mixed content. Metrics line should be parsed carefully.
    # Actually, let's print metrics to specific file or handle parsing.
    # Easy way: Print metrics to the very last line. tail -1 should still work if we ensure a newline.

    local timing_line=$(tail -1 "$temp_timing")
    
    # Parse timing data (format: time_ms memory_kb cpu_user cpu_sys page_faults_major page_faults_minor ctx_vol ctx_invol io_in io_out exit_code)
    # Note: added exit_code to the python output for robust status handling
    local time_ms=$(echo "$timing_line" | awk '{print $1}')
    local memory_kb=$(echo "$timing_line" | awk '{print $2}')
    local cpu_user=$(echo "$timing_line" | awk '{print $3}')
    local cpu_sys=$(echo "$timing_line" | awk '{print $4}')
    local page_faults_major=$(echo "$timing_line" | awk '{print $5}')
    local page_faults_minor=$(echo "$timing_line" | awk '{print $6}')
    local ctx_switches_vol=$(echo "$timing_line" | awk '{print $7}')
    local ctx_switches_invol=$(echo "$timing_line" | awk '{print $8}')
    local io_inputs=$(echo "$timing_line" | awk '{print $9}')
    local io_outputs=$(echo "$timing_line" | awk '{print $10}')
    local exit_code=$(echo "$timing_line" | awk '{print $11}')

    # Validate parsing
    if [[ -z "$exit_code" ]]; then
       # Fallback if something crashed hard
       exit_code=1
       status="error"
    fi

    # Read output
    local output=$(cat "$temp_output")

    # Determine status
    local status="success"
    if [ "$exit_code" -eq 124 ]; then
        status="timeout"
    elif [ "$exit_code" -ne 0 ]; then
        status="error"
    fi

    # Extract iterations from output
    local iterations=$(extract_iterations "$output")

    # Default values if parsing failed
    time_ms=${time_ms:-0}
    memory_kb=${memory_kb:-0}
    cpu_user=${cpu_user:-0}
    cpu_sys=${cpu_sys:-0}
    page_faults_major=${page_faults_major:-0}
    page_faults_minor=${page_faults_minor:-0}
    ctx_switches_vol=${ctx_switches_vol:-0}
    ctx_switches_invol=${ctx_switches_invol:-0}
    io_inputs=${io_inputs:-0}
    io_outputs=${io_outputs:-0}
    iterations=${iterations:-0}

    # Convert memory from KB to bytes (HTMLGenerator expects bytes)
    local memory_bytes=$((memory_kb * 1024))

    # Escape output for JSON
    local escaped_output=$(echo "$output" | python3 -c 'import json, sys; print(json.dumps(sys.stdin.read())[1:-1])')

    # Write JSON result object ONLY if success
    if [ "$status" == "success" ]; then
        cat <<EOF
    {
      "matrix": "$matrix_num",
      "time": $time_ms,
      "iterations": $iterations,
      "memory": $memory_bytes,
      "cpu_user": $cpu_user,
      "cpu_sys": $cpu_sys,
      "page_faults_major": $page_faults_major,
      "page_faults_minor": $page_faults_minor,
      "context_switches_voluntary": $ctx_switches_vol,
      "context_switches_involuntary": $ctx_switches_invol,
      "io_inputs": $io_inputs,
      "io_outputs": $io_outputs,
      "status": "$status",
      "output": "$escaped_output"
    }
EOF
    else
        # Log failure to benchmark_issues.json
        local issues_file="../../../benchmark_issues.json"
        local error_msg="Matrix $matrix_num failed: $status (Exit code $exit_code)"
        
        python3 -c "
import json, os

issues_file = '$issues_file'
new_entry = {
    'solver': '$LANGUAGE',
    'runType': 'automated',
    'timestamp': '$(date -u +%Y-%m-%dT%H:%M:%SZ)',
    'status': '$status',
    'matrix': '$matrix_num',
    'output': '''$error_msg'''
}

data = []
if os.path.exists(issues_file):
    try:
        with open(issues_file, 'r') as f:
            content = f.read().strip()
            if content:
                data = json.loads(content)
    except Exception:
        data = []

if not isinstance(data, list): data = []
data.append(new_entry)

try:
    with open(issues_file, 'w') as f:
        json.dump(data, f, indent=2)
except Exception:
    pass
"
    fi

    # Cleanup temp files
    rm -f "$temp_output" "$temp_timing"
}

# ============================================================================
# MAIN EXECUTION WRAPPER
# ============================================================================

# Run benchmarks for all specified matrices
# Usage: run_benchmarks [matrix_paths...]
# If no paths given, runs all matrices from ../../../Matrices/*.matrix
# (All algorithm dirs are at Algorithms/{Algorithm}/{Language}/ depth)
run_benchmarks() {
    local matrices="$@"
    local timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # If no matrices specified, use all available
    if [ -z "$matrices" ]; then
        matrices="../../../Matrices/*.matrix"
    fi

    # Expand glob pattern
    echo "DEBUG: matrices before ls: $matrices" >&2
    matrices=$(ls $matrices 2>/dev/null)
    echo "DEBUG: matrices after ls: $matrices" >&2

    if [ -z "$matrices" ]; then
        report_env_error "No matrix files found"
    fi

    # Detect time command for this platform
    detect_time_cmd

    # Load existing results if metrics.json exists (for merging)
    local existing_results=""
    if [ -f "$METRICS_FILE" ]; then
        # Extract existing results array using simple parsing
        existing_results=$(cat "$METRICS_FILE")
    fi

    # Collect new results in temp file
    local temp_results=$(mktemp)
    local first=true

    # Run each matrix
    for matrix in $matrices; do
        if [ ! -f "$matrix" ]; then
            echo "WARNING: Matrix file not found: $matrix" >&2
            continue
        fi

        echo "Running $LANGUAGE on $(basename $matrix)..." >&2

        # Run matrix and append result
        local result=$(run_matrix "$matrix")
        
        if [ -n "$result" ]; then
            # Add comma separator if not first result
            if [ "$first" = true ]; then
                first=false
            else
                echo "," >> "$temp_results"
            fi
            echo "$result" >> "$temp_results"
        fi
    done

    # Read new results
    local new_results=$(cat "$temp_results")
    rm -f "$temp_results"

    # If no results (all failed), exit without writing metrics.json
    if [ -z "$new_results" ]; then
        echo "No successful results to write." >&2
        return
    fi

    # Merge results: use node/python if available, otherwise just use new results
    if [ -n "$existing_results" ] && command -v node &> /dev/null; then
        # Create temp files for the JSON data to avoid shell escaping issues
        local temp_existing=$(mktemp)
        local temp_new=$(mktemp)
        echo "$existing_results" > "$temp_existing"
        echo "[$new_results]" > "$temp_new"

        # Use Node.js to merge results
        node -e "
            const fs = require('fs');
            const existing = JSON.parse(fs.readFileSync(process.argv[1], 'utf8'));
            const newResults = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
            const timestamp = process.argv[3];
            const solver = process.argv[4];

            // Get existing results array or create new
            let results = existing[0]?.results || [];

            // Create map of existing results by matrix
            const resultMap = new Map(results.map(r => [r.matrix, r]));

            // Update/add new results
            newResults.forEach(r => resultMap.set(r.matrix, r));

            // Convert back to array and sort by matrix number
            results = Array.from(resultMap.values()).sort((a, b) => {
                const numA = parseInt(a.matrix) || 0;
                const numB = parseInt(b.matrix) || 0;
                return numA - numB;
            });

            // Output merged JSON
            const output = [{
                solver: solver,
                runType: 'automated',
                timestamp: timestamp,
                results: results
            }];
            console.log(JSON.stringify(output, null, 2));
        " "$temp_existing" "$temp_new" "$timestamp" "$LANGUAGE" > "$METRICS_FILE"
        rm -f "$temp_existing" "$temp_new"
    else
        # No merge capability, just write new results
        cat > "$METRICS_FILE" <<EOF
[
  {
    "solver": "$LANGUAGE",
    "runType": "automated",
    "timestamp": "$timestamp",
    "results": [
$new_results
    ]
  }
]
EOF
    fi

    echo "Metrics written to $METRICS_FILE" >&2
}

# ============================================================================
# COMPILATION HELPERS (optional, override in language scripts)
# ============================================================================

# Default compilation function (can be overridden)
compile() {
    if [ -n "$COMPILE_CMD" ]; then
        echo "Compiling $LANGUAGE..." >&2
        eval "$COMPILE_CMD"

        if [ $? -ne 0 ]; then
            report_env_error "Compilation failed"
        fi

        echo "Compilation successful" >&2
    fi
}

# Check if compiler/interpreter is available
check_toolchain() {
    local tool="$1"

    if ! command -v "$tool" &> /dev/null; then
        report_env_error "$tool not found in PATH"
    fi
}

# ============================================================================
# MAIN ENTRY POINT (called from language scripts)
# ============================================================================

# Default main function - can be overridden or called from language script
main() {
    # Check if we should compile
    local should_compile=true
    local check_binary="$SOLVER_BINARY"
    
    # Handle complex binary strings (e.g. "java Class") by taking first word if it's a file
    # But usually SOLVER_BINARY="java Class" won't match -f.
    # For compiled langs, SOLVER_BINARY="./Sudoku".
    
    # Strip arch prefix for check if present (specific to our BASIC fix)
    if [[ "$check_binary" == "arch -x86_64 "* ]]; then
        check_binary="${check_binary#arch -x86_64 }"
    fi

    if [[ "$check_binary" == ./* && -f "$check_binary" && -z "$FORCE_COMPILE" ]]; then
        echo "Binary '$check_binary' exists. Skipping compilation. (Use FORCE_COMPILE=1 to rebuild)" >&2
        should_compile=false
    fi

    if [ "$should_compile" = true ]; then
        compile
    fi

    # Run benchmarks
    run_benchmarks "$@"
}

# ============================================================================
# EXPORTS
# ============================================================================
# All functions are available when this script is sourced

echo "common.sh loaded for language: $LANGUAGE" >&2
