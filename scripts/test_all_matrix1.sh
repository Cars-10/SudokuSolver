#!/bin/bash
#
# test_all_matrix1.sh - Test all language implementations against Matrix 1
#
# This script iterates through all language directories and tests each
# implementation against the simplest puzzle (Matrix 1) which should
# complete in exactly 656 iterations.
#

set -e

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LANGUAGES_DIR="$PROJECT_ROOT/Languages"

# Default options
VERBOSE=false
QUIET=false
JSON_OUTPUT=false
SINGLE_LANGUAGE=""

# Expected iterations for Matrix 1
EXPECTED_ITERATIONS=656

# Docker container (set by validate_docker)
DOCKER_CONTAINER=""

# Counters for results
COUNT_PASSED=0
COUNT_FAILED=0
COUNT_SKIPPED=0
COUNT_TOTAL=0

# Failure counts by category
COUNT_COMPILE_ERROR=0
COUNT_RUNTIME_ERROR=0
COUNT_TIMEOUT=0
COUNT_WRONG_ITERATIONS=0
COUNT_MISSING_RUNME=0

# Arrays to track failed languages by category
FAILED_COMPILE_ERROR=()
FAILED_RUNTIME_ERROR=()
FAILED_TIMEOUT=()
FAILED_WRONG_ITERATIONS=()
FAILED_MISSING_RUNME=()

# Results directory and log file
RESULTS_DIR="$PROJECT_ROOT/test_results"
FAILURES_LOG="$RESULTS_DIR/matrix1_failures.log"
SUMMARY_FILE="$RESULTS_DIR/matrix1_summary.txt"
JSON_FILE="$RESULTS_DIR/matrix1_results.json"

# Array to store individual test results for JSON output
# Each entry is: "language|status|iterations|time_seconds|error"
declare -a JSON_RESULTS

# Timing
START_TIME=""
END_TIME=""

# Flag to track if interrupted
INTERRUPTED=false

#######################################
# Handle SIGINT (Ctrl+C) gracefully
#######################################
handle_interrupt() {
    echo ""
    echo ""
    echo "=== Interrupted by user (Ctrl+C) ==="
    INTERRUPTED=true
}

#######################################
# Display partial results summary
#######################################
show_partial_results() {
    echo ""
    echo "=== Partial Results (Interrupted) ==="
    echo "Passed:  $COUNT_PASSED"
    echo "Failed:  $COUNT_FAILED"
    echo "Skipped: $COUNT_SKIPPED"
    echo "Tested:  $((COUNT_PASSED + COUNT_FAILED + COUNT_SKIPPED)) / $COUNT_TOTAL"
}

#######################################
# Generate summary report
# Displays to stdout and writes to summary file
#######################################
generate_summary() {
    END_TIME=$(date +%s)
    local elapsed=$((END_TIME - START_TIME))
    local hours=$((elapsed / 3600))
    local minutes=$(((elapsed % 3600) / 60))
    local seconds=$((elapsed % 60))
    local time_str

    if [[ $hours -gt 0 ]]; then
        time_str="${hours}h ${minutes}m ${seconds}s"
    elif [[ $minutes -gt 0 ]]; then
        time_str="${minutes}m ${seconds}s"
    else
        time_str="${seconds}s"
    fi

    # Build the summary output
    local summary=""
    summary+="
========================================
        MATRIX 1 TEST SUMMARY
========================================

Test completed: $(date)
Total execution time: $time_str

--- RESULTS ---
Total:    $COUNT_TOTAL
Passed:   $COUNT_PASSED
Failed:   $COUNT_FAILED
Skipped:  $COUNT_SKIPPED

--- FAILURE BREAKDOWN ---
Compile Errors:     $COUNT_COMPILE_ERROR
Runtime Errors:     $COUNT_RUNTIME_ERROR
Timeouts:           $COUNT_TIMEOUT
Wrong Iterations:   $COUNT_WRONG_ITERATIONS
Missing runMe.sh:   $COUNT_MISSING_RUNME
"

    # Add failed language lists by category
    if [[ ${#FAILED_COMPILE_ERROR[@]} -gt 0 ]]; then
        summary+="
--- COMPILE ERRORS ---
"
        for lang in "${FAILED_COMPILE_ERROR[@]}"; do
            summary+="  - $lang
"
        done
    fi

    if [[ ${#FAILED_RUNTIME_ERROR[@]} -gt 0 ]]; then
        summary+="
--- RUNTIME ERRORS ---
"
        for lang in "${FAILED_RUNTIME_ERROR[@]}"; do
            summary+="  - $lang
"
        done
    fi

    if [[ ${#FAILED_TIMEOUT[@]} -gt 0 ]]; then
        summary+="
--- TIMEOUTS ---
"
        for lang in "${FAILED_TIMEOUT[@]}"; do
            summary+="  - $lang
"
        done
    fi

    if [[ ${#FAILED_WRONG_ITERATIONS[@]} -gt 0 ]]; then
        summary+="
--- WRONG ITERATIONS ---
"
        for lang in "${FAILED_WRONG_ITERATIONS[@]}"; do
            summary+="  - $lang
"
        done
    fi

    if [[ ${#FAILED_MISSING_RUNME[@]} -gt 0 ]]; then
        summary+="
--- MISSING runMe.sh ---
"
        for lang in "${FAILED_MISSING_RUNME[@]}"; do
            summary+="  - $lang
"
        done
    fi

    summary+="
========================================
"

    # Display to stdout
    echo "$summary"

    # Write to summary file
    echo "$summary" > "$SUMMARY_FILE"
    echo "Summary written to: $SUMMARY_FILE"
}

#######################################
# Record a result for JSON output
# Arguments:
#   $1 - Language name
#   $2 - Status (pass/fail/skip/etc)
#   $3 - Iterations count
#   $4 - Time in seconds
#   $5 - Error message (optional)
#######################################
record_json_result() {
    local language="$1"
    local status="$2"
    local iterations="$3"
    local time_secs="$4"
    local error="${5:-}"

    # Replace spaces and special chars in error message for pipe-delimited format
    # Use base64 encoding for error messages to safely handle special characters
    local encoded_error
    encoded_error=$(echo -n "$error" | base64 | tr -d '\n')

    # Store as pipe-delimited entry
    JSON_RESULTS+=("${language}|${status}|${iterations}|${time_secs}|${encoded_error}")
}

#######################################
# Generate JSON output
# Writes to JSON_FILE and optionally to stdout
#######################################
generate_json() {
    END_TIME=$(date +%s)
    local elapsed=$((END_TIME - START_TIME))
    local timestamp
    timestamp=$(date -Iseconds 2>/dev/null || date +"%Y-%m-%dT%H:%M:%S%z")

    # Build JSON using python3 for proper escaping
    local json_output
    json_output=$(python3 << PYEOF
import json
import sys
import base64

# Build the results array from the pipe-delimited entries
results = []
raw_results = '''${JSON_RESULTS[*]:-}'''

for entry in raw_results.split():
    if not entry or '|' not in entry:
        continue
    parts = entry.split('|', 4)  # Split into 5 parts max
    if len(parts) >= 4:
        language = parts[0]
        status = parts[1]
        iterations = int(parts[2]) if parts[2].isdigit() else 0
        try:
            time_secs = float(parts[3])
        except (ValueError, TypeError):
            time_secs = 0.0
        # Decode base64-encoded error message
        encoded_error = parts[4] if len(parts) > 4 else ""
        try:
            error = base64.b64decode(encoded_error).decode('utf-8') if encoded_error else ""
        except:
            error = ""
        results.append({
            "language": language,
            "status": status,
            "iterations": iterations,
            "time_seconds": round(time_secs, 3),
            "error": error
        })

# Build the full JSON structure
output = {
    "timestamp": "$timestamp",
    "total_time_seconds": $elapsed,
    "summary": {
        "total": $COUNT_TOTAL,
        "passed": $COUNT_PASSED,
        "failed": $COUNT_FAILED,
        "skipped": $COUNT_SKIPPED
    },
    "results": results
}

print(json.dumps(output, indent=2))
PYEOF
)

    # Write JSON to file
    echo "$json_output" > "$JSON_FILE"
    echo "JSON results written to: $JSON_FILE"

    # Output to stdout if --json flag is set
    if [[ "$JSON_OUTPUT" == true ]]; then
        echo ""
        echo "=== JSON OUTPUT ==="
        echo "$json_output"
    fi
}

#######################################
# Initialize results directory and log file
#######################################
init_results_dir() {
    # Create test_results directory if it doesn't exist
    if [[ ! -d "$RESULTS_DIR" ]]; then
        mkdir -p "$RESULTS_DIR"
    fi

    # Append timestamp header to log file
    echo "" >> "$FAILURES_LOG"
    echo "========================================" >> "$FAILURES_LOG"
    echo "Test Run: $(date)" >> "$FAILURES_LOG"
    echo "========================================" >> "$FAILURES_LOG"
}

#######################################
# Log a failure to the failures log file
# Arguments:
#   $1 - Category (compile_error, runtime_error, timeout, wrong_iterations, missing_runme)
#   $2 - Language name
#   $3 - Error snippet/message
#######################################
log_failure() {
    local category="$1"
    local language="$2"
    local error_msg="$3"

    # Write to log file in format: [CATEGORY] LanguageName: error snippet
    local category_upper
    category_upper=$(echo "$category" | tr '[:lower:]' '[:upper:]')
    echo "[$category_upper] $language: $error_msg" >> "$FAILURES_LOG"
}

#######################################
# Record a failure with categorization
# Arguments:
#   $1 - Category (compile_error, runtime_error, timeout, wrong_iterations, missing_runme)
#   $2 - Language name
#   $3 - Error message
#######################################
record_failure() {
    local category="$1"
    local language="$2"
    local error_msg="$3"

    # Track in category arrays and increment category counters
    case "$category" in
        compile_error)
            FAILED_COMPILE_ERROR+=("$language")
            COUNT_COMPILE_ERROR=$((COUNT_COMPILE_ERROR + 1))
            ;;
        runtime_error)
            FAILED_RUNTIME_ERROR+=("$language")
            COUNT_RUNTIME_ERROR=$((COUNT_RUNTIME_ERROR + 1))
            ;;
        timeout)
            FAILED_TIMEOUT+=("$language")
            COUNT_TIMEOUT=$((COUNT_TIMEOUT + 1))
            ;;
        wrong_iterations)
            FAILED_WRONG_ITERATIONS+=("$language")
            COUNT_WRONG_ITERATIONS=$((COUNT_WRONG_ITERATIONS + 1))
            ;;
        missing_runme)
            FAILED_MISSING_RUNME+=("$language")
            COUNT_MISSING_RUNME=$((COUNT_MISSING_RUNME + 1))
            ;;
    esac

    # Log to file
    log_failure "$category" "$language" "$error_msg"
}

#######################################
# Display usage information
#######################################
show_help() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Test all Sudoku solver implementations against Matrix 1.

Options:
  --help              Show this help message and exit
  --language <name>   Test only the specified language
  --verbose           Show full output from each language test
  --quiet             Suppress per-language progress (only show summary)
  --json              Output results as JSON

Examples:
  $(basename "$0")                    # Test all languages
  $(basename "$0") --language C       # Test only C implementation
  $(basename "$0") --verbose          # Show detailed output
  $(basename "$0") --json             # Output results as JSON

Matrix 1 Reference:
  Expected iterations: $EXPECTED_ITERATIONS
  Any implementation reporting a different count has an algorithm bug.
EOF
}

#######################################
# Parse command line arguments
#######################################
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help)
                show_help
                exit 0
                ;;
            --language)
                if [[ -z "$2" || "$2" == --* ]]; then
                    echo "Error: --language requires a language name argument"
                    exit 1
                fi
                SINGLE_LANGUAGE="$2"
                shift 2
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            --quiet)
                QUIET=true
                shift
                ;;
            --json)
                JSON_OUTPUT=true
                shift
                ;;
            *)
                echo "Error: Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done

    # Quiet wins if both specified
    if [[ "$VERBOSE" == true && "$QUIET" == true ]]; then
        VERBOSE=false
    fi
}

#######################################
# Validate Docker environment
# Sets DOCKER_CONTAINER on success, exits on failure
#######################################
validate_docker() {
    # Check if docker command is available
    if ! command -v docker &> /dev/null; then
        echo "Error: docker command not found"
        echo "Please install Docker: https://docs.docker.com/get-docker/"
        exit 1
    fi

    # Look for running container named 'sudoku-benchmark' or from sudoku-benchmark image
    # First try to find by container name
    local container_id
    container_id=$(docker ps --filter "name=sudoku-benchmark" --format "{{.ID}}" 2>/dev/null | head -n1)

    # If not found by name, try by image name
    if [[ -z "$container_id" ]]; then
        container_id=$(docker ps --filter "ancestor=sudoku-benchmark" --format "{{.ID}}" 2>/dev/null | head -n1)
    fi

    # Also check for image name with tag variations
    if [[ -z "$container_id" ]]; then
        container_id=$(docker ps --filter "ancestor=sudoku-benchmark:latest" --format "{{.ID}}" 2>/dev/null | head -n1)
    fi

    if [[ -z "$container_id" ]]; then
        echo "Error: sudoku-benchmark container not running. Start with: docker-compose up -d"
        exit 1
    fi

    DOCKER_CONTAINER="$container_id"
    echo "Using Docker container: $DOCKER_CONTAINER"
}

#######################################
# Test a single language against Matrix 1
# Arguments:
#   $1 - Language name
# Sets global variables:
#   TEST_STATUS  - pass/compile_error/runtime_error/timeout/wrong_iterations
#   TEST_ITERATIONS - Iteration count (0 if not parsed)
#   TEST_ERROR - Error message if applicable
#   TEST_OUTPUT - Full output from the test
#   TEST_TIME_SECONDS - Time taken for this test in seconds
#######################################
test_language() {
    local lang="$1"
    local lang_dir="/app/Languages/$lang"
    local matrix_path="../../Matrices/1.matrix"

    # Reset result variables
    TEST_STATUS="runtime_error"
    TEST_ITERATIONS=0
    TEST_ERROR=""
    TEST_OUTPUT=""
    TEST_TIME_SECONDS=0

    # Start timing this test
    local test_start_time
    test_start_time=$(date +%s.%N 2>/dev/null || date +%s)

    # Execute test inside Docker with 60 second timeout
    # timeout returns 124 if command times out
    local exit_code=0
    TEST_OUTPUT=$(timeout 60 docker exec -w "$lang_dir" "$DOCKER_CONTAINER" ./runMe.sh "$matrix_path" 2>&1) || exit_code=$?

    # Handle timeout (exit code 124)
    if [[ $exit_code -eq 124 ]]; then
        TEST_STATUS="timeout"
        TEST_ERROR="Test exceeded 60 second timeout"
        return
    fi

    # Handle other errors - try to distinguish compile vs runtime errors
    if [[ $exit_code -ne 0 ]]; then
        # Check output for common compilation error indicators
        if echo "$TEST_OUTPUT" | grep -qiE "(compile|compilation|cannot find|undefined reference|syntax error|error:.*\.c:|error:.*\.cpp:|error:.*\.go:|error:.*\.rs:|cannot execute|command not found|No such file)"; then
            TEST_STATUS="compile_error"
            TEST_ERROR="Compilation failed"
        else
            TEST_STATUS="runtime_error"
            TEST_ERROR="Exit code: $exit_code"
        fi
        # Try to extract meaningful error from output (first 200 chars)
        if [[ -n "$TEST_OUTPUT" ]]; then
            TEST_ERROR="$TEST_ERROR - ${TEST_OUTPUT:0:200}"
        fi
        return
    fi

    # runMe.sh writes results to metrics.json - read it to get the iteration count
    # The metrics.json contains an array with results for each matrix tested
    local metrics_output
    metrics_output=$(docker exec -w "$lang_dir" "$DOCKER_CONTAINER" cat metrics.json 2>/dev/null) || true

    if [[ -z "$metrics_output" ]]; then
        TEST_STATUS="runtime_error"
        TEST_ERROR="Could not read metrics.json"
        return
    fi

    # Parse the metrics.json for matrix 1 results
    # Look for status and iterations in the results array
    local status iterations output_field

    # Extract the result for matrix "1" - simple grep/sed approach
    # Looking for: "matrix": "1", followed by "iterations": N and "status": "..."
    if echo "$metrics_output" | grep -q '"matrix": "1"'; then
        # Get the iterations value for matrix 1
        # The JSON structure has results array with matrix, iterations, status fields
        iterations=$(echo "$metrics_output" | python3 -c "
import json, sys
data = json.load(sys.stdin)
for entry in data:
    for result in entry.get('results', []):
        if result.get('matrix') == '1':
            print(result.get('iterations', 0))
            sys.exit(0)
print(0)
" 2>/dev/null) || iterations=0

        status=$(echo "$metrics_output" | python3 -c "
import json, sys
data = json.load(sys.stdin)
for entry in data:
    for result in entry.get('results', []):
        if result.get('matrix') == '1':
            print(result.get('status', 'unknown'))
            sys.exit(0)
print('unknown')
" 2>/dev/null) || status="unknown"

        output_field=$(echo "$metrics_output" | python3 -c "
import json, sys
data = json.load(sys.stdin)
for entry in data:
    for result in entry.get('results', []):
        if result.get('matrix') == '1':
            print(result.get('output', '')[:200])
            sys.exit(0)
print('')
" 2>/dev/null) || output_field=""

        TEST_ITERATIONS="$iterations"

        # Check status from metrics
        if [[ "$status" == "success" ]]; then
            # Check if iterations match expected
            if [[ "$TEST_ITERATIONS" -eq "$EXPECTED_ITERATIONS" ]]; then
                TEST_STATUS="pass"
            else
                TEST_STATUS="wrong_iterations"
                TEST_ERROR="Wrong iterations: expected $EXPECTED_ITERATIONS, got $TEST_ITERATIONS"
            fi
        elif [[ "$status" == "timeout" ]]; then
            TEST_STATUS="timeout"
            TEST_ERROR="Solver timed out"
        else
            TEST_STATUS="runtime_error"
            TEST_ERROR="Solver status: $status"
            if [[ -n "$output_field" ]]; then
                TEST_ERROR="$TEST_ERROR - ${output_field:0:200}"
            fi
        fi
    else
        # Fallback: try to parse iteration count from the console output
        if [[ "$TEST_OUTPUT" =~ Solved\ in\ Iterations=([0-9]+) ]]; then
            TEST_ITERATIONS="${BASH_REMATCH[1]}"

            if [[ "$TEST_ITERATIONS" -eq "$EXPECTED_ITERATIONS" ]]; then
                TEST_STATUS="pass"
            else
                TEST_STATUS="wrong_iterations"
                TEST_ERROR="Wrong iterations: expected $EXPECTED_ITERATIONS, got $TEST_ITERATIONS"
            fi
        else
            TEST_STATUS="runtime_error"
            TEST_ERROR="Could not parse iteration count"
            if [[ -n "$TEST_OUTPUT" ]]; then
                TEST_ERROR="$TEST_ERROR - ${TEST_OUTPUT:0:200}"
            fi
        fi
    fi

    # Calculate elapsed time for this test
    local test_end_time
    test_end_time=$(date +%s.%N 2>/dev/null || date +%s)
    TEST_TIME_SECONDS=$(echo "$test_end_time - $test_start_time" | bc 2>/dev/null || echo "0")
}

#######################################
# Discover all language directories
# Excludes hidden directories and files
#######################################
discover_languages() {
    local languages=()

    for entry in "$LANGUAGES_DIR"/*; do
        # Skip if not a directory
        [[ ! -d "$entry" ]] && continue

        local name
        name=$(basename "$entry")

        # Skip hidden directories
        [[ "$name" == .* ]] && continue

        languages+=("$name")
    done

    # Sort alphabetically
    IFS=$'\n' languages=($(sort <<< "${languages[*]}"))
    unset IFS

    echo "${languages[@]}"
}

#######################################
# Main entry point
#######################################
main() {
    parse_args "$@"

    # Record start time for execution time calculation
    START_TIME=$(date +%s)

    # Output start timestamp
    echo "=== Matrix 1 Test Suite ==="
    echo "Started: $(date)"
    echo ""

    # Validate Docker environment
    validate_docker
    echo ""

    # Initialize results directory and log file
    init_results_dir

    # Discover languages
    local all_languages
    read -ra all_languages <<< "$(discover_languages)"

    local total=${#all_languages[@]}
    echo "Found $total language implementations in $LANGUAGES_DIR"

    # If single language specified, validate it exists
    if [[ -n "$SINGLE_LANGUAGE" ]]; then
        local found=false
        for lang in "${all_languages[@]}"; do
            if [[ "$lang" == "$SINGLE_LANGUAGE" ]]; then
                found=true
                break
            fi
        done

        if [[ "$found" == false ]]; then
            echo "Error: Language '$SINGLE_LANGUAGE' not found in $LANGUAGES_DIR"
            echo "Available languages: ${all_languages[*]}"
            exit 1
        fi

        echo "Testing single language: $SINGLE_LANGUAGE"
        all_languages=("$SINGLE_LANGUAGE")
    fi

    echo ""

    # Set up SIGINT trap for graceful Ctrl+C handling
    trap handle_interrupt SIGINT

    # Set total count for results display
    COUNT_TOTAL=${#all_languages[@]}

    # Main test loop
    local current=0
    for lang in "${all_languages[@]}"; do
        # Check if interrupted
        if [[ "$INTERRUPTED" == true ]]; then
            show_partial_results
            exit 130
        fi

        current=$((current + 1))

        # Display progress
        echo -n "Testing [$current/$COUNT_TOTAL]: $lang... "

        # Check if runMe.sh exists for this language
        if ! docker exec "$DOCKER_CONTAINER" test -f "/app/Languages/$lang/runMe.sh" 2>/dev/null; then
            echo "SKIPPED (no runMe.sh)"
            COUNT_SKIPPED=$((COUNT_SKIPPED + 1))
            record_failure "missing_runme" "$lang" "No runMe.sh file found"
            record_json_result "$lang" "skipped" 0 0 "No runMe.sh file found"
            continue
        fi

        # Run the test
        test_language "$lang"

        # Display result and update counters
        case "$TEST_STATUS" in
            pass)
                echo "PASS (iterations: $TEST_ITERATIONS)"
                COUNT_PASSED=$((COUNT_PASSED + 1))
                record_json_result "$lang" "pass" "$TEST_ITERATIONS" "$TEST_TIME_SECONDS" ""
                ;;
            compile_error)
                echo "COMPILE_ERROR ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "compile_error" "$lang" "$TEST_ERROR"
                record_json_result "$lang" "compile_error" "$TEST_ITERATIONS" "$TEST_TIME_SECONDS" "$TEST_ERROR"
                ;;
            runtime_error)
                echo "RUNTIME_ERROR ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "runtime_error" "$lang" "$TEST_ERROR"
                record_json_result "$lang" "runtime_error" "$TEST_ITERATIONS" "$TEST_TIME_SECONDS" "$TEST_ERROR"
                ;;
            timeout)
                echo "TIMEOUT (exceeded 60s)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "timeout" "$lang" "$TEST_ERROR"
                record_json_result "$lang" "timeout" "$TEST_ITERATIONS" "$TEST_TIME_SECONDS" "$TEST_ERROR"
                ;;
            wrong_iterations)
                echo "WRONG_ITERATIONS ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "wrong_iterations" "$lang" "$TEST_ERROR"
                record_json_result "$lang" "wrong_iterations" "$TEST_ITERATIONS" "$TEST_TIME_SECONDS" "$TEST_ERROR"
                ;;
            *)
                echo "ERROR ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "runtime_error" "$lang" "$TEST_ERROR"
                record_json_result "$lang" "error" "$TEST_ITERATIONS" "$TEST_TIME_SECONDS" "$TEST_ERROR"
                ;;
        esac
    done

    # Generate full summary report if not interrupted
    if [[ "$INTERRUPTED" == false ]]; then
        generate_summary
        generate_json
    fi
}

main "$@"
