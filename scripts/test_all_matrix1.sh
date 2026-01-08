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
            continue
        fi

        # Run the test
        test_language "$lang"

        # Display result and update counters
        case "$TEST_STATUS" in
            pass)
                echo "PASS (iterations: $TEST_ITERATIONS)"
                COUNT_PASSED=$((COUNT_PASSED + 1))
                ;;
            compile_error)
                echo "COMPILE_ERROR ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "compile_error" "$lang" "$TEST_ERROR"
                ;;
            runtime_error)
                echo "RUNTIME_ERROR ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "runtime_error" "$lang" "$TEST_ERROR"
                ;;
            timeout)
                echo "TIMEOUT (exceeded 60s)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "timeout" "$lang" "$TEST_ERROR"
                ;;
            wrong_iterations)
                echo "WRONG_ITERATIONS ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "wrong_iterations" "$lang" "$TEST_ERROR"
                ;;
            *)
                echo "ERROR ($TEST_ERROR)"
                COUNT_FAILED=$((COUNT_FAILED + 1))
                record_failure "runtime_error" "$lang" "$TEST_ERROR"
                ;;
        esac
    done

    # Show final results if not interrupted
    if [[ "$INTERRUPTED" == false ]]; then
        echo ""
        echo "=== Results Summary ==="
        echo "Passed:  $COUNT_PASSED"
        echo "Failed:  $COUNT_FAILED"
        echo "Skipped: $COUNT_SKIPPED"
        echo "Total:   $COUNT_TOTAL"
    fi
}

main "$@"
