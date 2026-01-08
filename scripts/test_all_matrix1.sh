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
#   TEST_STATUS  - pass/fail/timeout/error
#   TEST_ITERATIONS - Iteration count (0 if not parsed)
#   TEST_ERROR - Error message if applicable
#   TEST_OUTPUT - Full output from the test
#######################################
test_language() {
    local lang="$1"
    local lang_dir="/app/Languages/$lang"
    local matrix_path="../../Matrices/1.matrix"

    # Reset result variables
    TEST_STATUS="error"
    TEST_ITERATIONS=0
    TEST_ERROR=""
    TEST_OUTPUT=""

    # Execute test inside Docker with 60 second timeout
    # timeout returns 124 if command times out
    local exit_code
    TEST_OUTPUT=$(timeout 60 docker exec -w "$lang_dir" "$DOCKER_CONTAINER" ./runMe.sh "$matrix_path" 2>&1) || exit_code=$?

    # Check if we got an exit code (command failed or timed out)
    if [[ -z "$exit_code" ]]; then
        exit_code=0
    fi

    # Handle timeout (exit code 124)
    if [[ $exit_code -eq 124 ]]; then
        TEST_STATUS="timeout"
        TEST_ERROR="Test exceeded 60 second timeout"
        return
    fi

    # Handle other errors
    if [[ $exit_code -ne 0 ]]; then
        TEST_STATUS="error"
        TEST_ERROR="Exit code: $exit_code"
        # Try to extract meaningful error from output (first 200 chars)
        if [[ -n "$TEST_OUTPUT" ]]; then
            TEST_ERROR="$TEST_ERROR - ${TEST_OUTPUT:0:200}"
        fi
        return
    fi

    # Parse iteration count from output
    # Expected format: "Solved in Iterations=NNN"
    if [[ "$TEST_OUTPUT" =~ Solved\ in\ Iterations=([0-9]+) ]]; then
        TEST_ITERATIONS="${BASH_REMATCH[1]}"

        # Check if iterations match expected
        if [[ "$TEST_ITERATIONS" -eq "$EXPECTED_ITERATIONS" ]]; then
            TEST_STATUS="pass"
        else
            TEST_STATUS="fail"
            TEST_ERROR="Wrong iterations: expected $EXPECTED_ITERATIONS, got $TEST_ITERATIONS"
        fi
    else
        # Could not parse iterations - might be compile error or runtime error
        TEST_STATUS="error"
        TEST_ERROR="Could not parse iteration count from output"
        if [[ -n "$TEST_OUTPUT" ]]; then
            TEST_ERROR="$TEST_ERROR - ${TEST_OUTPUT:0:200}"
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

    # Placeholder for test loop (US-004)
    echo "Test execution will be implemented in US-004"
}

main "$@"
