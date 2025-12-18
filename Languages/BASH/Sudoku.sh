#!/bin/bash
# Sudoku Solver in Bash
# Brute-force algorithm matching C reference exactly
# Counts EVERY placement attempt (not just valid ones)
#
# WARNING: Bash is inherently slow for this algorithm.
# Matrix 1 will complete, but larger matrices will likely timeout.

# Global variables
declare -a puzzle    # 1D array simulating 9x9 grid (index = row*9 + col)
count=0              # Iteration counter

# Get value at (row, col) - 0-indexed
get_cell() {
    local row=$1 col=$2
    local idx=$(( row * 9 + col ))
    echo "${puzzle[$idx]}"
}

# Set value at (row, col) - 0-indexed
set_cell() {
    local row=$1 col=$2 val=$3
    local idx=$(( row * 9 + col ))
    puzzle[$idx]=$val
}

# Print puzzle in standard format
print_puzzle() {
    echo ""
    echo "Puzzle:"
    for row in {0..8}; do
        for col in {0..8}; do
            local idx=$(( row * 9 + col ))
            printf "%d " "${puzzle[$idx]}"
        done
        echo ""
    done
}

# Read matrix file and populate puzzle
read_matrix_file() {
    local filename=$1

    if [[ ! -f "$filename" ]]; then
        echo "Error opening file '$filename'" >&2
        return 1
    fi

    # Print filename
    echo "$filename"

    local line_count=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Trim whitespace
        line=$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

        # Skip empty lines and comments
        [[ -z "$line" ]] && continue
        [[ "$line" == \#* ]] && continue

        # Parse 9 integers from line
        local -a values
        read -ra values <<< "$line"

        if [[ ${#values[@]} -eq 9 ]]; then
            if [[ $line_count -lt 9 ]]; then
                # Store in puzzle array
                for i in {0..8}; do
                    local idx=$(( line_count * 9 + i ))
                    puzzle[$idx]=${values[$i]}
                done

                # Print raw row as read
                printf "%d %d %d %d %d %d %d %d %d \n" "${values[@]}"

                (( line_count++ ))
            fi
        else
            echo "Error: line does not contain 9 integers" >&2
            return 1
        fi
    done < "$filename"

    return 0
}

# Check if placing val at (row, col) is valid
is_valid() {
    local row=$1 col=$2 val=$3

    # Check row
    for i in {0..8}; do
        local idx=$(( row * 9 + i ))
        if [[ ${puzzle[$idx]} -eq $val ]]; then
            return 1
        fi
    done

    # Check column
    for i in {0..8}; do
        local idx=$(( i * 9 + col ))
        if [[ ${puzzle[$idx]} -eq $val ]]; then
            return 1
        fi
    done

    # Check 3x3 box
    local box_row=$(( (row / 3) * 3 ))
    local box_col=$(( (col / 3) * 3 ))
    for i in {0..2}; do
        for j in {0..2}; do
            local idx=$(( (box_row + i) * 9 + (box_col + j) ))
            if [[ ${puzzle[$idx]} -eq $val ]]; then
                return 1
            fi
        done
    done

    return 0
}

# BRUTE-FORCE SOLVER
# Searches row-major order (top-to-bottom, left-to-right)
# Tries candidates 1-9 in ascending order
# Counts EVERY placement attempt (the algorithm fingerprint)
solve() {
    # Find first empty cell (row-major order)
    local empty_row=-1 empty_col=-1
    for r in {0..8}; do
        for c in {0..8}; do
            local idx=$(( r * 9 + c ))
            if [[ ${puzzle[$idx]} -eq 0 ]]; then
                empty_row=$r
                empty_col=$c
                break 2
            fi
        done
    done

    # If no empty cell found, puzzle is solved
    if [[ $empty_row -eq -1 ]]; then
        print_puzzle
        echo ""
        echo "Solved in Iterations=$count"
        echo ""
        return 0  # Success
    fi

    # Try values 1-9 in order
    for val in {1..9}; do
        (( count++ ))  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if is_valid $empty_row $empty_col $val; then
            # Place value
            set_cell $empty_row $empty_col $val

            if solve; then
                return 0  # Solved
            fi

            # Backtrack
            set_cell $empty_row $empty_col 0
        fi
    done

    return 1  # No solution found
}

# Initialize puzzle array
init_puzzle() {
    puzzle=()
    for i in {0..80}; do
        puzzle[$i]=0
    done
}

# Main execution
main() {
    local start_time=$(date +%s.%N 2>/dev/null || date +%s)

    # Process each .matrix file from command line
    for filename in "$@"; do
        if [[ "$filename" == *.matrix ]]; then
            init_puzzle

            if read_matrix_file "$filename"; then
                print_puzzle
                count=0
                solve
            else
                echo "Error reading $filename" >&2
            fi
        fi
    done

    local end_time=$(date +%s.%N 2>/dev/null || date +%s)
    local duration=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "0")
    printf "Seconds to process %.3f\n" "$duration"
}

# Run main with all arguments
main "$@"
