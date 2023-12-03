#!/bin/bash

# Step 1: Function to read a Sudoku board from a file and return a 1D array
read_board() {
    local file="$1"
    local -a board=()
    while IFS=' ' read -r -a line; do
        for cell in "${line[@]}"; do
            if [[ "$cell" =~ ^[1-9]?$ ]]; then
                board+=("$cell")
            fi
        done
    done < "$file"
    echo "${board[@]}"
}

# Step 2: Function to calculate the complexity of a Sudoku matrix
complexity() {
    local -a board=("$@")
    local empty_count=0
    for cell in "${board[@]}"; do
        if [[ -z "$cell" ]]; then
            ((empty_count++))
        fi
    done
    echo "$empty_count"
}

# Step 3: Function to print the Sudoku board in a 9x9 grid
print_board() {
    local -a board=("$@")
    for ((i = 0; i < 81; i++)); do
        if [[ $((i % 9)) -eq 0 ]]; then
            echo ""
        fi
        if [[ -z "${board[i]}" ]]; then
            echo -n ". "
        else
            echo -n "${board[i]} "
        fi
    done
    echo ""
}

# Step 4: Function to solve the Sudoku board using backtracking algorithm
solve_board() {
    local -a board=("$@")
    local -i iterations=0

    # Recursive backtracking function
    backtrack() {
        ((iterations++))
        local i j n
        for ((i = 0; i < 81; i++)); do
            if [[ -z "${board[i]}" ]]; then
                for n in {1..9}; do
                    if is_valid_move "${board[@]}" "$i" "$n"; then
                        board[i]=$n
                        if backtrack "${board[@]}"; then
                            return 0
                        else
                            board[i]=''
                        fi
                    fi
                done
                return 1
            fi
        done
        return 0
    }

    # Helper function to check if placing n at position p is valid
    is_valid_move() {
        local -a board=("$@")
        local -i p="$2" n="$3"
        local -i row=$((p / 9)) col=$((p % 9))
        local -i row_start=$((row / 3 * 3)) col_start=$((col / 3 * 3))

        # Check row and column
        for i in {0..8}; do
            if [[ "${board[row * 9 + i]}" -eq n || "${board[i * 9 + col]}" -eq n ]]; then
                return 1
            fi
        done

        # Check 3x3 square
        for ((i = row_start; i < row_start + 3; i++)); do
            for ((j = col_start; j < col_start + 3; j++)); do
                if [[ "${board[i * 9 + j]}" -eq n ]]; then
                    return 1
                fi
            done
        done

        return 0
    }

    if backtrack "${board[@]}"; then
        echo "${board[@]}"
        echo "Solved in $iterations iterations"
    else
        echo "No solution found"
    fi
}

# Main execution
main() {
    local -a board
    local input_file="$1"
    local -i comp iterations

    board=($(read_board "$input_file"))
    print_board "${board[@]}"
    comp=$(complexity "${board[@]}")
    echo "Complexity: $comp"
    solve_board "${board[@]}"
}

# Step 6: Reading matrices from the command line and executing
if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <sudoku_puzzle_file>"
    exit 1
fi

main "$1"
