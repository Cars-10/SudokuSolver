#!/usr/bin/env bash

# Define the board as an array
declare -a board
iterations = 0


# Function to print the board
# Function to print the Sudoku board
# Usage: print_board
# Arguments: None
# Output: Prints the current state of the Sudoku board
print_board() {
    echo
    echo "Puzzle:"
    for (( i=0; i<81; i++ )); do
        echo -n "${board[$i]} "
        if (( (i+1) % 9 == 0 )); then
            echo
        fi
    done
    echo
}

# Function to check if a move is valid
is_valid() {
    local num=$1
    local row=$2
    local col=$3
    local startRow=$4
    local startCol=$5

    # Check row and column
    for (( i=0; i<9; i++ )); do
        if [[ ${board[$(( row*9 + i ))]} == $num || ${board[$(( i*9 + col ))]} == $num ]]; then
            return 1
        fi
    done

    # Check 3x3 square
    for (( i=0; i<3; i++ )); do
        for (( j=0; j<3; j++ )); do
            if [[ ${board[$(( (startRow + i)*9 + (startCol + j) ))]} == $num ]]; then
                return 1
            fi
        done
    done

    return 0
}


# Function to read the board from a file
read_board() {
    local filename="$1"
    if [[ ! -f $filename ]]; then
        echo "Error: File not found."
        exit 1
    fi

    local index=0
    while IFS=' ' read -ra line; do
        # Check if the line starts with a #
        if [[ ${line[0]} == \#* ]]; then
            continue
        fi
        #echo "line: ${line[@]}"
        for num in "${line[@]}"; do
            board[$index]=$num
            #echo "board[$index]=$num"
            (( index++ ))
        done
    done < "$filename"
}


# Recursive function to solve the board
# This function solves a Sudoku puzzle recursively using backtracking.
# It takes a single argument, the current position on the board to be filled.
# If the position is the last cell on the board, the function prints the solved board and exits.
# If the current cell is already filled, the function moves to the next cell.
# If the current cell is empty, the function tries to fill it with a valid number (1-9).
# If a valid number is found, the function moves to the next cell and repeats the process.
# If no valid number is found, the function backtracks to the previous cell and tries a different number.
# The function uses the is_valid function to check if a number is valid in a given cell.
# The function uses the print_board function to print the solved board.
solve() {
    iterations=$(( iterations + 1 ))
    local pos=$1
    local num
    #echo "Solve: pos: $pos, ${board[$pos]} stack level: ${#FUNCNAME[@]}"

    if [[ $pos == 81 ]]; then
        print_board
        return 0
    fi

    if [[ ${board[${pos}]} != 0 ]]; then
        if solve $(( ${pos} + 1 )); then
            return 0
        else
            return 1
        fi
    else
        local row=$(( pos / 9 ))
        local col=$(( pos % 9 ))
        local startRow=$(( row / 3 * 3 ))
        local startCol=$(( col / 3 * 3 ))

        for (( num=1; num<=9; num++ )); do
            if is_valid $num $row $col $startRow $startCol; then
                #echo "Is Valid: board[$pos]=$num"

                board[${pos}]=${num}
                if solve $(( ${pos} + 1 )); then
                    return 0
                fi
                #echo "Invalid: Setting board[$pos]=0 stack level: ${#FUNCNAME[@]}"
                board[$pos]=0 # Reset the cell to 0 if the solve function returns
                #echo "Shold be 1-9 $num"
            fi
        done
        return 1
    fi
}

# Main
# ...

# Main
if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <path_to_sudoku_file1> [<path_to_sudoku_file2> ...]"
    exit 1
fi

for file in $@; do
    echo
    echo "Processing file: $file"
    read_board "$file"
    print_board
    solve 0
    echo -e "\nSolved in Iterations=$iterations\n"
    iterations=0

done

echo -e "\nSolved in Iterations=$iterations\n"

