#!/usr/local/bin/bash

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
    # This function checks if a given number is valid at a given position on the Sudoku board.
    # It takes two arguments: the number to be checked and the position on the board.
    # It returns 1 if the number is invalid (already exists in the same row, column or 3x3 square), and 0 if the number is valid.
    # The function first calculates the row and column of the given position, then checks if the number already exists in the same row or column.
    # If the number does not exist in the same row or column, the function checks the 3x3 square that contains the given position.
    # If the number already exists in the 3x3 square, the function returns 1, otherwise it returns 0.
    local num=$1
    local pos=$2

    local row=$(( pos / 9 ))
    local col=$(( pos % 9 ))
    #echo "Is $num valid at $pos, row: $row, col: $col"

    # Check row and column
    for (( i=0; i<9; i++ )); do
        #echo "Is Valid: Row: board[$(( row*9 + i ))] ${board[$(( row*9 + i ))]} == $num || Col: board[$(( i*9 + col ))] ${board[$(( i*9 + col ))]} == $num ]] "
        if [[ ${board[$(( row*9 + i ))]} == $num || ${board[$(( i*9 + col ))]} == $num ]]; then
            return 1
        fi
    done

    # Check 3x3 square
    local startRow=$(( row / 3 * 3 ))
    local startCol=$(( col / 3 * 3 ))
    #echo "Check 3x3 startRow: $startRow, startCol: $startCol"
    for (( i=0; i<3; i++ )); do
        for (( j=0; j<3; j++ )); do
            #echo  "Is Valid: board[$(( (startRow + i)*9 + (startCol + j) ))] == $num"
            if [[ ${board[$(( (startRow + i)*9 + (startCol + j) ))]} == $num ]]; then
                #echo "Not In 3x3"
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
        return
    fi

    if [[ ${board[${pos}]} != 0 ]]; then
        solve $(( ${pos} + 1 ))
    else
        for (( num=1; num<=9; num++ )); do
            if is_valid $num $pos; then
                #echo "Is Valid: board[$pos]=$num"

                board[${pos}]=${num}
                solve $(( ${pos} + 1 ))
                #echo "Invalid: Setting board[$pos]=0 stack level: ${#FUNCNAME[@]}"
                board[$pos]=0 # Reset the cell to 0 if the solve function returns
                #echo "Shold be 1-9 $num"
            fi
        done
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

