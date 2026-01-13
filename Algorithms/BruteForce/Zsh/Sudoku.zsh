#!/bin/zsh
# Sudoku Solver in Zsh
# Iterative backtracking matching C algorithm fingerprint

# Global iterations
iterations=0

# The board is an array 1..81
typeset -a grid

print_grid() {
    for r in {0..8}; do
        line=""
        for c in {1..9}; do
            line+="${grid[$((r*9 + c))]} "
        done
        echo "$line"
    done
}

is_valid() {
    local row=$1 col=$2 v=$3
    local i j br bc idx
    
    # Row check
    for i in {1..9}; do
        if [[ "${grid[$(( (row-1)*9 + i ))]}" == "$v" ]]; then return 1; fi
    done
    
    # Col check
    for i in {0..8}; do
        if [[ "${grid[$(( i*9 + col ))]}" == "$v" ]]; then return 1; fi
    done
    
    # Box check
    br=$(( ((row-1) / 3) * 3 + 1 ))
    bc=$(( ((col-1) / 3) * 3 + 1 ))
    for i in {0..2}; do
        for j in {0..2}; do
            idx=$(( (br+i-1)*9 + bc + j ))
            if [[ "${grid[$idx]}" == "$v" ]]; then return 1; fi
        done
    done
    
    return 0
}

solve_iterative() {
    local -a empty_cells
    local i
    for i in {1..81}; do
        if [[ "${grid[$i]}" == "0" ]]; then
            empty_cells+=($i)
        fi
    done

    local num_empty=${#empty_cells[@]}
    if [[ $num_empty -eq 0 ]]; then return 0; fi

    local -a tried_vals
    for i in {1..$num_empty}; do tried_vals+=(0); done
    
    local sptr=1
    while [[ $sptr -gt 0 ]]; do
        local idx=${empty_cells[$sptr]}
        local last_val=${tried_vals[$sptr]}
        
        local r=$(( (idx-1) / 9 + 1 ))
        local c=$(( (idx-1) % 9 + 1 ))
        
        local found_valid=0
        local v=$(( last_val + 1 ))
        while [[ $v -le 9 ]]; do
            (( iterations++ ))
            if is_valid $r $c $v; then
                grid[$idx]=$v
                tried_vals[$sptr]=$v
                found_valid=1
                break
            fi
            (( v++ ))
        done

        if [[ $found_valid -eq 1 ]]; then
            if [[ $sptr -eq $num_empty ]]; then
                return 0 # Solved
            fi
            (( sptr++ ))
        else
            grid[$idx]=0
            tried_vals[$sptr]=0
            (( sptr-- ))
        fi
    done
    return 1
}

# Main
FILE=$1
if [[ -z "$FILE" ]]; then echo "Usage: $0 <file>"; exit 1; fi

echo "$FILE"
echo ""

# Read and initialize grid
raw_data=$(grep -v '^#' "$FILE" | tr -cd '0-9')
grid=(${(s::)raw_data})

echo "Puzzle:"
print_grid
echo ""

if solve_iterative; then
    echo "Puzzle:"
    print_grid
    echo ""
    echo "Solved in Iterations=$iterations"
else
    echo "No solution found."
fi