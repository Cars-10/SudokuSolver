#!/bin/ksh
# Sudoku Solver in Ksh
# Iterative backtracking matching C algorithm fingerprint

# Global iterations
iterations=0

# The board is an array 0..80
set -A grid

print_grid() {
    typeset -i pr=0
    while (( pr < 9 )); do
        line=""
        typeset -i pc=0
        while (( pc < 9 )); do
            line+="${grid[$((pr*9 + pc))]} "
            (( pc++ ))
        done
        echo "$line"
        (( pr++ ))
    done
}

is_valid() {
    typeset -i vrow=$1 vcol=$2 vv=$3
    typeset -i vk vbr vbc vidx
    
    # Row check
    vk=0
    while (( vk < 9 )); do
        if [[ "${grid[$(( vrow*9 + vk ))]}" == "$vv" ]]; then return 1; fi
        (( vk++ ))
    done
    
    # Col check
    vk=0
    while (( vk < 9 )); do
        if [[ "${grid[$(( vk*9 + vcol ))]}" == "$vv" ]]; then return 1; fi
        (( vk++ ))
    done
    
    # Box check
    vbr=$(( (vrow / 3) * 3 ))
    vbc=$(( (vcol / 3) * 3 ))
    vk=0
    while (( vk < 3 )); do
        typeset -i vl=0
        while (( vl < 3 )); do
            vidx=$(( (vbr+vk)*9 + vbc + vl ))
            if [[ "${grid[$vidx]}" == "$vv" ]]; then return 1; fi
            (( vl++ ))
        done
        (( vk++ ))
    done
    
    return 0
}

solve_iterative() {
    typeset -a empty_cells
    typeset -i i=0 ec_ptr=0
    while (( i < 81 )); do
        if [[ "${grid[$i]}" == "0" ]]; then
            empty_cells[ec_ptr]=$i
            (( ec_ptr++ ))
        fi
        (( i++ ))
    done

    typeset -i num_empty=${#empty_cells[@]}
    if (( num_empty == 0 )); then return 0; fi

    typeset -a tried_vals
    i=0
    while (( i < num_empty )); do
        tried_vals[i]=0
        (( i++ ))
    done
    
    typeset -i sptr=0
    while (( sptr >= 0 )); do
        typeset -i idx=${empty_cells[$sptr]}
        typeset -i last_val=${tried_vals[$sptr]}
        
        typeset -i row=$(( idx / 9 ))
        typeset -i col=$(( idx % 9 ))
        
        typeset -i found_valid=0
        typeset -i v=$(( last_val + 1 ))
        while (( v <= 9 )); do
            (( iterations++ ))
            if is_valid $row $col $v; then
                grid[$idx]=$v
                tried_vals[$sptr]=$v
                found_valid=1
                break
            fi
            (( v++ ))
        done

        if (( found_valid == 1 )); then
            if (( sptr == num_empty - 1 )); then
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
# Properly populate array in Ksh93
i=0
while (( i < 81 )); do
    grid[i]=${raw_data:i:1}
    (( i++ ))
done

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
