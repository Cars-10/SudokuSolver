#!/opt/homebrew/bin/bash
# DLX (Dancing Links) Algorithm X Implementation in BASH
# Implements Knuth's Algorithm X for exact cover problems using Dancing Links
# Applied to Sudoku solving with 4 constraints: position, row, column, box
#
# NOTE: BASH is extremely slow for this algorithm due to associative array overhead.
# Matrix 1 may take several minutes. This implementation prioritizes correctness.

# Global iteration counter
dlx_iterations=0

# Node structure simulation using associative arrays indexed as "array_idx"
declare -A LEFT RIGHT UP DOWN COL ROW_ID
declare -A SIZE NAME

# Node count and allocation
MAX_NODES=5000
NODE_COUNT=0

# Root is node 0, columns are nodes 1-324
ROOT=0

# Row metadata (sparse - only used rows)
declare -A INFO_ROW INFO_COL INFO_NUM ROW_START

# Puzzle (1D array, 81 elements)
declare -a PUZZLE SOLUTION

# Initialize a node as a circular list pointing to itself
init_node() {
    local n=$1
    LEFT[$n]=$n
    RIGHT[$n]=$n
    UP[$n]=$n
    DOWN[$n]=$n
    COL[$n]=$n
    ROW_ID[$n]=-1
}

# Initialize DLX matrix structure
init_dlx() {
    # Root node
    init_node $ROOT
    NAME[$ROOT]="root"
    SIZE[$ROOT]=0
    NODE_COUNT=1

    # Create 324 column headers
    local i
    for ((i=1; i<=324; i++)); do
        init_node $i
        NAME[$i]="C$((i-1))"
        SIZE[$i]=0

        # Insert at end of header list
        local prev=${LEFT[$ROOT]}
        RIGHT[$prev]=$i
        LEFT[$i]=$prev
        RIGHT[$i]=$ROOT
        LEFT[$ROOT]=$i
    done

    NODE_COUNT=325
}

# Global variable for returning node index (avoids subshell issues)
LAST_NODE=0

# Add node to column - sets LAST_NODE to the new node index
add_node() {
    local col=$1 rid=$2

    if ((NODE_COUNT >= MAX_NODES)); then
        echo "ERROR: Node limit exceeded" >&2
        exit 1
    fi

    local n=$NODE_COUNT
    ((NODE_COUNT++))

    COL[$n]=$col
    ROW_ID[$n]=$rid

    # Insert vertically at end of column
    local u=${UP[$col]}
    DOWN[$u]=$n
    UP[$n]=$u
    DOWN[$n]=$col
    UP[$col]=$n
    ((SIZE[$col]++))

    LAST_NODE=$n
}

# Constraint column calculations
pos_col() { echo $(($1 * 9 + $2 + 1)); }
row_col() { echo $((81 + $1 * 9 + $2)); }
col_col() { echo $((162 + $1 * 9 + $2)); }
box_col() { echo $((243 + (($1/3)*3 + $2/3) * 9 + $3)); }

# Build DLX row for cell (r,c) with digit d
build_row() {
    local r=$1 c=$2 d=$3 rid=$4

    INFO_ROW[$rid]=$r
    INFO_COL[$rid]=$c
    INFO_NUM[$rid]=$d

    # Create 4 nodes for 4 constraints (using LAST_NODE to avoid subshell issues)
    add_node $((r * 9 + c + 1)) $rid; local n1=$LAST_NODE
    add_node $((81 + r * 9 + d)) $rid; local n2=$LAST_NODE
    add_node $((162 + c * 9 + d)) $rid; local n3=$LAST_NODE
    add_node $((243 + (r/3*3 + c/3) * 9 + d)) $rid; local n4=$LAST_NODE

    # Link horizontally
    RIGHT[$n1]=$n2
    RIGHT[$n2]=$n3
    RIGHT[$n3]=$n4
    RIGHT[$n4]=$n1

    LEFT[$n1]=$n4
    LEFT[$n2]=$n1
    LEFT[$n3]=$n2
    LEFT[$n4]=$n3

    ROW_START[$rid]=$n1
}

# Build full matrix from puzzle
build_matrix() {
    local rid=0 r c n idx

    for ((r=0; r<9; r++)); do
        for ((c=0; c<9; c++)); do
            idx=$((r*9+c))
            local val=${PUZZLE[$idx]}

            if ((val != 0)); then
                # Clue: single row
                build_row $r $c $val $rid
                ((rid++))
            else
                # Empty: all 9 possibilities
                for ((n=1; n<=9; n++)); do
                    build_row $r $c $n $rid
                    ((rid++))
                done
            fi
        done
    done
}

# Cover column
cover() {
    local c=$1

    # Debug: check c is valid
    [[ -z "$c" ]] && { echo "ERROR: cover() called with empty c" >&2; return 1; }

    # Remove column from header
    local left_c=${LEFT[$c]:-}
    local right_c=${RIGHT[$c]:-}
    [[ -z "$left_c" || -z "$right_c" ]] && { echo "ERROR: LEFT[$c]=$left_c RIGHT[$c]=$right_c" >&2; return 1; }
    RIGHT[$left_c]=$right_c
    LEFT[$right_c]=$left_c

    # For each row in column
    local i=${DOWN[$c]}
    while ((i != c)); do
        # For each node in row
        local j=${RIGHT[$i]}
        while ((j != i)); do
            # Remove from column
            local up_j=${UP[$j]}
            local down_j=${DOWN[$j]}
            DOWN[$up_j]=$down_j
            UP[$down_j]=$up_j
            local col_j=${COL[$j]}
            ((SIZE[$col_j]--))
            j=${RIGHT[$j]}
        done
        i=${DOWN[$i]}
    done
}

# Uncover column (reverse of cover)
uncover() {
    local c=$1

    # For each row in column (reverse order)
    local i=${UP[$c]}
    while ((i != c)); do
        # For each node in row (reverse order)
        local j=${LEFT[$i]}
        while ((j != i)); do
            # Restore to column
            local col_j=${COL[$j]}
            ((SIZE[$col_j]++))
            local up_j=${UP[$j]}
            local down_j=${DOWN[$j]}
            DOWN[$up_j]=$j
            UP[$down_j]=$j
            j=${LEFT[$j]}
        done
        i=${UP[$i]}
    done

    # Restore column to header
    local left_c=${LEFT[$c]}
    local right_c=${RIGHT[$c]}
    RIGHT[$left_c]=$c
    LEFT[$right_c]=$c
}

# Choose column with minimum size
choose_col() {
    local min=999999 best=-1
    local j=${RIGHT[$ROOT]}

    while ((j != ROOT)); do
        if ((SIZE[$j] < min)); then
            min=${SIZE[$j]}
            best=$j
        fi
        j=${RIGHT[$j]}
    done

    echo $best
}

# DLX search
declare -a SOL
search() {
    local k=$1
    ((dlx_iterations++))

    # Success: matrix empty
    ((RIGHT[$ROOT] == ROOT)) && return 0

    # Choose column
    local c=$(choose_col)
    ((c < 0 || SIZE[$c] == 0)) && return 1

    cover $c

    # Try each row in column
    local r=${DOWN[$c]}
    while ((r != c)); do
        SOL[$k]=${ROW_ID[$r]}

        # Cover row
        local j=${RIGHT[$r]}
        while ((j != r)); do
            cover ${COL[$j]}
            j=${RIGHT[$j]}
        done

        # Recurse
        if search $((k+1)); then
            return 0
        fi

        # Uncover row
        j=${LEFT[$r]}
        while ((j != r)); do
            uncover ${COL[$j]}
            j=${LEFT[$j]}
        done

        r=${DOWN[$r]}
    done

    uncover $c
    return 1
}

# Cover clues
cover_clues() {
    local r c idx val rid

    for ((r=0; r<9; r++)); do
        for ((c=0; c<9; c++)); do
            idx=$((r*9+c))
            val=${PUZZLE[$idx]}

            if ((val != 0)); then
                # Find row for this clue
                for rid in "${!ROW_START[@]}"; do
                    if [[ ${INFO_ROW[$rid]} -eq $r && ${INFO_COL[$rid]} -eq $c && ${INFO_NUM[$rid]} -eq $val ]]; then
                        # Cover all columns in row
                        local n=${ROW_START[$rid]:-}
                        [[ -z "$n" ]] && { echo "ERROR: ROW_START[$rid] is empty" >&2; continue; }

                        local curr=$n
                        while true; do
                            local col_curr=${COL[$curr]:-}
                            [[ -z "$col_curr" ]] && { echo "ERROR: COL[$curr] is empty at rid=$rid" >&2; break; }
                            cover $col_curr || break
                            curr=${RIGHT[$curr]:-}
                            [[ -z "$curr" ]] && { echo "ERROR: RIGHT is empty" >&2; break; }
                            ((curr == n)) && break
                        done
                        break
                    fi
                done
            fi
        done
    done
}

# Extract solution
extract_sol() {
    local i rid r c n idx

    # Start with puzzle
    for ((i=0; i<81; i++)); do
        SOLUTION[$i]=${PUZZLE[$i]}
    done

    # Apply solution
    for rid in "${SOL[@]}"; do
        [[ -z "$rid" || $rid -lt 0 ]] && continue
        r=${INFO_ROW[$rid]}
        c=${INFO_COL[$rid]}
        n=${INFO_NUM[$rid]}
        idx=$((r*9+c))
        SOLUTION[$idx]=$n
    done
}

# Print grid
print_grid() {
    local -n arr=$1
    echo ""
    echo "Puzzle:"
    local r c idx
    for ((r=0; r<9; r++)); do
        for ((c=0; c<9; c++)); do
            idx=$((r*9+c))
            echo -n "${arr[$idx]} "
        done
        echo ""
    done
}

# Read matrix file
read_matrix() {
    local file=$1

    [[ ! -f "$file" ]] && { echo "Error: File not found: $file" >&2; return 1; }

    # Normalize path
    local path="$file"
    [[ "$file" == /app/Matrices/* ]] && path="../${file#/app/}"
    echo "$path"

    local lc=0
    while IFS= read -r line; do
        [[ "$line" =~ ^#.*$ || -z "$line" ]] && continue

        local -a vals
        read -ra vals <<< "$line"

        if [[ ${#vals[@]} -eq 9 ]]; then
            echo "${vals[*]}"
            local i
            for ((i=0; i<9; i++)); do
                PUZZLE[$((lc*9+i))]=${vals[$i]}
            done
            ((lc++))
            ((lc >= 9)) && break
        fi
    done < <(cat "$file"; echo "")

    ((lc == 9)) || { echo "Error: Need 9 rows" >&2; return 1; }
    return 0
}

# Main
main() {
    [[ $# -ne 1 ]] && { echo "Usage: $0 <matrix_file>" >&2; exit 1; }

    local t1=$(date +%s.%N)

    # Read puzzle
    read_matrix "$1" || exit 1
    print_grid PUZZLE

    # Build and solve
    init_dlx
    build_matrix
    cover_clues

    dlx_iterations=0
    if search 0; then
        extract_sol
        print_grid SOLUTION
        echo ""
        echo "Solved in Iterations=$dlx_iterations"
        echo ""
    else
        echo ""
        echo "No solution found"
    fi

    local t2=$(date +%s.%N)
    printf "Seconds to process %.3f\n" "$(echo "$t2 - $t1" | bc)"
}

main "$@"
