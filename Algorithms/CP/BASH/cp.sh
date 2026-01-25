#!/opt/homebrew/bin/bash
# Constraint Propagation Sudoku Solver in BASH
# Implements constraint propagation with bitsets and MRV heuristic
#
# NOTE: BASH is slow for bitwise operations but manageable for CP
# Matrix 1 should complete in reasonable time

# Global iteration counter
cp_iterations=0

# Grid state: values (0=empty) and candidates (bitsets)
declare -a VALUES CANDIDATES

# Bitset operations (bits 1-9 represent digits 1-9)
# Full candidate set: 0x3FE = 1111111110 in binary (bits 1-9 set)
FULL_CANDIDATES=1022  # 0x3FE

# Bitset helper functions
has_bit() { (( ($1 & (1 << $2)) != 0 )); }
set_bit() { echo $(($1 | (1 << $2))); }
clear_bit() { echo $(($1 & ~(1 << $2))); }

count_bits() {
    local n=$1 count=0
    for ((i=1; i<=9; i++)); do
        ((n & (1 << i))) && ((count++))
    done
    echo $count
}

get_first_bit() {
    local n=$1
    for ((i=1; i<=9; i++)); do
        ((n & (1 << i))) && { echo $i; return; }
    done
    echo 0
}

# Get all peer cells for (row, col)
# Returns 20 peers as space-separated "r,c" pairs
get_peers() {
    local row=$1 col=$2
    local peers=()

    # Same row (8 cells)
    local c
    for ((c=0; c<9; c++)); do
        ((c != col)) && peers+=("$row,$c")
    done

    # Same column (8 cells)
    local r
    for ((r=0; r<9; r++)); do
        ((r != row)) && peers+=("$r,$col")
    done

    # Same box (4 cells not already counted)
    local br=$(( (row/3)*3 )) bc=$(( (col/3)*3 ))
    for ((r=br; r<br+3; r++)); do
        for ((c=bc; c<bc+3; c++)); do
            ((r != row && c != col)) && peers+=("$r,$c")
        done
    done

    echo "${peers[@]}"
}

# Eliminate digit from cell
# Returns 0 on contradiction, 1 on success
eliminate() {
    local row=$1 col=$2 digit=$3
    local idx=$(( row*9 + col ))

    # Already eliminated?
    has_bit ${CANDIDATES[$idx]} $digit || return 0

    # Remove digit
    CANDIDATES[$idx]=$(clear_bit ${CANDIDATES[$idx]} $digit)

    # Check for contradiction
    local count=$(count_bits ${CANDIDATES[$idx]})
    ((count == 0)) && return 1

    # Singleton elimination: if only one candidate left and cell is empty, assign it
    if ((count == 1 && VALUES[$idx] == 0)); then
        local last=$(get_first_bit ${CANDIDATES[$idx]})
        assign $row $col $last || return 1
    fi

    return 0
}

# Assign digit to cell and propagate
# Returns 0 on contradiction, 1 on success
assign() {
    local row=$1 col=$2 digit=$3
    local idx=$(( row*9 + col ))

    # Increment iteration counter (this is our benchmark metric)
    ((cp_iterations++))

    # Set value
    VALUES[$idx]=$digit
    CANDIDATES[$idx]=$(( 1 << digit ))

    # Eliminate from all peers
    local peers=$(get_peers $row $col)
    local peer pr pc pidx

    for peer in $peers; do
        IFS=',' read -r pr pc <<< "$peer"
        eliminate $pr $pc $digit || return 1
    done

    return 0
}

# Propagate constraints until fixpoint
# Returns 0 on contradiction, 1 on success
propagate() {
    local changed=1

    while ((changed)); do
        changed=0

        # Strategy 1: Singleton elimination
        local r c idx
        for ((r=0; r<9; r++)); do
            for ((c=0; c<9; c++)); do
                idx=$((r*9+c))
                if ((VALUES[$idx] == 0)); then
                    local count=$(count_bits ${CANDIDATES[$idx]})
                    ((count == 0)) && return 1
                    if ((count == 1)); then
                        local digit=$(get_first_bit ${CANDIDATES[$idx]})
                        assign $r $c $digit || return 1
                        changed=1
                    fi
                fi
            done
        done

        # Strategy 2: Hidden singles in rows
        local digit
        for ((r=0; r<9; r++)); do
            for ((digit=1; digit<=9; digit++)); do
                local count=0 last_col=-1

                # Check if digit already placed in row
                local placed=0
                for ((c=0; c<9; c++)); do
                    idx=$((r*9+c))
                    if ((VALUES[$idx] == digit)); then
                        placed=1
                        break
                    fi
                done
                ((placed)) && continue

                # Count possible positions
                for ((c=0; c<9; c++)); do
                    idx=$((r*9+c))
                    if has_bit ${CANDIDATES[$idx]} $digit; then
                        ((count++))
                        last_col=$c
                    fi
                done

                ((count == 0)) && return 1
                if ((count == 1)); then
                    assign $r $last_col $digit || return 1
                    changed=1
                fi
            done
        done

        # Strategy 3: Hidden singles in columns
        local c
        for ((c=0; c<9; c++)); do
            for ((digit=1; digit<=9; digit++)); do
                local count=0 last_row=-1

                # Check if digit already placed in column
                local placed=0
                for ((r=0; r<9; r++)); do
                    idx=$((r*9+c))
                    if ((VALUES[$idx] == digit)); then
                        placed=1
                        break
                    fi
                done
                ((placed)) && continue

                # Count possible positions
                for ((r=0; r<9; r++)); do
                    idx=$((r*9+c))
                    if has_bit ${CANDIDATES[$idx]} $digit; then
                        ((count++))
                        last_row=$r
                    fi
                done

                ((count == 0)) && return 1
                if ((count == 1)); then
                    assign $last_row $c $digit || return 1
                    changed=1
                fi
            done
        done

        # Strategy 4: Hidden singles in boxes
        local box br bc
        for ((box=0; box<9; box++)); do
            br=$(( (box/3)*3 ))
            bc=$(( (box%3)*3 ))

            for ((digit=1; digit<=9; digit++)); do
                local count=0 last_r=-1 last_c=-1

                # Check if digit already placed in box
                local placed=0
                for ((r=br; r<br+3; r++)); do
                    for ((c=bc; c<bc+3; c++)); do
                        idx=$((r*9+c))
                        if ((VALUES[$idx] == digit)); then
                            placed=1
                            break 2
                        fi
                    done
                done
                ((placed)) && continue

                # Count possible positions
                for ((r=br; r<br+3; r++)); do
                    for ((c=bc; c<bc+3; c++)); do
                        idx=$((r*9+c))
                        if has_bit ${CANDIDATES[$idx]} $digit; then
                            ((count++))
                            last_r=$r
                            last_c=$c
                        fi
                    done
                done

                ((count == 0)) && return 1
                if ((count == 1)); then
                    assign $last_r $last_c $digit || return 1
                    changed=1
                fi
            done
        done
    done

    return 0
}

# Find cell with minimum remaining values (MRV heuristic)
# Returns "row,col" or empty string if grid is complete
find_mrv() {
    local min_count=10 best_r=-1 best_c=-1

    local r c idx
    for ((r=0; r<9; r++)); do
        for ((c=0; c<9; c++)); do
            idx=$((r*9+c))
            if ((VALUES[$idx] == 0)); then
                local count=$(count_bits ${CANDIDATES[$idx]})
                if ((count < min_count)); then
                    min_count=$count
                    best_r=$r
                    best_c=$c
                fi
            fi
        done
    done

    ((best_r >= 0)) && echo "$best_r,$best_c"
}

# Backtracking search
search() {
    # Find MRV cell
    local mrv=$(find_mrv)
    [[ -z "$mrv" ]] && return 0  # Grid complete

    # Parse coordinates
    local mrv_r mrv_c
    IFS=',' read -r mrv_r mrv_c <<< "$mrv"
    local idx=$(( mrv_r*9 + mrv_c ))

    # Try each candidate
    local cands=${CANDIDATES[$idx]}
    local digit

    for ((digit=1; digit<=9; digit++)); do
        if has_bit $cands $digit; then
            # Save state
            local saved_values=("${VALUES[@]}")
            local saved_candidates=("${CANDIDATES[@]}")

            # Try assignment
            if assign $mrv_r $mrv_c $digit && propagate && search; then
                return 0  # Success
            fi

            # Restore state
            VALUES=("${saved_values[@]}")
            CANDIDATES=("${saved_candidates[@]}")
        fi
    done

    return 1  # No solution
}

# Initialize grid from puzzle
init_grid() {
    local r c idx

    for ((r=0; r<9; r++)); do
        for ((c=0; c<9; c++)); do
            idx=$((r*9+c))
            if ((PUZZLE[$idx] == 0)); then
                # Empty cell: all candidates
                VALUES[$idx]=0
                CANDIDATES[$idx]=$FULL_CANDIDATES
            else
                # Clue: single candidate
                local digit=${PUZZLE[$idx]}
                VALUES[$idx]=$digit
                CANDIDATES[$idx]=$(( 1 << digit ))
            fi
        done
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
declare -a PUZZLE
main() {
    [[ $# -ne 1 ]] && { echo "Usage: $0 <matrix_file>" >&2; exit 1; }

    local t1=$(date +%s.%N)

    # Read puzzle
    read_matrix "$1" || exit 1
    print_grid PUZZLE

    # Initialize and propagate clues
    init_grid
    if ! propagate; then
        echo ""
        echo "No solution found (contradiction during initial propagation)"
        local t2=$(date +%s.%N)
        printf "Seconds to process %.3f\n" "$(echo "$t2 - $t1" | bc)"
        exit 0
    fi

    # Search
    cp_iterations=0
    if search; then
        print_grid VALUES
        echo ""
        echo "Solved in Iterations=$cp_iterations"
        echo ""
    else
        echo ""
        echo "No solution found"
    fi

    local t2=$(date +%s.%N)
    printf "Seconds to process %.3f\n" "$(echo "$t2 - $t1" | bc)"
}

main "$@"
