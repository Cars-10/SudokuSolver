#!/opt/homebrew/bin/fish
# Sudoku Solver in Fish
# Fixed iterative backtracking with corrected math

set -g iterations 0
set -g grid

function print_grid
    for r in (seq 0 8)
        set -l line ""
        for c in (seq 1 9)
            set -l idx (math "$r * 9 + $c")
            set line "$line$grid[$idx] "
        end
        echo "$line"
    end
end

function is_valid -a row col val
    # Row check (row is 1..9)
    set -l row_start (math "($row - 1) * 9 + 1")
    for i in (seq $row_start (math "$row_start + 8"))
        if test "$grid[$i]" = "$val"
            return 1
        end
    end
    # Col check (col is 1..9)
    for i in (seq 0 8)
        set -l idx (math "$i * 9 + $col")
        if test "$grid[$idx]" = "$val"
            return 1
        end
    end
    # Box check
    set -l br (math "floor(($row - 1) / 3) * 3 + 1")
    set -l bc (math "floor(($col - 1) / 3) * 3 + 1")
    for i in (seq 0 2)
        set -l r_offset (math "($br + $i - 1) * 9")
        for j in (seq 0 2)
            set -l idx (math "$r_offset + $bc + $j")
            if test "$grid[$idx]" = "$val"
                return 1
            end
        end
    end
    return 0
end

function solve_iterative
    set -l empty_cells
    for i in (seq 1 81)
        if test "$grid[$i]" = 0
            set -a empty_cells $i
        end
    end

    set -l num_empty (count $empty_cells)
    if test "$num_empty" = 0
        return 0
    end

    set -l tried_vals (for i in (seq 1 $num_empty); echo 0; end)
    set -l sptr 1
    
    while test "$sptr" -gt 0
        set -l idx $empty_cells[$sptr]
        set -l last_val $tried_vals[$sptr]
        
        set -l r (math "floor(($idx - 1) / 9) + 1")
        set -l c (math "(($idx - 1) % 9) + 1")
        
        set -l found_valid false
        set -l v (math "$last_val + 1")
        
        while test "$v" -le 9
            set -g iterations (math "$iterations + 1")
            if is_valid $r $c $v
                set -g grid[$idx] $v
                set tried_vals[$sptr] $v
                set found_valid true
                break
            end
            set v (math "$v + 1")
        end

        if test "$found_valid" = true
            if test "$sptr" = "$num_empty"
                return 0 # Solved
            end
            set sptr (math "$sptr + 1")
        else
            set -g grid[$idx] 0
            set tried_vals[$sptr] 0
            set sptr (math "$sptr - 1")
        end
    end
    return 1
end

function main
    set -l filename $argv[1]
    if test -z "$filename"
        echo "Usage: Sudoku.fish <matrix_file>"
        exit 1
    end
    
    echo "$filename"
    echo ""
    
    set -l raw_data (grep -v '^#' "$filename" | tr -cd '0-9')
    set -g grid (string split "" "$raw_data")
    
    echo "Puzzle:"
    print_grid
    echo ""
    
    set -g iterations 0
    if solve_iterative
        echo "Puzzle:"
        print_grid
        echo ""
        echo "Solved in Iterations=$iterations"
    else
        echo "No solution found."
    end
end

main $argv