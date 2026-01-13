#!/bin/tcsh -f

# Sudoku Solver in Tcsh
# Iterative brute-force backtracking algorithm
# This matches the C reference algorithm fingerprint (iteration count)
# while avoiding the overhead of shell recursion/sourcing.

# Process each .matrix file from command line
set start_time = `python3 -c 'import time; print(time.time())'`

foreach file ($argv)
    # Check if it's a matrix file
    if ( "$file:e" != "matrix" ) continue
    
    # Normalized path for output
    if ( "$file" =~ */Matrices/* ) then
        set display_path = `echo $file | sed 's/.*\/Matrices\//..\/Matrices\//'`
        echo "$display_path"
    else
        echo "$file"
    endif
    
    # Read matrix file
    set puzzle = ()
    set lines = `grep -v '^#' "$file"`
    foreach val ($lines)
        set puzzle = ($puzzle $val)
    end
    
    # Print raw puzzle as read
    set r = 1
    while ($r <= 9)
        set c = 1
        while ($c <= 9)
            @ idx = ($r - 1) * 9 + $c
            printf "%d " $puzzle[$idx]
            @ c++
        end
        echo ""
        @ r++
    end

    # Print Puzzle header
    echo ""
    echo "Puzzle:"
    set r = 1
    while ($r <= 9)
        set c = 1
        while ($c <= 9)
            @ idx = ($r - 1) * 9 + $c
            printf "%d " $puzzle[$idx]
            @ c++
        end
        echo ""
        @ r++
    end

    # --- Solver Initialization ---
    set count = 0
    
    # Find all empty cells (indices)
    set empty_indices = ()
    set i = 1
    while ($i <= 81)
        if ($puzzle[$i] == 0) then
            set empty_indices = ($empty_indices $i)
        endif
        @ i++
    end
    
    set num_empty = $#empty_indices
    
    # Manual stack for iterative backtracking
    # current_val[k] stores the last value tried at empty_indices[k]
    set current_val = ()
    set k = 1
    while ($k <= $num_empty)
        set current_val = ($current_val 0)
        @ k++
    end

    set k = 1
    while ($k > 0 && $k <= $num_empty)
        @ idx = $empty_indices[$k]
        @ row = ($idx - 1) / 9 + 1
        @ col = ($idx - 1) % 9 + 1
        
        set found = 0
        @ val = $current_val[$k] + 1
        
        while ($val <= 9)
            @ count++
            
            # isValid check
            set is_v = 1
            
            # Check row
            set i = 1
            while ($i <= 9)
                @ check_idx = ($row - 1) * 9 + $i
                if ($puzzle[$check_idx] == $val) then
                    set is_v = 0
                    goto check_done
                endif
                @ i++
            end
            
            # Check column
            set i = 1
            while ($i <= 9)
                @ check_idx = ($i - 1) * 9 + $col
                if ($puzzle[$check_idx] == $val) then
                    set is_v = 0
                    goto check_done
                endif
                @ i++
            end
            
            # Check 3x3 box
            @ box_r = (($row - 1) / 3) * 3
            @ box_c = (($col - 1) / 3) * 3
            set i = 1
            while ($i <= 3)
                set j = 1
                while ($j <= 3)
                    @ check_idx = ($box_r + $i - 1) * 9 + ($box_c + $j)
                    if ($puzzle[$check_idx] == $val) then
                        set is_v = 0
                        goto check_done
                    endif
                    @ j++
                end
                @ i++
            end
            
            check_done:
            if ($is_v) then
                set puzzle[$idx] = $val
                set current_val[$k] = $val
                set found = 1
                break
            endif
            
            @ val++
        end
        
        if ($found) then
            @ k++
        else
            set puzzle[$idx] = 0
            set current_val[$k] = 0
            @ k--
        endif
    end

    # Print solved puzzle
    echo ""
    echo "Puzzle:"
    set r = 1
    while ($r <= 9)
        set c = 1
        while ($c <= 9)
            @ idx = ($r - 1) * 9 + $c
            printf "%d " $puzzle[$idx]
            @ c++
        end
        echo ""
        @ r++
    end
    echo ""
    echo "Solved in Iterations=$count"
    echo ""
    
end

set end_time = `python3 -c 'import time; print(time.time())'`
set duration = `python3 -c "print('%.3f' % ($end_time - $start_time))"`
echo "Seconds to process $duration"

exit 0
