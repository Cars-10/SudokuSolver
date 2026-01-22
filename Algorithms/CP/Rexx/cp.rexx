#!/usr/bin/env regina
/* Constraint Propagation (CP) Sudoku Solver - Rexx Implementation */
/* Algorithm: Constraint propagation with MRV heuristic */

/* Global iteration counter */
cp_iterations = 0

/* Grid arrays (1-based indexing) */
/* values.r.c = assigned value (0 = empty) */
/* candidates.r.c = possible values (bitset) */

/* Parse command line */
parse arg filename
if filename = '' then do
    say 'Usage: rexx cp.rexx <matrix_file>'
    exit 1
end

/* Read puzzle from file */
call read_puzzle filename

/* Initialize grid */
call init_grid

/* Print initial puzzle */
call print_grid 'Initial'

/* Solve */
call time 'R'
solved = cp_search()
elapsed = time('E')

/* Print solution */
call print_grid 'Solution'

if solved then do
    say ''
    say 'Solved in Iterations='cp_iterations
end
else do
    say ''
    say 'No solution found'
end

say ''
say 'Seconds to process' format(elapsed, , 3)
exit 0

/* ================================================================
 * Bitset operations
 * ================================================================ */
has_candidate:
    parse arg candidate_set, digit
    return (candidate_set // (2 ** (digit + 1))) >= (2 ** digit)

add_candidate:
    parse arg candidate_set, digit
    return candidate_set + (2 ** digit)

remove_candidate:
    parse arg candidate_set, digit
    if \has_candidate(candidate_set, digit) then
        return candidate_set
    return candidate_set - (2 ** digit)

count_candidates:
    parse arg candidate_set
    count = 0
    do digit = 1 to 9
        if has_candidate(candidate_set, digit) then
            count = count + 1
    end
    return count

get_first_candidate:
    parse arg candidate_set
    do digit = 1 to 9
        if has_candidate(candidate_set, digit) then
            return digit
    end
    return 0

/* ================================================================
 * Procedures
 * ================================================================ */

/* Read puzzle from file */
read_puzzle: procedure expose puzzle.
    parse arg fname
    
    if stream(fname, 'C', 'QUERY EXISTS') = '' then do
        say 'Error: File not found:' fname
        exit 1
    end
    
    row = 1
    do while lines(fname) > 0 & row <= 9
        line = linein(fname)
        col = 1
        do i = 1 to length(line)
            ch = substr(line, i, 1)
            if datatype(ch, 'W') & ch >= '0' & ch <= '9' then do
                puzzle.row.col = ch
                col = col + 1
                if col > 9 then leave
            end
        end
        if col > 9 then
            row = row + 1
    end
    call stream fname, 'C', 'CLOSE'
    return

/* Initialize grid from puzzle */
init_grid: procedure expose puzzle. values. candidates.
    do row = 1 to 9
        do col = 1 to 9
            if puzzle.row.col = 0 then do
                values.row.col = 0
                candidates.row.col = 1022  /* bits 1-9 set (2^1 + 2^2 + ... + 2^9) */
            end
            else do
                digit = puzzle.row.col
                values.row.col = digit
                candidates.row.col = 2 ** digit
            end
        end
    end
    return

/* Eliminate digit from candidates at (row, col) */
eliminate: procedure expose values. candidates. cp_iterations
    parse arg row, col, digit
    
    /* Check if already eliminated */
    if \has_candidate(candidates.row.col, digit) then
        return 1
    
    /* Remove digit */
    candidates.row.col = remove_candidate(candidates.row.col, digit)
    
    /* Check for contradiction */
    remaining = count_candidates(candidates.row.col)
    if remaining = 0 then
        return 0
    
    /* Singleton elimination */
    if remaining = 1 & values.row.col = 0 then do
        last_digit = get_first_candidate(candidates.row.col)
        if \assign_value(row, col, last_digit) then
            return 0
    end
    
    return 1

/* Assign digit to cell at (row, col) */
assign_value: procedure expose values. candidates. cp_iterations
    parse arg row, col, digit
    
    cp_iterations = cp_iterations + 1
    
    values.row.col = digit
    candidates.row.col = 2 ** digit
    
    /* Eliminate from row */
    do c = 1 to 9
        if c \= col then do
            if \eliminate(row, c, digit) then
                return 0
        end
    end
    
    /* Eliminate from column */
    do r = 1 to 9
        if r \= row then do
            if \eliminate(r, col, digit) then
                return 0
        end
    end
    
    /* Eliminate from box */
    box_row = ((row - 1) % 3) * 3 + 1
    box_col = ((col - 1) % 3) * 3 + 1
    do r = box_row to box_row + 2
        do c = box_col to box_col + 2
            if r \= row & c \= col then do
                if \eliminate(r, c, digit) then
                    return 0
            end
        end
    end
    
    return 1

/* Apply constraint propagation */
propagate: procedure expose values. candidates. cp_iterations
    changed = 1
    
    do while changed
        changed = 0
        
        /* Singleton elimination */
        do row = 1 to 9
            do col = 1 to 9
                if values.row.col = 0 then do
                    num_candidates = count_candidates(candidates.row.col)
                    if num_candidates = 0 then
                        return 0
                    if num_candidates = 1 then do
                        digit = get_first_candidate(candidates.row.col)
                        if \assign_value(row, col, digit) then
                            return 0
                        changed = 1
                    end
                end
            end
        end
        
        /* Hidden singles - rows */
        do row = 1 to 9
            do digit = 1 to 9
                count = 0
                last_col = 0
                already_assigned = 0
                
                do col = 1 to 9
                    if values.row.col = digit then do
                        already_assigned = 1
                        leave
                    end
                    if has_candidate(candidates.row.col, digit) then do
                        count = count + 1
                        last_col = col
                    end
                end
                
                if already_assigned then
                    iterate
                
                if count = 1 then do
                    if \assign_value(row, last_col, digit) then
                        return 0
                    changed = 1
                end
                else if count = 0 then
                    return 0
            end
        end
        
        /* Hidden singles - columns */
        do col = 1 to 9
            do digit = 1 to 9
                count = 0
                last_row = 0
                already_assigned = 0
                
                do row = 1 to 9
                    if values.row.col = digit then do
                        already_assigned = 1
                        leave
                    end
                    if has_candidate(candidates.row.col, digit) then do
                        count = count + 1
                        last_row = row
                    end
                end
                
                if already_assigned then
                    iterate
                
                if count = 1 then do
                    if \assign_value(last_row, col, digit) then
                        return 0
                    changed = 1
                end
                else if count = 0 then
                    return 0
            end
        end
        
        /* Hidden singles - boxes */
        do box = 0 to 8
            box_row = (box % 3) * 3 + 1
            box_col = (box // 3) * 3 + 1
            
            do digit = 1 to 9
                count = 0
                last_r = 0
                last_c = 0
                already_assigned = 0
                
                do r = box_row to box_row + 2
                    do c = box_col to box_col + 2
                        if values.r.c = digit then do
                            already_assigned = 1
                            leave r
                        end
                        if has_candidate(candidates.r.c, digit) then do
                            count = count + 1
                            last_r = r
                            last_c = c
                        end
                    end
                end
                
                if already_assigned then
                    iterate
                
                if count = 1 then do
                    if \assign_value(last_r, last_c, digit) then
                        return 0
                    changed = 1
                end
                else if count = 0 then
                    return 0
            end
        end
    end
    
    return 1

/* Check if grid is complete */
is_complete: procedure expose values.
    do row = 1 to 9
        do col = 1 to 9
            if values.row.col = 0 then
                return 0
        end
    end
    return 1

/* Find cell with minimum remaining values (MRV) */
find_mrv_cell: procedure expose values. candidates.
    min_count = 10
    min_row = 0
    min_col = 0
    
    do row = 1 to 9
        do col = 1 to 9
            if values.row.col = 0 then do
                count = count_candidates(candidates.row.col)
                if count < min_count then do
                    min_count = count
                    min_row = row
                    min_col = col
                end
            end
        end
    end
    
    return min_row min_col

/* Save grid state */
save_state: procedure expose values. candidates. saved_values. saved_candidates.
    do row = 1 to 9
        do col = 1 to 9
            saved_values.row.col = values.row.col
            saved_candidates.row.col = candidates.row.col
        end
    end
    return

/* Restore grid state */
restore_state: procedure expose values. candidates. saved_values. saved_candidates.
    do row = 1 to 9
        do col = 1 to 9
            values.row.col = saved_values.row.col
            candidates.row.col = saved_candidates.row.col
        end
    end
    return

/* Solve with search */
cp_search: procedure expose values. candidates. saved_values. saved_candidates. cp_iterations
    if \propagate() then
        return 0
    
    if is_complete() then
        return 1
    
    parse value find_mrv_cell() with row col
    
    if row = 0 then
        return 0
    
    candidates_set = candidates.row.col
    
    do digit = 1 to 9
        if has_candidate(candidates_set, digit) then do
            call save_state
            
            if assign_value(row, col, digit) then do
                if cp_search() then
                    return 1
            end
            
            call restore_state
        end
    end
    
    return 0

/* Print grid */
print_grid: procedure expose values. puzzle.
    parse arg label
    
    say ''
    say 'Puzzle:'
    do row = 1 to 9
        line = ''
        do col = 1 to 9
            line = line || values.row.col || ' '
        end
        say line
    end
    return