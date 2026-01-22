#!/usr/bin/env regina
/* DLX Sudoku Solver in Regina Rexx */

/* Global iteration counter */
dlx_iterations = 0

/* Parse command line */
parse arg filename
if filename = '' then do
    say 'Usage: rexx dlx.rexx <matrix_file>'
    exit 1
end

/* Read puzzle */
call read_puzzle filename

/* Solve */
call init_dlx
call build_matrix

call time 'R'
/* Solution array */
do i = 0 to 80
    solution.i = -1
end

found = search(0)
elapsed = time('E')

if found then do
    /* Construct grid from solution */
    do r = 0 to 8
        do c = 0 to 8
            result_grid.r.c = puzzle.r.c
        end
    end
    
    do k = 0 to 80
        if solution.k \= -1 then do
            r_id = solution.k
            r_idx = row_info.r_id.row
            c_idx = row_info.r_id.col
            v_idx = row_info.r_id.val
            result_grid.r_idx.c_idx = v_idx
        end
    end
    
    call print_result_grid
    say ''
    say 'Solved in Iterations='dlx_iterations
end
else do
    say ''
    say 'No solution found'
end

say ''
say 'Seconds to process' format(elapsed, , 3)
exit 0

/* ================================================================
 * DLX Core
 * ================================================================ */

init_dlx: procedure expose L. R. U. D. C. S. RowID. head_idx node_count
    head_idx = 0
    node_count = 0
    
    /* Root */
    L.0 = 0
    R.0 = 0
    U.0 = 0
    D.0 = 0
    C.0 = 0
    RowID.0 = -1
    
    /* Columns 1-324 */
    do i = 1 to 324
        prev = i - 1
        L.i = prev
        R.i = 0
        R.prev = i
        
        U.i = i
        D.i = i
        C.i = i
        S.i = 0
        RowID.i = -1
        node_count = i
    end
    
    /* Circular link */
    L.0 = 324
    R.324 = 0
    
    return

add_node: procedure expose L. R. U. D. C. S. RowID. node_count
    parse arg col_idx, r_id
    
    node_count = node_count + 1
    new_node = node_count
    
    C.new_node = col_idx
    RowID.new_node = r_id
    
    /* Link vertical */
    col_node = col_idx
    up_node = U.col_node
    
    D.up_node = new_node
    U.new_node = up_node
    D.new_node = col_node
    U.col_node = new_node
    
    S.col_idx = S.col_idx + 1
    
    return new_node

cover: procedure expose L. R. U. D. C. S.
    parse arg c
    
    /* Unlink column header */
    lc = L.c
    rc = R.c
    R.lc = rc
    L.rc = lc
    
    /* Iterate down */
    i = D.c
    do while i \= c
        /* Iterate right */
        j = R.i
        do while j \= i
            dj = D.j
            uj = U.j
            U.dj = uj
            D.uj = dj
            
            col = C.j
            S.col = S.col - 1
            j = R.j
        end
        i = D.i
    end
    return

uncover: procedure expose L. R. U. D. C. S.
    parse arg c
    
    /* Iterate up */
    i = U.c
    do while i \= c
        /* Iterate left */
        j = L.i
        do while j \= i
            col = C.j
            S.col = S.col + 1
            
            dj = D.j
            uj = U.j
            U.dj = j
            D.uj = j
            
            j = L.j
        end
        i = U.i
    end
    
    /* Relink column header */
    lc = L.c
    rc = R.c
    R.lc = c
    L.rc = c
    return

search: procedure expose L. R. U. D. C. S. RowID. solution. dlx_iterations head_idx
    parse arg k
    
    dlx_iterations = dlx_iterations + 1
    
    if R.head_idx = head_idx then return 1
    
    /* Choose column (MRV) */
    c = R.head_idx
    min_size = S.c
    
    curr = R.c
    do while curr \= head_idx
        if S.curr < min_size then do
            min_size = S.curr
            c = curr
        end
        curr = R.curr
    end
    
    call cover c
    
    r = D.c
    do while r \= c
        solution.k = RowID.r
        
        j = R.r
        do while j \= r
            call cover C.j
            j = R.j
        end
        
        if search(k + 1) then return 1
        
        j = L.r
        do while j \= r
            call uncover C.j
            j = L.j
        end
        
        r = D.r
    end
    
    call uncover c
    return 0

/* ================================================================
 * Matrix Building
 * ================================================================ */

build_matrix: procedure expose L. R. U. D. C. S. RowID. node_count puzzle. row_info.
    r_id = 0
    
    do r = 0 to 8
        do c = 0 to 8
            val = puzzle.r.c
            if val \= 0 then do
                call build_row r, c, val, r_id
                r_id = r_id + 1
            end
            else do
                do v = 1 to 9
                    call build_row r, c, v, r_id
                    r_id = r_id + 1
                end
            end
        end
    end
    return

build_row: procedure expose L. R. U. D. C. S. RowID. node_count row_info.
    parse arg r, c, v, r_id
    
    row_info.r_id.row = r
    row_info.r_id.col = c
    row_info.r_id.val = v
    
    /* Constraints */
    c1 = r * 9 + c + 1
    c2 = 81 + r * 9 + (v - 1) + 1
    c3 = 162 + c * 9 + (v - 1) + 1
    box = (r % 3) * 3 + (c % 3)
    c4 = 243 + box * 9 + (v - 1) + 1
    
    n1 = add_node(c1, r_id)
    n2 = add_node(c2, r_id)
    n3 = add_node(c3, r_id)
    n4 = add_node(c4, r_id)
    
    /* Link row */
    L.n1 = n4; R.n1 = n2
    L.n2 = n1; R.n2 = n3
    L.n3 = n2; R.n3 = n4
    L.n4 = n3; R.n4 = n1
    
    return

/* ================================================================
 * I/O
 * ================================================================ */

read_puzzle: procedure expose puzzle.
    parse arg fname
    if stream(fname, 'C', 'QUERY EXISTS') = '' then do
        say 'Error: File not found:' fname
        exit 1
    end
    
    say fname
    
    row = 0
    do while lines(fname) > 0 & row < 9
        line = linein(fname)
        if left(strip(line), 1) = '#' then iterate
        if strip(line) = '' then iterate
        
        col = 0
        do i = 1 to length(line)
            ch = substr(line, i, 1)
            if datatype(ch, 'W') & ch >= '0' & ch <= '9' then do
                puzzle.row.col = ch
                col = col + 1
                if col >= 9 then leave
            end
        end
        
        out = ''
        do c = 0 to 8
            if c > 0 then out = out || ' '
            out = out || puzzle.row.c
        end
        say out
        
        row = row + 1
    end
    call stream fname, 'C', 'CLOSE'
    return

print_result_grid: procedure expose result_grid.
    say ''
    say 'Puzzle:'
    do r = 0 to 8
        line = ''
        do c = 0 to 8
            if c > 0 then line = line || ' '
            line = line || result_grid.r.c
        end
        say line
    end
    return
