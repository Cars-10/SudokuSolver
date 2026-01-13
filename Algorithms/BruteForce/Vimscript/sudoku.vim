" Sudoku Solver in Vimscript
" Exact match of C brute-force algorithm

let s:board = []
let s:iterations = 0

function! ReadMatrix(filename)
    let s:board = []
    let lines = readfile(a:filename)
    let row = 0
    for line in lines
        if line =~ '^#' || len(line) == 0
            continue
        endif
        if row >= 9
            break
        endif
        
        let col = 0
        let board_row = []
        let parts = split(line, '\s\+')
        for part in parts
            if col < 9
                call add(board_row, str2nr(part))
                let col += 1
            endif
        endfor
        if len(board_row) == 9
            call add(s:board, board_row)
            let row += 1
        endif
    endfor
endfunction

function! GetBoardString()
    let res = ""
    for row in s:board
        for cell in row
            let res .= cell . " "
        endfor
        let res .= "\n"
    endfor
    return res
endfunction

function! IsValid(row, col, num)
    " Row check
    for c in range(9)
        if s:board[a:row][c] == a:num
            return 0
        endif
    endfor

    " Col check
    for r in range(9)
        if s:board[r][a:col] == a:num
            return 0
        endif
    endfor

    " Box check
    let boxRow = (a:row / 3) * 3
    let boxCol = (a:col / 3) * 3

    for r in range(3)
        for c in range(3)
            if s:board[boxRow + r][boxCol + c] == a:num
                return 0
            endif
        endfor
    endfor

    return 1
endfunction

function! Solve()
    let row = -1
    let col = -1
    let found = 0

    " Find first empty cell (row-major)
    for r in range(9)
        for c in range(9)
            if s:board[r][c] == 0
                let row = r
                let col = c
                let found = 1
                break
            endif
        endfor
        if found | break | endif
    endfor

    if !found
        return 1
    endif

    for num in range(1, 9)
        let s:iterations += 1
        if IsValid(row, col, num)
            let s:board[row][col] = num
            if Solve()
                return 1
            endif
            let s:board[row][col] = 0
        endif
    endfor

    return 0
endfunction

function! Main()
    " Use v:argv to get arguments passed after --
    let args = []
    let found_dash_dash = 0
    for arg in v:argv
        if found_dash_dash
            call add(args, arg)
        elseif arg == '--'
            let found_dash_dash = 1
        endif
    endfor

    if len(args) < 1
        qall!
    endif

    let filename = args[0]
    call ReadMatrix(filename)

    let output = []
    call add(output, filename)
    call add(output, "")
    
    let initial_board = split(GetBoardString(), "\n")
    call add(output, "Puzzle:")
    for line in initial_board
        call add(output, line)
    endfor

    let s:iterations = 0
    if Solve()
        call add(output, "")
        call add(output, "Puzzle:")
        let solved_board = split(GetBoardString(), "\n")
        for line in solved_board
            call add(output, line)
        endfor
        call add(output, "")
        call add(output, "Solved in Iterations=".s:iterations)
    else
        call add(output, "No solution found.")
    endif
    
    call writefile(output, "/dev/stdout")
    qall!
endfunction

call Main()