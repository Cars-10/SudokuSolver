" Sudoku Solver in Vimscript

let s:board = []
let s:iterations = 0

function! ReadMatrix(filename)
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
        for i in range(len(line))
            let char = line[i]
            if char =~ '\d'
                if col < 9
                    call add(board_row, str2nr(char))
                    let col += 1
                endif
            elseif char == '.'
                if col < 9
                    call add(board_row, 0)
                    let col += 1
                endif
            endif
        endfor
        call add(s:board, board_row)
        let row += 1
    endfor
endfunction

function! PrintBoard()
    for row in s:board
        let line = ""
        for cell in row
            let line .= cell . " "
        endfor
        echo line
    endfor
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

function! FindEmpty()
    for r in range(9)
        for c in range(9)
            if s:board[r][c] == 0
                return [r, c]
            endif
        endfor
    endfor
    return [-1, -1]
endfunction

function! Solve()
    let empty = FindEmpty()
    let row = empty[0]
    let col = empty[1]

    if row == -1
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
    let args = argv()
    if len(args) < 1
        echo "Usage: vim -S sudoku.vim -- <matrix_file>"
        qall!
    endif

    let filename = args[0]
    call ReadMatrix(filename)

    let output = []
    call add(output, "Puzzle:")
    
    " Capture board output
    for row in s:board
        let line = ""
        for cell in row
            let line .= cell . " "
        endfor
        call add(output, line)
    endfor

    if Solve()
        call add(output, "Puzzle:")
        for row in s:board
            let line = ""
            for cell in row
                let line .= cell . " "
            endfor
            call add(output, line)
        endfor
        call add(output, "Solved in Iterations=" . s:iterations)
    else
        call add(output, "No solution found.")
    endif
    
    call writefile(output, "out.txt")
    qall!
endfunction

call Main()
