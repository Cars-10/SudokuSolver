" Vimscript Sudoku Solver

let s:puzzle = []
let s:count = 0

function! s:PrintPuzzle()
    echo "\nPuzzle:"
    for r in range(9)
        let line = ""
        for c in range(9)
            let line .= s:puzzle[r][c] . " "
        endfor
        echo line
    endfor
endfunction

function! s:ReadMatrixFile(filename)
    echo a:filename
    let s:puzzle = []
    let lines = readfile(a:filename)
    let row = 0
    for line in lines
        if line =~ '^#' || line =~ '^\s*$'
            continue
        endif
        let parts = split(line)
        if len(parts) == 9
            call add(s:puzzle, map(parts, 'str2nr(v:val)'))
            let row += 1
            if row == 9
                break
            endif
        endif
    endfor
endfunction

function! s:IsPossible(r, c, val)
    for i in range(9)
        if s:puzzle[i][a:c] == a:val
            return 0
        endif
        if s:puzzle[a:r][i] == a:val
            return 0
        endif
    endfor
    
    let r0 = (a:r / 3) * 3
    let c0 = (a:c / 3) * 3
    
    for i in range(3)
        for j in range(3)
            if s:puzzle[r0 + i][c0 + j] == a:val
                return 0
            endif
        endfor
    endfor
    return 1
endfunction

function! s:Solve()
    for r in range(9)
        for c in range(9)
            if s:puzzle[r][c] == 0
                for val in range(1, 9)
                    let s:count += 1
                    if s:IsPossible(r, c, val)
                        let s:puzzle[r][c] = val
                        if s:Solve()
                            return 1
                        endif
                        let s:puzzle[r][c] = 0
                    endif
                endfor
                return 0
            endif
        endfor
    endfor
    call s:PrintPuzzle()
    echo "\nSolved in Iterations=" . s:count . "\n"
    return 1
endfunction

function! Main()
    let start = reltime()
    for arg in argv()
        if arg =~ '\.matrix$'
            call s:ReadMatrixFile(arg)
            call s:PrintPuzzle()
            let s:count = 0
            call s:Solve()
        endif
    endfor
    let end = reltime(start)
    echo "Seconds to process " . reltimestr(end)
    qall!
endfunction

call Main()
