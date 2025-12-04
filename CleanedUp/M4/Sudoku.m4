dnl Sudoku Solver in M4
dnl This is a simplified solver using recursion and macros

define(`forloop', `pushdef(`$1', `$2')_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_forloop', `$4`'ifelse($1, `$3', , `define(`$1', incr($1))_forloop(`$1', `$2', `$3', `$4')')')

define(`get', `defn(`cell_$1_$2')')
define(`set', `define(`cell_$1_$2', `$3')')

dnl Initialize board
forloop(`r', 0, 8, `forloop(`c', 0, 8, `set(r, c, 0)')')

dnl Read input (simulated for now, M4 file I/O is hard)
dnl We will pass input as arguments or include a file that defines the board
dnl But for this benchmark, we need to read a file.
dnl We can use syscmd to cat the file and parse it?
dnl Or we can just use a wrapper script to convert input to M4 definitions.

dnl Let's assume the wrapper converts "123..." to a series of set() calls.

define(`solve', `
    dnl Find empty
    pushdef(`found', 0)
    pushdef(`er', -1)
    pushdef(`ec', -1)
    
    forloop(`r', 0, 8, `
        ifelse(found, 0, `
            forloop(`c', 0, 8, `
                ifelse(found, 0, `
                    ifelse(get(r, c), 0, `
                        define(`found', 1)
                        define(`er', r)
                        define(`ec', c)
                    ')
                ')
            ')
        ')
    ')
    
    ifelse(found, 0, `1', `
        pushdef(`res', 0)
        forloop(`n', 1, 9, `
            ifelse(res, 0, `
                dnl Check valid
                pushdef(`valid', 1)
                
                dnl Row
                forloop(`k', 0, 8, `
                    ifelse(get(er, k), n, `define(`valid', 0)')
                ')
                
                dnl Col
                ifelse(valid, 1, `
                    forloop(`k', 0, 8, `
                        ifelse(get(k, ec), n, `define(`valid', 0)')
                    ')
                ')
                
                dnl Box
                ifelse(valid, 1, `
                    pushdef(`sr', eval(er / 3 * 3))
                    pushdef(`sc', eval(ec / 3 * 3))
                    forloop(`i', 0, 2, `
                        forloop(`j', 0, 2, `
                            ifelse(get(eval(sr+i), eval(sc+j)), n, `define(`valid', 0)')
                        ')
                    ')
                    popdef(`sr')
                    popdef(`sc')
                ')
                
                ifelse(valid, 1, `
                    set(er, ec, n)
                    ifelse(solve(), 1, `define(`res', 1)', `set(er, ec, 0)')
                ')
                popdef(`valid')
            ')
        ')
        res
        popdef(`res')
    ')
    popdef(`found')
    popdef(`er')
    popdef(`ec')
')

dnl Print board
define(`print_board', `
    forloop(`r', 0, 8, `
        forloop(`c', 0, 8, `get(r, c) ')
        errprint(`
')
    ')
')

dnl Main execution happens when the file is processed
