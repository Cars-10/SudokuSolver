#!/usr/bin/env tclsh
# Sudoku Solver in Tcl
# Brute-force backtracking algorithm matching C reference exactly

# Global variables
array set puzzle {}
set count 0

proc read_matrix {filename} {
    global puzzle

    # Print filename (normalize /app/Matrices to ../Matrices)
    if {[string match "/app/Matrices/*" $filename]} {
        set display_path "../[string range $filename 5 end]"
        puts $display_path
    } else {
        puts $filename
    }

    set f [open $filename r]
    set row 0

    while {[gets $f line] >= 0 && $row < 9} {
        # Skip comments and empty lines
        set line [string trim $line]
        if {$line eq "" || [string index $line 0] eq "#"} {
            continue
        }

        # Parse 9 numbers from line
        set values [split $line " "]
        set col 0
        foreach val $values {
            if {$val ne "" && $col < 9} {
                set puzzle($row,$col) [expr {int($val)}]
                puts -nonewline "$puzzle($row,$col) "
                incr col
            }
        }
        puts ""
        incr row
    }

    close $f
}

proc print_puzzle {} {
    global puzzle
    puts "\nPuzzle:"
    for {set r 0} {$r < 9} {incr r} {
        for {set c 0} {$c < 9} {incr c} {
            puts -nonewline "$puzzle($r,$c) "
        }
        puts ""
    }
}

proc is_valid {row col val} {
    global puzzle

    # Check row
    for {set i 0} {$i < 9} {incr i} {
        if {$puzzle($row,$i) == $val} {
            return 0
        }
    }

    # Check column
    for {set i 0} {$i < 9} {incr i} {
        if {$puzzle($i,$col) == $val} {
            return 0
        }
    }

    # Check 3x3 box
    set box_row [expr {($row / 3) * 3}]
    set box_col [expr {($col / 3) * 3}]
    for {set i 0} {$i < 3} {incr i} {
        for {set j 0} {$j < 3} {incr j} {
            if {$puzzle([expr {$box_row + $i}],[expr {$box_col + $j}]) == $val} {
                return 0
            }
        }
    }

    return 1
}

proc solve {} {
    global puzzle count

    # Find first empty cell (row-major order)
    set found_row -1
    set found_col -1
    for {set r 0} {$r < 9} {incr r} {
        for {set c 0} {$c < 9} {incr c} {
            if {$puzzle($r,$c) == 0} {
                set found_row $r
                set found_col $c
                break
            }
        }
        if {$found_row != -1} {
            break
        }
    }

    # If no empty cell, puzzle is solved
    if {$found_row == -1} {
        print_puzzle
        puts "\nSolved in Iterations=$count\n"
        return 1
    }

    # Try values 1-9 in order
    for {set val 1} {$val <= 9} {incr val} {
        incr count  ;# Count EVERY attempt

        if {[is_valid $found_row $found_col $val]} {
            set puzzle($found_row,$found_col) $val

            if {[solve]} {
                return 1
            }

            set puzzle($found_row,$found_col) 0  ;# Backtrack
        }
    }

    return 0
}

# Main entry point
if {$argc < 1} {
    puts stderr "Usage: tclsh Sudoku.tcl <matrix_file>"
    exit 1
}

foreach datafile $argv {
    if {[file extension $datafile] eq ".matrix"} {
        read_matrix $datafile
        print_puzzle
        set count 0
        solve
    }
}
