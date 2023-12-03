#!/usr/bin/env tclsh

# Step 1: Read the Sudoku matrix from a file and return as a 1D array
proc readSudokuMatrix {filename} {
    set file [open $filename]
    set data {}
    while {[gets $file line] >= 0} {
        # Ignore comments
        if {[string match "#*" $line]} continue
        # Append numbers to data array
        lappend data {*}[split $line]
    }
    close $file
    return $data
}

# Step 2: Calculate the complexity of a Sudoku matrix
proc calculateComplexity {board} {
    # Complexity can be determined by the number of non-empty cells
    set complexity 0
    foreach cell $board {
        if {$cell != "0"} {
            incr complexity
        }
    }
    return $complexity
}

# Step 3: Print the Sudoku board in a 9x9 grid
proc printBoard {board {message ""}} {
    if {$message != ""} {
        puts $message
    }
    for {set i 0} {$i < 9} {incr i} {
        for {set j 0} {$j < 9} {incr j} {
            set idx [expr {$i * 9 + $j}]
            puts -nonewline "[lindex $board $idx] "
        }
        puts ""
    }
}

# Step 4: Sudoku solver using backtracking algorithm
proc solveSudoku {board} {
    set iterations 0
    for {set row 0} {$row < 9} {incr row} {
        for {set col 0} {$col < 9} {incr col} {
            set idx [expr {$row * 9 + $col}]
            if {[lindex $board $idx] == "0"} {
                for {set num 1} {$num <= 9} {incr num} {
                    if {[isValid $board $row $col $num]} {
                        set board [lreplace $board $idx $idx $num]
                        incr iterations
                        if {[solveSudoku $board]!=0} {
                            return [list $board $iterations]
                        }
                        set board [lreplace $board $idx $idx "0"]
                    }
                }
                return 0
            }
        }
    }
    return [list $board $iterations]
}

# Helper function to check if a number can be placed at a certain position
proc isValid {board row col num} {
    # Check the row
    for {set i 0} {$i < 9} {incr i} {
        if {[lindex $board [expr {$row * 9 + $i}]] == $num} {
            return 0
        }
    }
    # Check the column
    for {set i 0} {$i < 9} {incr i} {
        if {[lindex $board [expr {$i * 9 + $col}]] == $num} {
            return 0
        }
    }
    # Check the box
    set startRow [expr {$row - $row % 3}]
    set startCol [expr {$col - $col % 3}]
    for {set i $startRow} {$i < $startRow + 3} {incr i} {
        for {set j $startCol} {$j < $startCol + 3} {incr j} {
            if {[lindex $board [expr {$i * 9 + $j}]] == $num} {
                return 0
            }
        }
    }
    return 1
}

# Step 6: Main procedure to solve Sudoku from command line argument
proc main {argv} {
    set filename [lindex $argv 0]
    set board [readSudokuMatrix $filename]

    # Print initial board and its complexity
    printBoard $board "Initial Sudoku Board:"
    set complexity [calculateComplexity $board]
    puts "Complexity: $complexity"

    # Solve the Sudoku
    set result [solveSudoku $board]
    if {[lindex $result 0] == 0} {
        puts "Sudoku could not be solved."
        return
    }
    set solvedBoard [lindex $result 0]
    set iterations [lindex $result 1]

    # Print final board and number of iterations
    printBoard $solvedBoard "Solved Sudoku Board:"
    puts "Iterations: [format "%d" $iterations]"
}

# Start the program if it's not being sourced
if {$argc == 1} {
    main $argv
} else {
    puts "Usage: $argv0 <sudoku_file>"
}