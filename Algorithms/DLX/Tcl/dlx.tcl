#!/usr/bin/env tclsh
# Dancing Links (DLX) Sudoku Solver - Tcl Implementation
# Algorithm: Knuth's Algorithm X with Dancing Links data structure

# Global iteration counter (analogous to brute-force count)
set ::dlx_iterations 0

# Node counter for unique IDs
set ::node_counter 0

# DLX matrix structures
set ::root_node ""
array set ::columns {}       ;# 324 constraint columns
array set ::nodes {}          ;# All nodes by ID
array set ::row_info {}       ;# Row metadata: row, col, num
array set ::row_starts {}     ;# First node in each row

# Sudoku puzzle grid
array set ::puzzle {}
array set ::solution_grid {}

# Create a new node
proc create_node {} {
    set id [incr ::node_counter]
    set ::nodes($id,left) $id
    set ::nodes($id,right) $id
    set ::nodes($id,up) $id
    set ::nodes($id,down) $id
    set ::nodes($id,column) ""
    set ::nodes($id,row_id) -1
    return $id
}

# Create a column header
proc create_column {name} {
    set id [create_node]
    set ::nodes($id,name) $name
    set ::nodes($id,size) 0
    set ::nodes($id,column) $id
    set ::nodes($id,is_column) 1
    return $id
}

# Position constraint: cell (r,c) must be filled
proc get_position_col {r c} {
    return [expr {$r * 9 + $c}]
}

# Row constraint: row r must have number n
proc get_row_col {r n} {
    return [expr {81 + $r * 9 + ($n - 1)}]
}

# Column constraint: column c must have number n
proc get_col_col {c n} {
    return [expr {162 + $c * 9 + ($n - 1)}]
}

# Box constraint: box b must have number n
proc get_box_col {r c n} {
    set box [expr {($r / 3) * 3 + ($c / 3)}]
    return [expr {243 + $box * 9 + ($n - 1)}]
}

# Cover a column in the DLX matrix
proc dlx_cover_column {col_id} {
    # Remove column header from the header list
    set left $::nodes($col_id,left)
    set right $::nodes($col_id,right)
    set ::nodes($right,left) $left
    set ::nodes($left,right) $right

    # For each row in this column
    set row_node $::nodes($col_id,down)
    while {$row_node != $col_id} {
        # For each node in this row (excluding the column itself)
        set right_node $::nodes($row_node,right)
        while {$right_node != $row_node} {
            # Remove this node from its column
            set up $::nodes($right_node,up)
            set down $::nodes($right_node,down)
            set ::nodes($down,up) $up
            set ::nodes($up,down) $down

            set col $::nodes($right_node,column)
            incr ::nodes($col,size) -1

            set right_node $::nodes($right_node,right)
        }
        set row_node $::nodes($row_node,down)
    }
}

# Uncover a column (exact reverse of cover)
proc dlx_uncover_column {col_id} {
    # For each row in this column (in reverse order)
    set row_node $::nodes($col_id,up)
    while {$row_node != $col_id} {
        # For each node in this row (in reverse order)
        set left_node $::nodes($row_node,left)
        while {$left_node != $row_node} {
            # Restore this node to its column
            set col $::nodes($left_node,column)
            incr ::nodes($col,size)

            set up $::nodes($left_node,up)
            set down $::nodes($left_node,down)
            set ::nodes($down,up) $left_node
            set ::nodes($up,down) $left_node

            set left_node $::nodes($left_node,left)
        }
        set row_node $::nodes($row_node,up)
    }

    # Restore column header to the header list
    set left $::nodes($col_id,left)
    set right $::nodes($col_id,right)
    set ::nodes($right,left) $col_id
    set ::nodes($left,right) $col_id
}

# Choose column with minimum size (Knuth's S heuristic)
proc choose_column {} {
    set root $::root_node
    set best ""
    set min_size 999999

    set col_node $::nodes($root,right)
    while {$col_node != $root} {
        if {$::nodes($col_node,size) < $min_size} {
            set min_size $::nodes($col_node,size)
            set best $col_node
        }
        set col_node $::nodes($col_node,right)
    }

    return $best
}

# DLX Search - Algorithm X with Dancing Links
proc dlx_search {k solution_ref} {
    upvar $solution_ref solution
    incr ::dlx_iterations

    set root $::root_node

    # If matrix is empty, we found a solution
    if {$::nodes($root,right) == $root} {
        return 1
    }

    # Choose column with minimum size
    set col [choose_column]

    # If column has no rows, no solution possible
    if {$col == "" || $::nodes($col,size) == 0} {
        return 0
    }

    # Cover this column
    dlx_cover_column $col

    # Try each row in this column
    set row_node $::nodes($col,down)
    while {$row_node != $col} {
        # Add row to partial solution
        set solution($k) $::nodes($row_node,row_id)

        # Cover all other columns in this row
        set right_node $::nodes($row_node,right)
        while {$right_node != $row_node} {
            dlx_cover_column $::nodes($right_node,column)
            set right_node $::nodes($right_node,right)
        }

        # Recurse
        if {[dlx_search [expr {$k + 1}] solution]} {
            return 1
        }

        # Backtrack: uncover all columns in this row
        set left_node $::nodes($row_node,left)
        while {$left_node != $row_node} {
            dlx_uncover_column $::nodes($left_node,column)
            set left_node $::nodes($left_node,left)
        }

        set row_node $::nodes($row_node,down)
    }

    # Uncover column
    dlx_uncover_column $col

    return 0
}

# Initialize DLX matrix structure
proc init_dlx_matrix {} {
    # Reset counters
    set ::node_counter 0
    array unset ::nodes
    array unset ::columns
    array unset ::row_info
    array unset ::row_starts

    # Allocate root column
    set ::root_node [create_column "root"]

    # Allocate 324 column headers
    for {set i 0} {$i < 324} {incr i} {
        set col_id [create_column "C$i"]
        set ::columns($i) $col_id

        # Link into header list (append to end)
        set root $::root_node
        set left $::nodes($root,left)

        set ::nodes($col_id,left) $left
        set ::nodes($col_id,right) $root
        set ::nodes($left,right) $col_id
        set ::nodes($root,left) $col_id
    }
}

# Add a node to the DLX matrix
proc add_node {col_idx row_id} {
    set col $::columns($col_idx)
    set node_id [create_node]

    set ::nodes($node_id,column) $col
    set ::nodes($node_id,row_id) $row_id

    # Insert at end of column's circular list
    set up $::nodes($col,up)
    set ::nodes($node_id,down) $col
    set ::nodes($node_id,up) $up
    set ::nodes($up,down) $node_id
    set ::nodes($col,up) $node_id

    incr ::nodes($col,size)

    return $node_id
}

# Build a DLX row for Sudoku cell (r,c) with value n
proc build_dlx_row {r c n row_id} {
    # Store row metadata
    set ::row_info($row_id,row) $r
    set ::row_info($row_id,col) $c
    set ::row_info($row_id,num) $n

    # Create nodes for the 4 constraints
    set n1 [add_node [get_position_col $r $c] $row_id]
    set n2 [add_node [get_row_col $r $n] $row_id]
    set n3 [add_node [get_col_col $c $n] $row_id]
    set n4 [add_node [get_box_col $r $c $n] $row_id]

    # Link nodes horizontally in circular list
    set ::nodes($n1,right) $n2
    set ::nodes($n2,right) $n3
    set ::nodes($n3,right) $n4
    set ::nodes($n4,right) $n1

    set ::nodes($n1,left) $n4
    set ::nodes($n2,left) $n1
    set ::nodes($n3,left) $n2
    set ::nodes($n4,left) $n3

    # Store first node for this row
    set ::row_starts($row_id) $n1
}

# Build the complete DLX matrix from the puzzle
proc build_dlx_matrix_from_puzzle {} {
    set row_id 0

    for {set r 0} {$r < 9} {incr r} {
        for {set c 0} {$c < 9} {incr c} {
            if {$::puzzle($r,$c) != 0} {
                # Cell has a clue - create only one row for that value
                build_dlx_row $r $c $::puzzle($r,$c) $row_id
                incr row_id
            } else {
                # Cell is empty - create rows for all possible values
                for {set n 1} {$n <= 9} {incr n} {
                    build_dlx_row $r $c $n $row_id
                    incr row_id
                }
            }
        }
    }
}

# Cover given clues (pre-selected rows)
proc cover_clues {} {
    for {set r 0} {$r < 9} {incr r} {
        for {set c 0} {$c < 9} {incr c} {
            if {$::puzzle($r,$c) != 0} {
                set n $::puzzle($r,$c)

                # Find the row for this clue
                for {set row_id 0} {$row_id < 729} {incr row_id} {
                    if {[info exists ::row_starts($row_id)] &&
                        [info exists ::row_info($row_id,row)] &&
                        $::row_info($row_id,row) == $r &&
                        $::row_info($row_id,col) == $c &&
                        $::row_info($row_id,num) == $n} {

                        # Cover all columns in this row
                        set node $::row_starts($row_id)
                        set curr $node
                        while {1} {
                            dlx_cover_column $::nodes($curr,column)
                            set curr $::nodes($curr,right)
                            if {$curr == $node} break
                        }
                        break
                    }
                }
            }
        }
    }
}

# Extract solution from DLX and populate solution_grid
proc extract_solution {solution_ref solution_len} {
    upvar $solution_ref solution

    # Initialize solution grid - start with the original puzzle (includes clues)
    for {set r 0} {$r < 9} {incr r} {
        for {set c 0} {$c < 9} {incr c} {
            set ::solution_grid($r,$c) $::puzzle($r,$c)
        }
    }

    # Each solution entry is a row_id
    for {set i 0} {$i < $solution_len} {incr i} {
        if {[info exists solution($i)]} {
            set row_id $solution($i)
            if {$row_id >= 0 && $row_id < 729 && [info exists ::row_info($row_id,row)]} {
                set r $::row_info($row_id,row)
                set c $::row_info($row_id,col)
                set n $::row_info($row_id,num)
                set ::solution_grid($r,$c) $n
            }
        }
    }
}

# Print puzzle
proc print_puzzle {grid_ref} {
    upvar $grid_ref grid
    puts "\nPuzzle:"
    for {set r 0} {$r < 9} {incr r} {
        set line ""
        for {set c 0} {$c < 9} {incr c} {
            append line "$grid($r,$c) "
        }
        puts $line
    }
}

# Read matrix file
proc read_matrix_file {filename} {
    # Normalize path for output (convert absolute to relative)
    set display_path $filename
    if {[string match "/app/Matrices/*" $filename]} {
        set display_path "../[string range $filename 5 end]"
    }
    puts $display_path

    set fp [open $filename r]
    set line_count 0

    while {[gets $fp line] >= 0} {
        # Skip comments and empty lines
        set line [string trim $line]
        if {$line == "" || [string match "#*" $line]} {
            continue
        }

        # Parse 9 integers from line
        set values [split $line]
        if {[llength $values] == 9 && $line_count < 9} {
            for {set c 0} {$c < 9} {incr c} {
                set ::puzzle($line_count,$c) [lindex $values $c]
            }
            puts "$line "
            incr line_count
        }
    }

    close $fp

    if {$line_count != 9} {
        error "Expected 9 lines, got $line_count"
    }
}

# Main program
proc main {} {
    global argc argv

    set start_time [clock milliseconds]

    # Process each .matrix file from command line
    foreach arg $argv {
        if {[string match "*.matrix" $arg]} {
            # Reset puzzle
            array unset ::puzzle
            array unset ::solution_grid
            for {set r 0} {$r < 9} {incr r} {
                for {set c 0} {$c < 9} {incr c} {
                    set ::puzzle($r,$c) 0
                    set ::solution_grid($r,$c) 0
                }
            }

            if {[catch {
                read_matrix_file $arg
                print_puzzle ::puzzle

                # Initialize DLX matrix
                init_dlx_matrix

                # Build matrix from puzzle
                build_dlx_matrix_from_puzzle

                # Cover pre-filled clues
                cover_clues

                # Solve using DLX
                set ::dlx_iterations 0
                array set solution {}
                set result [dlx_search 0 solution]

                if {$result} {
                    extract_solution solution 81
                    print_puzzle ::solution_grid
                    puts "\nSolved in Iterations=$::dlx_iterations\n"
                } else {
                    puts "\nNo solution found\n"
                }
            } err]} {
                puts stderr "Error processing $arg: $err"
                continue
            }
        }
    }

    set end_time [clock milliseconds]
    set elapsed [expr {($end_time - $start_time) / 1000.0}]
    puts [format "Seconds to process %.3f" $elapsed]
}

# Run main if executed as script
if {$argc < 1} {
    puts "Usage: $argv0 <matrix_file>"
    exit 1
}

main
