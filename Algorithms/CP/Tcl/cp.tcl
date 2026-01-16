#!/usr/bin/env tclsh
# Constraint Propagation (CP) Sudoku Solver - Tcl Implementation
# Algorithm: Constraint propagation with MRV (Minimum Remaining Values) heuristic

# Global iteration counter
set ::cp_iterations 0

# Grid structure: values and candidates
array set ::grid_values {}
array set ::grid_candidates {}

# Bitset operations
proc has_candidate {candidate_set digit} {
    return [expr {($candidate_set & (1 << $digit)) != 0}]
}

proc add_candidate {candidate_set digit} {
    return [expr {$candidate_set | (1 << $digit)}]
}

proc remove_candidate {candidate_set digit} {
    return [expr {$candidate_set & ~(1 << $digit)}]
}

proc count_candidates {candidate_set} {
    set count 0
    for {set i 1} {$i <= 9} {incr i} {
        if {[has_candidate $candidate_set $i]} {
            incr count
        }
    }
    return $count
}

proc get_first_candidate {candidate_set} {
    for {set i 1} {$i <= 9} {incr i} {
        if {[has_candidate $candidate_set $i]} {
            return $i
        }
    }
    return 0
}

# Get all peers for a cell (20 cells: 8 in row + 8 in col + 4 in box)
proc get_peers {row col} {
    set peers {}

    # Same row
    for {set c 0} {$c < 9} {incr c} {
        if {$c != $col} {
            lappend peers [list $row $c]
        }
    }

    # Same column
    for {set r 0} {$r < 9} {incr r} {
        if {$r != $row} {
            lappend peers [list $r $col]
        }
    }

    # Same 3x3 box
    set box_row [expr {($row / 3) * 3}]
    set box_col [expr {($col / 3) * 3}]
    for {set r $box_row} {$r < [expr {$box_row + 3}]} {incr r} {
        for {set c $box_col} {$c < [expr {$box_col + 3}]} {incr c} {
            if {$r != $row && $c != $col} {
                lappend peers [list $r $c]
            }
        }
    }

    return $peers
}

# Initialize grid from puzzle
proc init_grid {puzzle} {
    for {set row 0} {$row < 9} {incr row} {
        for {set col 0} {$col < 9} {incr col} {
            set val [lindex [lindex $puzzle $row] $col]
            if {$val == 0} {
                set ::grid_values($row,$col) 0
                set ::grid_candidates($row,$col) 0x3FE  ;# bits 1-9 set
            } else {
                set ::grid_values($row,$col) $val
                set ::grid_candidates($row,$col) [expr {1 << $val}]
            }
        }
    }
}

# Assign digit to cell at (row, col)
# Note: Must be defined before eliminate due to mutual recursion
proc assign {row col digit} {
    incr ::cp_iterations

    set ::grid_values($row,$col) $digit
    set ::grid_candidates($row,$col) [expr {1 << $digit}]

    # Eliminate from all peers
    foreach peer [get_peers $row $col] {
        lassign $peer peer_row peer_col
        if {![eliminate_impl $peer_row $peer_col $digit]} {
            return 0
        }
    }

    return 1
}

# Eliminate digit from candidates at (row, col)
proc eliminate_impl {row col digit} {
    # Check if already eliminated
    if {![has_candidate $::grid_candidates($row,$col) $digit]} {
        return 1
    }

    # Remove digit
    set ::grid_candidates($row,$col) [remove_candidate $::grid_candidates($row,$col) $digit]

    # Check for contradiction
    set remaining [count_candidates $::grid_candidates($row,$col)]
    if {$remaining == 0} {
        return 0
    }

    # Singleton elimination
    if {$remaining == 1 && $::grid_values($row,$col) == 0} {
        set last_digit [get_first_candidate $::grid_candidates($row,$col)]
        if {![assign $row $col $last_digit]} {
            return 0
        }
    }

    return 1
}

# Wrapper for eliminate to match expected name
proc eliminate {row col digit} {
    return [eliminate_impl $row $col $digit]
}

# Apply constraint propagation
proc propagate {} {
    set changed 1

    while {$changed} {
        set changed 0

        # Singleton elimination
        for {set row 0} {$row < 9} {incr row} {
            for {set col 0} {$col < 9} {incr col} {
                if {$::grid_values($row,$col) == 0} {
                    set num_candidates [count_candidates $::grid_candidates($row,$col)]
                    if {$num_candidates == 0} {
                        return 0
                    }
                    if {$num_candidates == 1} {
                        set digit [get_first_candidate $::grid_candidates($row,$col)]
                        if {![assign $row $col $digit]} {
                            return 0
                        }
                        set changed 1
                    }
                }
            }
        }

        # Hidden singles - rows
        for {set row 0} {$row < 9} {incr row} {
            for {set digit 1} {$digit <= 9} {incr digit} {
                set count 0
                set last_col -1
                set already_assigned 0

                for {set col 0} {$col < 9} {incr col} {
                    if {$::grid_values($row,$col) == $digit} {
                        set already_assigned 1
                        break
                    }
                    if {[has_candidate $::grid_candidates($row,$col) $digit]} {
                        incr count
                        set last_col $col
                    }
                }

                if {$already_assigned} continue

                if {$count == 1} {
                    if {![assign $row $last_col $digit]} {
                        return 0
                    }
                    set changed 1
                } elseif {$count == 0} {
                    return 0
                }
            }
        }

        # Hidden singles - columns
        for {set col 0} {$col < 9} {incr col} {
            for {set digit 1} {$digit <= 9} {incr digit} {
                set count 0
                set last_row -1
                set already_assigned 0

                for {set row 0} {$row < 9} {incr row} {
                    if {$::grid_values($row,$col) == $digit} {
                        set already_assigned 1
                        break
                    }
                    if {[has_candidate $::grid_candidates($row,$col) $digit]} {
                        incr count
                        set last_row $row
                    }
                }

                if {$already_assigned} continue

                if {$count == 1} {
                    if {![assign $last_row $col $digit]} {
                        return 0
                    }
                    set changed 1
                } elseif {$count == 0} {
                    return 0
                }
            }
        }

        # Hidden singles - boxes
        for {set box 0} {$box < 9} {incr box} {
            set box_row [expr {($box / 3) * 3}]
            set box_col [expr {($box % 3) * 3}]

            for {set digit 1} {$digit <= 9} {incr digit} {
                set count 0
                set last_r -1
                set last_c -1
                set already_assigned 0

                for {set r $box_row} {$r < [expr {$box_row + 3}]} {incr r} {
                    for {set c $box_col} {$c < [expr {$box_col + 3}]} {incr c} {
                        if {$::grid_values($r,$c) == $digit} {
                            set already_assigned 1
                            break
                        }
                        if {[has_candidate $::grid_candidates($r,$c) $digit]} {
                            incr count
                            set last_r $r
                            set last_c $c
                        }
                    }
                    if {$already_assigned} break
                }

                if {$already_assigned} continue

                if {$count == 1} {
                    if {![assign $last_r $last_c $digit]} {
                        return 0
                    }
                    set changed 1
                } elseif {$count == 0} {
                    return 0
                }
            }
        }
    }

    return 1
}

# Check if grid is complete
proc is_complete {} {
    for {set row 0} {$row < 9} {incr row} {
        for {set col 0} {$col < 9} {incr col} {
            if {$::grid_values($row,$col) == 0} {
                return 0
            }
        }
    }
    return 1
}

# Find cell with minimum remaining values (MRV heuristic)
proc find_mrv_cell {} {
    set min_count 10
    set min_row -1
    set min_col -1

    for {set row 0} {$row < 9} {incr row} {
        for {set col 0} {$col < 9} {incr col} {
            if {$::grid_values($row,$col) == 0} {
                set count [count_candidates $::grid_candidates($row,$col)]
                if {$count < $min_count} {
                    set min_count $count
                    set min_row $row
                    set min_col $col
                }
            }
        }
    }

    return [list $min_row $min_col]
}

# Save grid state
proc save_state {} {
    set state {}
    for {set row 0} {$row < 9} {incr row} {
        for {set col 0} {$col < 9} {incr col} {
            lappend state $::grid_values($row,$col) $::grid_candidates($row,$col)
        }
    }
    return $state
}

# Restore grid state
proc restore_state {state} {
    set idx 0
    for {set row 0} {$row < 9} {incr row} {
        for {set col 0} {$col < 9} {incr col} {
            set ::grid_values($row,$col) [lindex $state $idx]
            incr idx
            set ::grid_candidates($row,$col) [lindex $state $idx]
            incr idx
        }
    }
}

# Solve with search
proc cp_search {} {
    if {![propagate]} {
        return 0
    }

    if {[is_complete]} {
        return 1
    }

    lassign [find_mrv_cell] row col

    if {$row == -1} {
        return 0
    }

    set candidates $::grid_candidates($row,$col)

    for {set digit 1} {$digit <= 9} {incr digit} {
        if {[has_candidate $candidates $digit]} {
            set saved_state [save_state]

            if {[assign $row $col $digit]} {
                if {[cp_search]} {
                    return 1
                }
            }

            restore_state $saved_state
        }
    }

    return 0
}

# Read puzzle from file
proc read_puzzle {filename} {
    set fp [open $filename r]
    set puzzle {}

    while {[gets $fp line] >= 0} {
        set row {}
        foreach val $line {
            lappend row $val
        }
        if {[llength $row] == 9} {
            lappend puzzle $row
        }
    }

    close $fp
    return $puzzle
}

# Print grid
proc print_grid {} {
    puts "\nPuzzle:"
    for {set row 0} {$row < 9} {incr row} {
        set line ""
        for {set col 0} {$col < 9} {incr col} {
            append line "$::grid_values($row,$col) "
        }
        puts $line
    }
}

# Main
if {$argc < 1} {
    puts "Usage: $argv0 <matrix_file>"
    exit 1
}

set matrix_file [lindex $argv 0]
puts $matrix_file

set puzzle [read_puzzle $matrix_file]
init_grid $puzzle
print_grid

set start_time [clock milliseconds]
set result [cp_search]
set end_time [clock milliseconds]

print_grid

if {$result} {
    puts "\nSolved in Iterations=$::cp_iterations"
} else {
    puts "\nNo solution found"
}

set elapsed [expr {($end_time - $start_time) / 1000.0}]
puts "\nSeconds to process [format "%.3f" $elapsed]"
