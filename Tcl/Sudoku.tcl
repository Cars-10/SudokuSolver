  #!/usr/bin/env tclsh

set start [clock microseconds]

package require Tclx
global puzzle count

proc readMatrixFile {filename} {
    global puzzle
    set index  0
    set matrix  [split [read_file $filename] \n]
    foreach line $matrix {
        if ![string match "#*" $line] {
            if {[llength $line] == 9} {
                lassign [split $line " "] puzzle($index,0) puzzle($index,1) puzzle($index,2) puzzle($index,3)\
                 puzzle($index,4) puzzle($index,5) puzzle($index,6) puzzle($index,7) puzzle($index,8) 
                 incr index
            } else {
                puts "Matrix input error with line $line"
                exit 1
            }
        }
    }
}

proc printPuzzleValues {} {
    global puzzle
        puts "\nPuzzle:"
    for {set j 0} {$j < 9} {incr j} {
        for {set i 0} {$i < 9} {incr i} {
            puts -nonewline "$puzzle($j,$i) "
        }
        puts ""
    }
}

proc isPossible {y x val} {
    global puzzle
    # Find if a matching number (val) already exists
    # in the same row (y) or column (x) or within its rectangle
        
    for {set i 0} {$i < 9} {incr i} { if {$puzzle($i,$x) == $val} {return 0 }}
    for {set i 0} {$i < 9} {incr i} { if {$puzzle($y,$i) == $val} {return 0 }}
    
    # Search the Rectangle containing x & y
    # Find which 3x3 square we are in using the floor quotient
    set x0 [expr int(floor([expr $x/3*1.0]))*3]
    set y0 [expr int(floor([expr $y/3*1.0]))*3]
    #puts "Is possible x=$x x0=$x0 ,y=$y y0=$y0 val=$val" 
    for {set i 0} {$i < 3} {incr i} {
        for {set j 0} {$j < 3} {incr j} {
            #puts "y0+i=[expr $y0+$i] i=$i, x0+j=[expr $x0+$j] j=$j Puzzle(y0+i,x0+j)=$puzzle([expr $y0+$i],[expr $x0+$j]), val=$val"
            if {$puzzle([expr $y0+$i],[expr $x0+$j]) == $val} {return 0}
        }
    }
    #puts "YES Is possible $x, $y, $val"
    return 1
}

proc solve {} {
    global puzzle count
    for {set j 0} {$j < 9} {incr j} {
        for {set i 0} {$i < 9} {incr i} {
            if {$puzzle($j,$i) == 0} {
                for {set val 1} {$val < 10} {incr val} {
                    incr count
                    if {[isPossible $j $i $val]} {
                        set puzzle($j,$i) $val
                        solve
                        set puzzle($j,$i) 0
                    }
                }
                return
            }
        }
    }
    printPuzzleValues
    puts "\nSolved in Iterations=$count\n"
}

##### Main Program Starts Here #####
# For each .matrix file supplied on the commandline run the solver
foreach datafile $argv {
    if {[file extension $datafile] == ".matrix"} {
        puts $datafile
        readMatrixFile $datafile
        printPuzzleValues
        set count 0
        solve
    }
 }
puts "Seconds to process [expr ([clock microseconds] - $start)/1000000.0]"