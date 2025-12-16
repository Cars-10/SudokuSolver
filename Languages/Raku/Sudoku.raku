use v6;

my @puzzle;
my $iterations = 0;

sub print-board() {
    say "Puzzle:";
    for 0..8 -> $i {
        for 0..8 -> $j {
            print "@puzzle[$i][$j] ";
        }
        say "";
    }
}

sub is-possible($row, $col, $num) {
    for 0..8 -> $i {
        return False if @puzzle[$row][$i] == $num || @puzzle[$i][$col] == $num;
    }

    my $start-row = ($row div 3) * 3;
    my $start-col = ($col div 3) * 3;
    for 0..2 -> $i {
        for 0..2 -> $j {
            return False if @puzzle[$start-row + $i][$start-col + $j] == $num;
        }
    }
    return True;
}

sub solve($row, $col) {

    return True if $row == 9;

    my $next-row = $row;
    my $next-col = $col + 1;
    if $next-col == 9 {
        $next-row = $row + 1;
        $next-col = 0;
    }

    if @puzzle[$row][$col] != 0 {
        return solve($next-row, $next-col);
    }

    for 1..9 -> $num {
        $iterations++;
        if is-possible($row, $col, $num) {
            @puzzle[$row][$col] = $num;
            return True if solve($next-row, $next-col);
            @puzzle[$row][$col] = 0;
        }
    }
    return False;
}

sub read-board($filename) {
    @puzzle = ();
    try {
        for $filename.IO.lines -> $line {
            my $trimmed = $line.trim;
            if $trimmed && !$trimmed.starts-with("#") {
                my @parts = $trimmed.split(/\s+/, :skip-empty);
                my @row-vals;
                for @parts -> $part {
                    @row-vals.push($part.Int);
                }
                @puzzle.push(@row-vals) if @row-vals.elems > 0;
                return True if @puzzle.elems == 9;
            }
        }
        return True;
    }
    CATCH {
        default {
            say "Error reading file $filename: $_";
            return False;
        }
    }
}

sub MAIN(*@files) {
    if @files.elems == 0 {
        say "Usage: raku Sudoku.raku <file1> <file2> ...";
        exit 1;
    }

    for @files -> $filename {
        say "\nProcessing $filename";
        if read-board($filename) {
            print-board();
            $iterations = 0;
            if solve(0, 0) {
                print-board();
                say "\nSolved in Iterations=$iterations";
            } else {
                say "No solution found";
            }
        }
    }
}
