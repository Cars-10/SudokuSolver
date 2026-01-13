#!/usr/bin/env raku
use v6;

my @puzzle[9;9];
my $count = 0;

sub is-valid(Int $row, Int $col, Int $val --> Bool) {
    # Check row
    for ^9 -> $i {
        return False if @puzzle[$row;$i] == $val;
    }

    # Check column
    for ^9 -> $i {
        return False if @puzzle[$i;$col] == $val;
    }

    # Check 3x3 box
    my $br = ($row div 3) * 3;
    my $bc = ($col div 3) * 3;
    for ^3 -> $i {
        for ^3 -> $j {
            return False if @puzzle[$br+$i;$bc+$j] == $val;
        }
    }
    return True;
}

sub solve(--> Bool) {
    # Find first empty cell (row-major order)
    my $row = -1;
    my $col = -1;

    for ^9 -> $r {
        for ^9 -> $c {
            if @puzzle[$r;$c] == 0 {
                $row = $r;
                $col = $c;
                last;
            }
        }
        last if $row >= 0;
    }

    # If no empty cell found, puzzle is solved
    if $row == -1 {
        return True;
    }

    # Try values 1-9 in order
    for 1..9 -> $val {
        $count++;  # COUNT BEFORE validity check
        if is-valid($row, $col, $val) {
            @puzzle[$row;$col] = $val;

            if solve() {
                return True;
            }

            @puzzle[$row;$col] = 0;  # Backtrack
        }
    }

    return False;
}

sub print-puzzle() {
    say "";
    say "Puzzle:";
    for ^9 -> $row {
        for ^9 -> $col {
            print "@puzzle[$row;$col] ";
        }
        say "";
    }
}

sub read-matrix-file(Str $filename) {
    # Normalize path for output (convert absolute to relative)
    my $display-path = $filename;
    if $filename.starts-with('/app/Matrices/') {
        $display-path = $filename.substr(5);  # Skip "/app/" to get "Matrices/..."
        say "../$display-path";
    } else {
        say $filename;
    }

    my $line-count = 0;

    for $filename.IO.lines -> $line {
        # Skip comments and empty lines
        next if $line ~~ /^ \s* '#'/ || $line ~~ /^ \s* $/;

        my @values = $line.words;
        if @values.elems == 9 {
            for ^9 -> $i {
                @puzzle[$line-count;$i] = @values[$i].Int;
                print "@values[$i] ";
            }
            say "";
            $line-count++;
            last if $line-count == 9;
        }
    }
}

sub MAIN(*@files) {
    my $start-time = now;

    for @files -> $filename {
        next unless $filename.ends-with('.matrix');

        read-matrix-file($filename);
        print-puzzle();

        $count = 0;
        if solve() {
            print-puzzle();
            say "";
            say "Solved in Iterations=$count";
            say "";
        } else {
            say "No solution found";
        }
    }

    my $elapsed = now - $start-time;
    printf("Seconds to process %.3f\n", $elapsed);
}
