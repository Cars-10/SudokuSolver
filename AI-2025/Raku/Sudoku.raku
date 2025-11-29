use v6;

my @puzzle[9;9];
my $count = 0;

sub print-puzzle() {
    say "\nPuzzle:";
    for 0..8 -> $r {
        for 0..8 -> $c {
            print "@puzzle[$r;$c] ";
        }
        say "";
    }
}

sub read-matrix-file($filename) {
    say $filename;
    my $row = 0;
    for $filename.IO.lines -> $line {
        next if $line.starts-with("#") || $line.trim eq "";
        my @parts = $line.words;
        if @parts.elems == 9 {
            for 0..8 -> $col {
                @puzzle[$row;$col] = @parts[$col].Int;
            }
            $row++;
            last if $row == 9;
        }
    }
}

sub is-possible($r, $c, $val) {
    for 0..8 -> $i {
        return False if @puzzle[$i;$c] == $val;
        return False if @puzzle[$r;$i] == $val;
    }

    my $r0 = ($r div 3) * 3;
    my $c0 = ($c div 3) * 3;

    for 0..2 -> $i {
        for 0..2 -> $j {
            return False if @puzzle[$r0 + $i;$c0 + $j] == $val;
        }
    }
    True;
}

sub solve() {
    for 0..8 -> $r {
        for 0..8 -> $c {
            if @puzzle[$r;$c] == 0 {
                for 1..9 -> $val {
                    $count++;
                    if is-possible($r, $c, $val) {
                        @puzzle[$r;$c] = $val;
                        return True if solve();
                        @puzzle[$r;$c] = 0;
                    }
                }
                return False;
            }
        }
    }
    print-puzzle();
    say "\nSolved in Iterations=$count\n";
    True;
}

sub MAIN(*@args) {
    my $start = now;
    for @args -> $arg {
        if $arg.ends-with(".matrix") {
            read-matrix-file($arg);
            print-puzzle();
            $count = 0;
            solve();
        }
    }
    my $end = now;
    printf "Seconds to process %.3f\n", $end - $start;
}
