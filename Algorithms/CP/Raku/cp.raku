#!/usr/bin/env raku
use v6;

# CP Grid class
class CPGrid {
    has Int @.values[9;9];
    has Int @.candidates[9;9];

    method clone-grid(--> CPGrid) {
        my $grid = CPGrid.new;
        for ^9 -> $r {
            for ^9 -> $c {
                $grid.values[$r;$c] = @!values[$r;$c];
                $grid.candidates[$r;$c] = @!candidates[$r;$c];
            }
        }
        return $grid;
    }
}

# Global state
my $cp-iterations = 0;

# Bitset helpers
sub has-candidate(Int $set, Int $digit --> Bool) {
    return ($set +& (1 +< $digit)) != 0;
}

sub remove-candidate(Int $set, Int $digit --> Int) {
    return $set +& +^(1 +< $digit);
}

sub count-candidates(Int $set --> Int) {
    my $count = 0;
    my $s = $set;
    while $s {
        $count += $s +& 1;
        $s +>= 1;
    }
    return $count;
}

sub get-first-candidate(Int $set --> Int) {
    for 1..9 -> $digit {
        return $digit if has-candidate($set, $digit);
    }
    return 0;
}

# Get peers for a cell
sub get-peers(Int $row, Int $col --> Array) {
    my @peers;

    # Same row
    for ^9 -> $c {
        @peers.push([$row, $c]) if $c != $col;
    }

    # Same column
    for ^9 -> $r {
        @peers.push([$r, $col]) if $r != $row;
    }

    # Same 3x3 box
    my $box-row = ($row div 3) * 3;
    my $box-col = ($col div 3) * 3;
    for $box-row .. $box-row + 2 -> $r {
        for $box-col .. $box-col + 2 -> $c {
            @peers.push([$r, $c]) if $r != $row && $c != $col;
        }
    }

    return @peers;
}

# Initialize grid
sub init-grid(CPGrid $grid, @puzzle) {
    for ^9 -> $row {
        for ^9 -> $col {
            if @puzzle[$row][$col] == 0 {
                # Empty cell: set all candidates 1-9
                $grid.values[$row;$col] = 0;
                $grid.candidates[$row;$col] = 0x3FE;
            } else {
                # Given clue: set single value
                my $digit = @puzzle[$row][$col];
                $grid.values[$row;$col] = $digit;
                $grid.candidates[$row;$col] = (1 +< $digit);
            }
        }
    }
}

# Eliminate a candidate
sub eliminate(CPGrid $grid, Int $row, Int $col, Int $digit --> Bool) {
    # Check if digit is already eliminated
    return True unless has-candidate($grid.candidates[$row;$col], $digit);

    # Remove digit from candidates
    $grid.candidates[$row;$col] = remove-candidate($grid.candidates[$row;$col], $digit);

    # Check for contradiction
    my $remaining = count-candidates($grid.candidates[$row;$col]);
    return False if $remaining == 0;

    # If only one candidate left, assign it
    if $remaining == 1 && $grid.values[$row;$col] == 0 {
        my $last-digit = get-first-candidate($grid.candidates[$row;$col]);
        return False unless assign($grid, $row, $col, $last-digit);
    }

    return True;
}

# Assign a value
sub assign(CPGrid $grid, Int $row, Int $col, Int $digit --> Bool) {
    # Increment iteration counter
    $cp-iterations++;

    # Set value
    $grid.values[$row;$col] = $digit;
    $grid.candidates[$row;$col] = (1 +< $digit);

    # Eliminate digit from all peers
    my @peers = get-peers($row, $col);

    for @peers -> @peer {
        return False unless eliminate($grid, @peer[0], @peer[1], $digit);
    }

    return True;
}

# Propagate constraints
sub propagate(CPGrid $grid --> Bool) {
    my $changed = True;

    while $changed {
        $changed = False;

        # Strategy 1: Singleton elimination
        for ^9 -> $row {
            for ^9 -> $col {
                if $grid.values[$row;$col] == 0 {
                    my $num-candidates = count-candidates($grid.candidates[$row;$col]);
                    return False if $num-candidates == 0;

                    if $num-candidates == 1 {
                        my $digit = get-first-candidate($grid.candidates[$row;$col]);
                        return False unless assign($grid, $row, $col, $digit);
                        $changed = True;
                    }
                }
            }
        }

        # Strategy 2: Hidden singles - Rows
        for ^9 -> $row {
            for 1..9 -> $digit {
                my $count = 0;
                my $last-col = -1;

                for ^9 -> $col {
                    if $grid.values[$row;$col] == $digit {
                        $count = 0;
                        last;
                    }
                    if has-candidate($grid.candidates[$row;$col], $digit) {
                        $count++;
                        $last-col = $col;
                    }
                }

                if $count == 1 {
                    return False unless assign($grid, $row, $last-col, $digit);
                    $changed = True;
                } elsif $count == 0 {
                    my $found = False;
                    for ^9 -> $col {
                        if $grid.values[$row;$col] == $digit {
                            $found = True;
                            last;
                        }
                    }
                    return False unless $found;
                }
            }
        }

        # Hidden singles - Columns
        for ^9 -> $col {
            for 1..9 -> $digit {
                my $count = 0;
                my $last-row = -1;

                for ^9 -> $row {
                    if $grid.values[$row;$col] == $digit {
                        $count = 0;
                        last;
                    }
                    if has-candidate($grid.candidates[$row;$col], $digit) {
                        $count++;
                        $last-row = $row;
                    }
                }

                if $count == 1 {
                    return False unless assign($grid, $last-row, $col, $digit);
                    $changed = True;
                } elsif $count == 0 {
                    my $found = False;
                    for ^9 -> $row {
                        if $grid.values[$row;$col] == $digit {
                            $found = True;
                            last;
                        }
                    }
                    return False unless $found;
                }
            }
        }

        # Hidden singles - Boxes
        for ^9 -> $box {
            my $box-row = ($box div 3) * 3;
            my $box-col = ($box % 3) * 3;

            for 1..9 -> $digit {
                my $count = 0;
                my $last-r = -1;
                my $last-c = -1;
                my $found-assigned = False;

                for $box-row .. $box-row + 2 -> $r {
                    for $box-col .. $box-col + 2 -> $c {
                        if $grid.values[$r;$c] == $digit {
                            $found-assigned = True;
                            $count = 0;
                            last;
                        }
                        if has-candidate($grid.candidates[$r;$c], $digit) {
                            $count++;
                            $last-r = $r;
                            $last-c = $c;
                        }
                    }
                    last if $found-assigned;
                }

                if $count == 1 {
                    return False unless assign($grid, $last-r, $last-c, $digit);
                    $changed = True;
                } elsif $count == 0 {
                    my $found = False;
                    for $box-row .. $box-row + 2 -> $r {
                        for $box-col .. $box-col + 2 -> $c {
                            if $grid.values[$r;$c] == $digit {
                                $found = True;
                                last;
                            }
                        }
                        last if $found;
                    }
                    return False unless $found;
                }
            }
        }
    }

    return True;
}

# Find MRV cell
sub find-mrv-cell(CPGrid $grid) {
    my $min-candidates = 10;
    my $mrv-row = -1;
    my $mrv-col = -1;

    for ^9 -> $r {
        for ^9 -> $c {
            if $grid.values[$r;$c] == 0 {
                my $num-candidates = count-candidates($grid.candidates[$r;$c]);
                if $num-candidates < $min-candidates {
                    $min-candidates = $num-candidates;
                    $mrv-row = $r;
                    $mrv-col = $c;
                }
            }
        }
    }

    return ($mrv-row == -1) ?? Nil !! { row => $mrv-row, col => $mrv-col };
}

# CP Search
sub cp-search(CPGrid $grid, @solution --> Bool) {
    # Base case: check if grid is complete
    my $mrv-cell = find-mrv-cell($grid);
    if !$mrv-cell {
        # No empty cells - extract solution
        for ^9 -> $r {
            for ^9 -> $c {
                @solution[$r * 9 + $c] = $grid.values[$r;$c];
            }
        }
        return True;
    }

    # Try each candidate for MRV cell
    my $candidates = $grid.candidates[$mrv-cell<row>;$mrv-cell<col>];

    for 1..9 -> $digit {
        if has-candidate($candidates, $digit) {
            # Save grid state for backtracking
            my $grid-copy = $grid.clone-grid;

            # Try assigning this digit
            if assign($grid, $mrv-cell<row>, $mrv-cell<col>, $digit) {
                # Propagate constraints
                if propagate($grid) {
                    # Recurse
                    return True if cp-search($grid, @solution);
                }
            }

            # Failed - restore grid state
            for ^9 -> $r {
                for ^9 -> $c {
                    $grid.values[$r;$c] = $grid-copy.values[$r;$c];
                    $grid.candidates[$r;$c] = $grid-copy.candidates[$r;$c];
                }
            }
        }
    }

    return False;
}

# Helper functions
sub print-puzzle(@grid) {
    say "\nPuzzle:";
    for @grid -> @row {
        say @row.join(" ") ~ " ";
    }
}

sub read-matrix-file(Str $filename --> Array) {
    # Normalize path for output
    my $display-path = $filename;
    if $filename.starts-with('/app/Matrices/') {
        $display-path = $filename.substr(5);
        say "../$display-path";
    } else {
        say $filename;
    }

    my @puzzle;
    my $line-count = 0;

    for $filename.IO.lines -> $line {
        # Skip comments and empty lines
        next if $line ~~ /^ \s* '#'/ || $line ~~ /^ \s* $/;

        my @values = $line.words.map(*.Int);
        if @values.elems == 9 && $line-count < 9 {
            @puzzle.push(@values);
            say @values.join(" ") ~ " ";
            $line-count++;
        }
    }

    return @puzzle;
}

# Main
sub MAIN(*@files) {
    my $start-time = now;

    for @files -> $filename {
        next unless $filename.ends-with('.matrix');

        my @puzzle = read-matrix-file($filename);
        next unless @puzzle.elems == 9;

        print-puzzle(@puzzle);

        # Initialize CP grid
        my $grid = CPGrid.new;
        init-grid($grid, @puzzle);

        # Apply initial propagation
        unless propagate($grid) {
            say "\nNo solution found (contradiction during initial propagation)\n";
            next;
        }

        # Run search
        my @solution = 0 xx 81;
        $cp-iterations = 0;
        my $solved = cp-search($grid, @solution);

        if $solved {
            # Convert solution array back to 2D
            my @solution-grid;
            for ^9 -> $r {
                my @row;
                for ^9 -> $c {
                    @row.push(@solution[$r * 9 + $c]);
                }
                @solution-grid.push(@row);
            }

            print-puzzle(@solution-grid);
            say "\nSolved in Iterations=$cp-iterations\n";
        } else {
            say "\nNo solution found\n";
        }
    }

    my $elapsed = now - $start-time;
    printf("Seconds to process %.3f\n", $elapsed);
}
