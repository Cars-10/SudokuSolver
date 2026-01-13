#!/usr/bin/env perl
# Constraint Propagation (CP) Sudoku Solver - Perl Implementation
# Mechanical translation from C reference to preserve algorithm correctness.
#
# Algorithm: Constraint propagation with MRV (Minimum Remaining Values) heuristic
# - Uses bitsets to track candidate values (bits 1-9)
# - Propagates constraints: singleton elimination, hidden singles
# - Search with MRV cell selection for efficiency

use strict;
use warnings;
use Time::HiRes qw(time);

# Global iteration counter
my $cp_iterations = 0;

# CPGrid structure (hash reference)
sub new_cp_grid {
    return {
        values => [map { [(0) x 9] } (0..8)],         # Assigned values (0 = empty)
        candidates => [map { [(0) x 9] } (0..8)]      # Possible values per cell (bitset)
    };
}

# Bitset helper functions
sub has_candidate {
    my ($candidate_set, $digit) = @_;
    return ($candidate_set & (1 << $digit)) != 0;
}

sub add_candidate {
    my ($candidate_set, $digit) = @_;
    return $candidate_set | (1 << $digit);
}

sub remove_candidate {
    my ($candidate_set, $digit) = @_;
    return $candidate_set & ~(1 << $digit);
}

sub count_candidates {
    my ($candidate_set) = @_;
    my $count = 0;
    while ($candidate_set) {
        $count += $candidate_set & 1;
        $candidate_set >>= 1;
    }
    return $count;
}

sub get_first_candidate {
    my ($candidate_set) = @_;
    for my $digit (1..9) {
        return $digit if has_candidate($candidate_set, $digit);
    }
    return 0;
}

# Get all 20 peers for a cell (row, col, box)
sub get_peers {
    my ($row, $col) = @_;
    my @peers;

    # Same row (9 cells minus self = 8)
    for my $c (0..8) {
        push @peers, [$row, $c] if $c != $col;
    }

    # Same column (9 cells minus self = 8)
    for my $r (0..8) {
        push @peers, [$r, $col] if $r != $row;
    }

    # Same 3x3 box (9 cells minus self minus already counted = 4)
    my $box_row = int($row / 3) * 3;
    my $box_col = int($col / 3) * 3;
    for my $r ($box_row .. $box_row + 2) {
        for my $c ($box_col .. $box_col + 2) {
            push @peers, [$r, $c] if $r != $row && $c != $col;
        }
    }

    return @peers;
}

# Initialize grid from puzzle
sub init_grid {
    my ($grid, $puzzle) = @_;
    for my $row (0..8) {
        for my $col (0..8) {
            if ($puzzle->[$row][$col] == 0) {
                # Empty cell: set all candidates 1-9 (bits 1-9 set)
                $grid->{values}[$row][$col] = 0;
                $grid->{candidates}[$row][$col] = 0x3FE;  # Binary: 0011 1111 1110
            } else {
                # Given clue: set single value
                my $digit = $puzzle->[$row][$col];
                $grid->{values}[$row][$col] = $digit;
                $grid->{candidates}[$row][$col] = (1 << $digit);
            }
        }
    }
}

# Forward declaration for mutual recursion
sub assign;

# Eliminate digit from candidates at (row, col)
sub eliminate {
    my ($grid, $row, $col, $digit) = @_;

    # Check if digit is already eliminated
    return 1 unless has_candidate($grid->{candidates}[$row][$col], $digit);

    # Remove digit from candidates
    $grid->{candidates}[$row][$col] = remove_candidate($grid->{candidates}[$row][$col], $digit);

    # Check for contradiction (no candidates left)
    my $remaining = count_candidates($grid->{candidates}[$row][$col]);
    return 0 if $remaining == 0;

    # If only one candidate left, assign it (singleton elimination)
    if ($remaining == 1 && $grid->{values}[$row][$col] == 0) {
        my $last_digit = get_first_candidate($grid->{candidates}[$row][$col]);
        return 0 unless assign($grid, $row, $col, $last_digit);
    }

    return 1;
}

# Assign digit to cell at (row, col)
sub assign {
    my ($grid, $row, $col, $digit) = @_;

    # Increment iteration counter (this is our benchmark metric)
    $cp_iterations++;

    # Set value
    $grid->{values}[$row][$col] = $digit;
    $grid->{candidates}[$row][$col] = (1 << $digit);

    # Eliminate digit from all peers
    my @peers = get_peers($row, $col);
    for my $peer (@peers) {
        my ($peer_row, $peer_col) = @$peer;
        return 0 unless eliminate($grid, $peer_row, $peer_col, $digit);
    }

    return 1;
}

# Apply constraint propagation until quiescence
sub propagate {
    my ($grid) = @_;
    my $changed = 1;

    while ($changed) {
        $changed = 0;

        # Strategy 1: Singleton elimination
        # If a cell has only one candidate, assign it
        for my $row (0..8) {
            for my $col (0..8) {
                if ($grid->{values}[$row][$col] == 0) {
                    my $num_candidates = count_candidates($grid->{candidates}[$row][$col]);
                    return 0 if $num_candidates == 0;  # Contradiction
                    if ($num_candidates == 1) {
                        my $digit = get_first_candidate($grid->{candidates}[$row][$col]);
                        return 0 unless assign($grid, $row, $col, $digit);
                        $changed = 1;
                    }
                }
            }
        }

        # Strategy 2: Hidden singles
        # For each unit (row, col, box), if a digit appears in only one cell, assign it

        # Check rows
        for my $row (0..8) {
            for my $digit (1..9) {
                my $count = 0;
                my $last_col = -1;
                my $already_assigned = 0;

                for my $col (0..8) {
                    if ($grid->{values}[$row][$col] == $digit) {
                        $already_assigned = 1;
                        last;
                    }
                    if (has_candidate($grid->{candidates}[$row][$col], $digit)) {
                        $count++;
                        $last_col = $col;
                    }
                }

                next if $already_assigned;

                if ($count == 1) {
                    return 0 unless assign($grid, $row, $last_col, $digit);
                    $changed = 1;
                } elsif ($count == 0) {
                    return 0;  # Digit cannot be placed anywhere in row
                }
            }
        }

        # Check columns
        for my $col (0..8) {
            for my $digit (1..9) {
                my $count = 0;
                my $last_row = -1;
                my $already_assigned = 0;

                for my $row (0..8) {
                    if ($grid->{values}[$row][$col] == $digit) {
                        $already_assigned = 1;
                        last;
                    }
                    if (has_candidate($grid->{candidates}[$row][$col], $digit)) {
                        $count++;
                        $last_row = $row;
                    }
                }

                next if $already_assigned;

                if ($count == 1) {
                    return 0 unless assign($grid, $last_row, $col, $digit);
                    $changed = 1;
                } elsif ($count == 0) {
                    return 0;  # Digit cannot be placed anywhere in column
                }
            }
        }

        # Check boxes
        for my $box (0..8) {
            my $box_row = int($box / 3) * 3;
            my $box_col = ($box % 3) * 3;

            for my $digit (1..9) {
                my $count = 0;
                my ($last_r, $last_c) = (-1, -1);
                my $already_assigned = 0;

                FIND_IN_BOX: for my $r ($box_row .. $box_row + 2) {
                    for my $c ($box_col .. $box_col + 2) {
                        if ($grid->{values}[$r][$c] == $digit) {
                            $already_assigned = 1;
                            last FIND_IN_BOX;
                        }
                        if (has_candidate($grid->{candidates}[$r][$c], $digit)) {
                            $count++;
                            ($last_r, $last_c) = ($r, $c);
                        }
                    }
                }

                next if $already_assigned;

                if ($count == 1) {
                    return 0 unless assign($grid, $last_r, $last_c, $digit);
                    $changed = 1;
                } elsif ($count == 0) {
                    return 0;  # Digit cannot be placed anywhere in box
                }
            }
        }
    }

    return 1;  # Success - reached fixpoint
}

# Find empty cell with Minimum Remaining Values (fewest candidates)
sub find_mrv_cell {
    my ($grid) = @_;
    my $min_candidates = 10;  # More than 9
    my ($mrv_row, $mrv_col) = (-1, -1);
    my $found = 0;

    for my $r (0..8) {
        for my $c (0..8) {
            if ($grid->{values}[$r][$c] == 0) {
                my $num_candidates = count_candidates($grid->{candidates}[$r][$c]);
                if ($num_candidates < $min_candidates) {
                    $min_candidates = $num_candidates;
                    ($mrv_row, $mrv_col) = ($r, $c);
                    $found = 1;
                }
            }
        }
    }

    return $found ? ($mrv_row, $mrv_col) : ();
}

# Deep copy grid for backtracking
sub copy_grid {
    my ($grid) = @_;
    my $copy = new_cp_grid();
    for my $r (0..8) {
        for my $c (0..8) {
            $copy->{values}[$r][$c] = $grid->{values}[$r][$c];
            $copy->{candidates}[$r][$c] = $grid->{candidates}[$r][$c];
        }
    }
    return $copy;
}

# Search with constraint propagation
sub cp_search {
    my ($grid, $solution) = @_;

    # Base case: check if grid is complete
    my @mrv_cell = find_mrv_cell($grid);
    unless (@mrv_cell) {
        # No empty cells - grid is complete, extract solution
        for my $r (0..8) {
            for my $c (0..8) {
                $solution->[$r * 9 + $c] = $grid->{values}[$r][$c];
            }
        }
        return 1;
    }

    my ($mrv_row, $mrv_col) = @mrv_cell;

    # Recursive case: try each candidate for the MRV cell
    my $candidates = $grid->{candidates}[$mrv_row][$mrv_col];

    for my $digit (1..9) {
        if (has_candidate($candidates, $digit)) {
            # Save grid state for backtracking
            my $grid_copy = copy_grid($grid);

            # Try assigning this digit
            if (assign($grid, $mrv_row, $mrv_col, $digit)) {
                # Assignment succeeded, propagate constraints
                if (propagate($grid)) {
                    # Propagation succeeded, recurse
                    return 1 if cp_search($grid, $solution);
                }
            }

            # Failed - restore grid state and try next candidate
            for my $r (0..8) {
                for my $c (0..8) {
                    $grid->{values}[$r][$c] = $grid_copy->{values}[$r][$c];
                    $grid->{candidates}[$r][$c] = $grid_copy->{candidates}[$r][$c];
                }
            }
        }
    }

    # All candidates exhausted - dead end
    return 0;
}

# Print puzzle
sub print_puzzle {
    my ($puzzle) = @_;
    print "\nPuzzle:\n";
    for my $r (0..8) {
        print join(" ", @{$puzzle->[$r]}) . " \n";
    }
}

# Read matrix file
sub read_matrix_file {
    my ($filename) = @_;
    my @puzzle = map { [(0) x 9] } (0..8);

    # Normalize path for output
    my $display_path = $filename;
    if ($filename =~ /^\/app\/Matrices\//) {
        $display_path = "../" . substr($filename, 5);
    }
    print "$display_path\n";

    open(my $fh, '<', $filename) or die "Cannot open $filename: $!";
    my $line_count = 0;

    while (my $line = <$fh>) {
        chomp $line;
        $line =~ s/^\s+|\s+$//g;

        # Skip comments and empty lines
        next if $line eq '' || $line =~ /^#/;

        # Parse 9 integers from line
        my @values = split(/\s+/, $line);
        if (scalar(@values) == 9 && $line_count < 9) {
            $puzzle[$line_count] = [@values];
            print join(" ", @values) . " \n";
            $line_count++;
        }
    }
    close($fh);

    die "Expected 9 lines, got $line_count" if $line_count != 9;
    return \@puzzle;
}

# Main program
unless (@ARGV == 1) {
    print STDERR "Usage: $0 <matrix_file>\n";
    exit 1;
}

my $start_time = time();

eval {
    # Read puzzle from file
    my $puzzle = read_matrix_file($ARGV[0]);
    print_puzzle($puzzle);

    # Initialize CP grid
    my $grid = new_cp_grid();
    init_grid($grid, $puzzle);

    # Apply initial propagation
    $cp_iterations = 0;
    unless (propagate($grid)) {
        print "\nNo solution found (contradiction during initial propagation)\n\n";
        my $elapsed = time() - $start_time;
        printf "Seconds to process %.3f\n", $elapsed;
        exit 0;
    }

    # Run search
    my @solution = (0) x 81;
    my $solved = cp_search($grid, \@solution);

    if ($solved) {
        # Convert solution array back to 2D for printing
        my @solution_grid = map { [(0) x 9] } (0..8);
        for my $r (0..8) {
            for my $c (0..8) {
                $solution_grid[$r][$c] = $solution[$r * 9 + $c];
            }
        }

        print_puzzle(\@solution_grid);
        print "\nSolved in Iterations=$cp_iterations\n\n";
    } else {
        print "\nNo solution found\n\n";
    }
};
if ($@) {
    print STDERR "Error: $@\n";
    exit 1;
}

my $elapsed = time() - $start_time;
printf "Seconds to process %.3f\n", $elapsed;
