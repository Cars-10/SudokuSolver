#!/usr/bin/env perl
# Dancing Links (DLX) Sudoku Solver - Perl Implementation
# Mechanical translation from C reference to preserve algorithm correctness.
#
# Algorithm: Knuth's Algorithm X with Dancing Links data structure
# - Exact cover problem: 324 constraints (row-col, row-num, col-num, box-num)
# - Each Sudoku placement satisfies exactly 4 constraints
# - Doubly-linked circular lists for efficient cover/uncover operations

use strict;
use warnings;
use Time::HiRes qw(time);

# Global iteration counter (analogous to brute-force count)
my $dlx_iterations = 0;

# DLX matrix structures (using hash references as objects)
my $root;                   # Root column
my @columns;                # 324 constraint columns
my @nodes;                  # Node pool
my @row_info;               # 729 possible rows (9x9x9) - stores {row, col, num}
my @row_starts;             # Pointer to first node in each row

# Sudoku puzzle grid [row][col]
my @puzzle = map { [(0) x 9] } (0..8);
my @solution_grid = map { [(0) x 9] } (0..8);

# Create a DlxNode (hash reference)
sub new_dlx_node {
    return {
        left => undef,
        right => undef,
        up => undef,
        down => undef,
        column => undef,
        row_id => -1
    };
}

# Create a DlxColumn (hash reference with embedded node)
sub new_dlx_column {
    my ($name) = @_;
    my $node = new_dlx_node();
    my $col = {
        node => $node,
        size => 0,
        name => $name
    };
    $node->{column} = $col;  # Column nodes point to themselves
    return $col;
}

# Constraint column mapping functions
sub get_position_col { my ($r, $c) = @_; return $r * 9 + $c; }
sub get_row_col { my ($r, $n) = @_; return 81 + $r * 9 + ($n - 1); }
sub get_col_col { my ($c, $n) = @_; return 162 + $c * 9 + ($n - 1); }
sub get_box_col {
    my ($r, $c, $n) = @_;
    my $box = int($r / 3) * 3 + int($c / 3);
    return 243 + $box * 9 + ($n - 1);
}

# Cover a column in the DLX matrix
sub dlx_cover_column {
    my ($col) = @_;
    my $col_node = $col->{node};

    # Remove column header from the header list
    $col_node->{right}->{left} = $col_node->{left};
    $col_node->{left}->{right} = $col_node->{right};

    # For each row in this column
    my $row_node = $col_node->{down};
    while ($row_node != $col_node) {
        # For each node in this row (excluding the column itself)
        my $right_node = $row_node->{right};
        while ($right_node != $row_node) {
            # Remove this node from its column
            $right_node->{down}->{up} = $right_node->{up};
            $right_node->{up}->{down} = $right_node->{down};
            $right_node->{column}->{size}--;
            $right_node = $right_node->{right};
        }
        $row_node = $row_node->{down};
    }
}

# Uncover a column (exact reverse of cover)
sub dlx_uncover_column {
    my ($col) = @_;
    my $col_node = $col->{node};

    # For each row in this column (in reverse order)
    my $row_node = $col_node->{up};
    while ($row_node != $col_node) {
        # For each node in this row (in reverse order)
        my $left_node = $row_node->{left};
        while ($left_node != $row_node) {
            # Restore this node to its column
            $left_node->{column}->{size}++;
            $left_node->{down}->{up} = $left_node;
            $left_node->{up}->{down} = $left_node;
            $left_node = $left_node->{left};
        }
        $row_node = $row_node->{up};
    }

    # Restore column header to the header list
    $col_node->{right}->{left} = $col_node;
    $col_node->{left}->{right} = $col_node;
}

# Choose column with minimum size (Knuth's S heuristic)
sub choose_column {
    my ($root_col) = @_;
    my $root_node = $root_col->{node};
    my $best = undef;
    my $min_size = 999999;

    my $col_node = $root_node->{right};
    while ($col_node != $root_node) {
        my $col = $col_node->{column};
        if ($col->{size} < $min_size) {
            $min_size = $col->{size};
            $best = $col;
        }
        $col_node = $col_node->{right};
    }

    return $best;
}

# DLX Search - Algorithm X with Dancing Links
sub dlx_search {
    my ($root_col, $k, $solution) = @_;
    $dlx_iterations++;  # Count every search call

    my $root_node = $root_col->{node};

    # If matrix is empty, we found a solution
    if ($root_node->{right} == $root_node) {
        return 1;
    }

    # Choose column with minimum size
    my $col = choose_column($root_col);

    # If column has no rows, no solution possible
    if ($col->{size} == 0) {
        return 0;
    }

    # Cover this column
    dlx_cover_column($col);

    # Try each row in this column
    my $row_node = $col->{node}->{down};
    while ($row_node != $col->{node}) {
        # Add row to partial solution
        $solution->[$k] = $row_node->{row_id};

        # Cover all other columns in this row
        my $right_node = $row_node->{right};
        while ($right_node != $row_node) {
            dlx_cover_column($right_node->{column});
            $right_node = $right_node->{right};
        }

        # Recurse
        if (dlx_search($root_col, $k + 1, $solution)) {
            return 1;  # Solution found
        }

        # Backtrack: uncover all columns in this row
        my $left_node = $row_node->{left};
        while ($left_node != $row_node) {
            dlx_uncover_column($left_node->{column});
            $left_node = $left_node->{left};
        }

        $row_node = $row_node->{down};
    }

    # Uncover column
    dlx_uncover_column($col);

    return 0;  # No solution found
}

# Initialize DLX matrix structure
sub init_dlx_matrix {
    # Allocate root column
    $root = new_dlx_column("root");
    $root->{node}->{left} = $root->{node};
    $root->{node}->{right} = $root->{node};
    $root->{node}->{up} = $root->{node};
    $root->{node}->{down} = $root->{node};
    $root->{node}->{row_id} = -1;

    # Allocate 324 column headers
    @columns = ();
    for my $i (0..323) {
        my $col = new_dlx_column("C$i");

        # Initialize as circular list
        $col->{node}->{up} = $col->{node};
        $col->{node}->{down} = $col->{node};
        $col->{node}->{row_id} = -1;

        # Link into header list
        $col->{node}->{left} = $root->{node}->{left};
        $col->{node}->{right} = $root->{node};
        $root->{node}->{left}->{right} = $col->{node};
        $root->{node}->{left} = $col->{node};

        push @columns, $col;
    }

    # Initialize node pool, row info, and row starts
    @nodes = ();
    @row_info = map { {row => 0, col => 0, num => 0} } (0..728);
    @row_starts = map { undef } (0..728);
}

# Add a node to the DLX matrix
sub add_node {
    my ($col, $row_id) = @_;
    my $node = new_dlx_node();
    $node->{column} = $col;
    $node->{row_id} = $row_id;

    # Insert at end of column's circular list
    $node->{down} = $col->{node};
    $node->{up} = $col->{node}->{up};
    $col->{node}->{up}->{down} = $node;
    $col->{node}->{up} = $node;
    $col->{size}++;

    push @nodes, $node;
    return $node;
}

# Build a DLX row for Sudoku cell (r,c) with value n
sub build_dlx_row {
    my ($r, $c, $n, $row_id) = @_;

    # Store row metadata
    $row_info[$row_id] = {row => $r, col => $c, num => $n};

    # Create nodes for the 4 constraints
    my $n1 = add_node($columns[get_position_col($r, $c)], $row_id);
    my $n2 = add_node($columns[get_row_col($r, $n)], $row_id);
    my $n3 = add_node($columns[get_col_col($c, $n)], $row_id);
    my $n4 = add_node($columns[get_box_col($r, $c, $n)], $row_id);

    # Link nodes horizontally in circular list
    $n1->{right} = $n2;
    $n2->{right} = $n3;
    $n3->{right} = $n4;
    $n4->{right} = $n1;

    $n1->{left} = $n4;
    $n2->{left} = $n1;
    $n3->{left} = $n2;
    $n4->{left} = $n3;

    # Store first node for this row
    $row_starts[$row_id] = $n1;
}

# Build the complete DLX matrix from the puzzle
sub build_dlx_matrix_from_puzzle {
    my $row_id = 0;

    for my $r (0..8) {
        for my $c (0..8) {
            if ($puzzle[$r][$c] != 0) {
                # Cell has a clue - create only one row for that value
                build_dlx_row($r, $c, $puzzle[$r][$c], $row_id);
                $row_id++;
            } else {
                # Cell is empty - create rows for all possible values
                for my $n (1..9) {
                    build_dlx_row($r, $c, $n, $row_id);
                    $row_id++;
                }
            }
        }
    }
}

# Cover given clues (pre-selected rows)
sub cover_clues {
    for my $r (0..8) {
        for my $c (0..8) {
            if ($puzzle[$r][$c] != 0) {
                my $n = $puzzle[$r][$c];

                # Find the row for this clue
                for my $row_id (0..728) {
                    if (defined $row_starts[$row_id] &&
                        $row_info[$row_id]->{row} == $r &&
                        $row_info[$row_id]->{col} == $c &&
                        $row_info[$row_id]->{num} == $n) {

                        # Cover all columns in this row
                        my $node = $row_starts[$row_id];
                        my $curr = $node;
                        while (1) {
                            dlx_cover_column($curr->{column});
                            $curr = $curr->{right};
                            last if $curr == $node;
                        }
                        last;
                    }
                }
            }
        }
    }
}

# Extract solution from DLX and populate solution_grid
sub extract_solution {
    my ($solution, $solution_len) = @_;

    # Initialize solution grid - start with the original puzzle (includes clues)
    for my $r (0..8) {
        for my $c (0..8) {
            $solution_grid[$r][$c] = $puzzle[$r][$c];
        }
    }

    # Each solution entry is a row_id
    for my $i (0..$solution_len-1) {
        my $row_id = $solution->[$i];
        if ($row_id >= 0 && $row_id < 729) {
            my $info = $row_info[$row_id];
            $solution_grid[$info->{row}][$info->{col}] = $info->{num};
        }
    }
}

# Print puzzle
sub print_puzzle {
    my ($grid) = @_;
    print "\nPuzzle:\n";
    for my $r (0..8) {
        print join(" ", @{$grid->[$r]}) . " \n";
    }
}

# Read matrix file
sub read_matrix_file {
    my ($filename) = @_;

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
}

# Main program - process each .matrix file from command line
my $start_time = time();

for my $arg (@ARGV) {
    next unless $arg =~ /\.matrix$/;

    # Reset puzzle
    @puzzle = map { [(0) x 9] } (0..8);

    eval {
        read_matrix_file($arg);
        print_puzzle(\@puzzle);

        # Initialize DLX matrix
        init_dlx_matrix();

        # Build matrix from puzzle
        build_dlx_matrix_from_puzzle();

        # Cover pre-filled clues
        cover_clues();

        # Solve using DLX
        $dlx_iterations = 0;
        my @solution = map { -1 } (0..80);
        my $result = dlx_search($root, 0, \@solution);

        if ($result) {
            extract_solution(\@solution, 81);
            print_puzzle(\@solution_grid);
            print "\nSolved in Iterations=$dlx_iterations\n\n";
        } else {
            print "\nNo solution found\n\n";
        }
    };
    if ($@) {
        print STDERR "Error processing $arg: $@\n";
    }
}

my $elapsed = time() - $start_time;
printf "Seconds to process %.3f\n", $elapsed;
