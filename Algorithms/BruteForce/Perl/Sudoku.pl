#!/usr/bin/perl
# Sudoku Solver - Perl Implementation
# Brute-force backtracking algorithm matching C reference exactly.
#
# Algorithm:
# - Row-major search for empty cells (top-to-bottom, left-to-right)
# - Try values 1-9 in ascending order
# - Count EVERY placement attempt (algorithm fingerprint)

use strict;
use warnings;
use Time::HiRes qw(time);

# Global puzzle grid [row][col]
my @puzzle;
my $count = 0;  # Iteration counter

sub print_puzzle {
    print "\nPuzzle:\n";
    for my $row (0..8) {
        print join(" ", @{$puzzle[$row]}) . " \n";
    }
}

sub read_matrix_file {
    my ($filename) = @_;

    # Normalize path for output (match C format)
    my $display_path = $filename;
    if ($filename =~ /^\/app\/Matrices\//) {
        $display_path = "../" . substr($filename, 5);  # Skip "/app/" to get "Matrices/..."
    }
    print "$display_path\n";

    open(my $fh, '<', $filename) or die "Cannot open $filename: $!";
    my $line_count = 0;

    while (my $line = <$fh>) {
        chomp $line;
        $line =~ s/^\s+|\s+$//g;  # Trim whitespace

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
}

sub is_valid {
    my ($row, $col, $val) = @_;

    # Check row
    for my $i (0..8) {
        return 0 if $puzzle[$row][$i] == $val;
    }

    # Check column
    for my $i (0..8) {
        return 0 if $puzzle[$i][$col] == $val;
    }

    # Check 3x3 box
    my $box_row = int($row / 3) * 3;
    my $box_col = int($col / 3) * 3;
    for my $i (0..2) {
        for my $j (0..2) {
            return 0 if $puzzle[$box_row + $i][$box_col + $j] == $val;
        }
    }

    return 1;
}

sub solve {
    # Find first empty cell (row-major order)
    my ($row, $col) = (-1, -1);
    OUTER: for my $r (0..8) {
        for my $c (0..8) {
            if ($puzzle[$r][$c] == 0) {
                $row = $r;
                $col = $c;
                last OUTER;
            }
        }
    }

    # If no empty cell found, puzzle is solved
    if ($row == -1) {
        print_puzzle();
        print "\nSolved in Iterations=$count\n\n";
        return 1;
    }

    # Try values 1-9 in order
    for my $val (1..9) {
        $count++;  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if (is_valid($row, $col, $val)) {
            $puzzle[$row][$col] = $val;  # Place value

            if (solve()) {
                return 1;
            }

            $puzzle[$row][$col] = 0;  # Backtrack
        }
    }

    return 0;
}

# Main program - process each .matrix file from command line
my $start_time = time();

for my $arg (@ARGV) {
    next unless $arg =~ /\.matrix$/;

    # Reset puzzle
    @puzzle = map { [(0) x 9] } (0..8);

    read_matrix_file($arg);
    print_puzzle();
    $count = 0;
    solve();
}

my $elapsed = time() - $start_time;
printf "Seconds to process %.3f\n", $elapsed;
