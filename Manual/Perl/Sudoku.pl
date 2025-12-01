#!/usr/bin/perl
use strict;
use warnings;

my @puzzle;
my $iterations = 0;

sub print_board {
    print "Puzzle:\n";
    for my $row (0..8) {
        for my $col (0..8) {
            print "$puzzle[$row][$col] ";
        }
        print "\n";
    }
}

sub is_possible {
    my ($row, $col, $num) = @_;
    for my $i (0..8) {
        return 0 if $puzzle[$row][$i] == $num;
        return 0 if $puzzle[$i][$col] == $num;
    }
    my $start_row = int($row / 3) * 3;
    my $start_col = int($col / 3) * 3;
    for my $i (0..2) {
        for my $j (0..2) {
            return 0 if $puzzle[$start_row + $i][$start_col + $j] == $num;
        }
    }
    return 1;
}

sub solve {
    my ($row, $col) = @_;
    return 1 if $row == 9;

    my $next_row = $row;
    my $next_col = $col + 1;
    if ($next_col == 9) {
        $next_row = $row + 1;
        $next_col = 0;
    }

    if ($puzzle[$row][$col] != 0) {
        return solve($next_row, $next_col);
    }

    for my $num (1..9) {
        $iterations++;
        if (is_possible($row, $col, $num)) {
            $puzzle[$row][$col] = $num;
            return 1 if solve($next_row, $next_col);
            $puzzle[$row][$col] = 0;
        }
    }
    return 0;
}

sub read_board {
    my ($filename) = @_;
    @puzzle = ();
    open(my $fh, '<', $filename) or die "Could not open file '$filename' $!";
    my $row = 0;
    while (my $line = <$fh>) {
        chomp $line;
        next if $line =~ /^\s*$/ || $line =~ /^#/;
        my @parts = split(/\s+/, $line);
        my $col = 0;
        for my $part (@parts) {
            if ($col < 9) {
                $puzzle[$row][$col] = $part;
                $col++;
            }
        }
        $row++;
        last if $row == 9;
    }
    close $fh;
}

if (@ARGV == 0) {
    print "Usage: perl Sudoku.pl <file1> <file2> ...\n";
    exit 1;
}

for my $filename (@ARGV) {
    print "\nProcessing $filename\n";
    read_board($filename);
    print_board();
    $iterations = 0;
    if (solve(0, 0)) {
        print_board();
        print "\nSolved in Iterations=$iterations\n";
    } else {
        print "No solution found\n";
    }
}
