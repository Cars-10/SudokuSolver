#!/usr/bin/perl
use strict;
use warnings;
use Time::HiRes qw(gettimeofday tv_interval);

my @puzzle;
my $count = 0;
my $DEBUG = 0;

sub printPuzzle {
    print "\nPuzzle:\n";
    for my $j (0..8) {
        for my $i (0..8) {
            print "$puzzle[$j][$i] ";
        }
        print "\n";
    }
}

sub readMatrixFile {
    my ($filename) = @_;
    print "$filename\n";
    open(my $fh, '<', $filename) or die "Could not open file '$filename' $!";
    my $row = 0;
    while (my $line = <$fh>) {
        chomp $line;
        next if $line =~ /^#/;
        next if $line =~ /^\s*$/;
        
        my @parts = split(/\s+/, $line);
        # Filter out empty strings resulting from leading spaces
        @parts = grep { $_ ne '' } @parts;
        
        if (scalar @parts == 9) {
            for my $col (0..8) {
                $puzzle[$row][$col] = int($parts[$col]);
            }
            $row++;
            last if $row == 9;
        }
    }
    close $fh;
}

sub isPossible {
    my ($y, $x, $val) = @_;
    
    for my $i (0..8) {
        return 0 if $puzzle[$i][$x] == $val;
        return 0 if $puzzle[$y][$i] == $val;
    }
    
    my $x0 = int($x / 3) * 3;
    my $y0 = int($y / 3) * 3;
    
    for my $i (0..2) {
        for my $j (0..2) {
            return 0 if $puzzle[$y0 + $i][$x0 + $j] == $val;
        }
    }
    return 1;
}

sub solve {
    for my $j (0..8) {
        for my $i (0..8) {
            if ($puzzle[$j][$i] == 0) {
                for my $val (1..9) {
                    $count++;
                    if (isPossible($j, $i, $val)) {
                        $puzzle[$j][$i] = $val;
                        return 2 if solve() == 2;
                        $puzzle[$j][$i] = 0;
                    }
                }
                return 0;
            }
        }
    }
    printPuzzle();
    print "\nSolved in Iterations=$count\n\n";
    return 2;
}

my $start_time = [gettimeofday];

foreach my $arg (@ARGV) {
    if ($arg =~ /\.matrix$/) {
        readMatrixFile($arg);
        printPuzzle();
        $count = 0;
        solve();
    }
}

my $elapsed = tv_interval($start_time);
printf "Seconds to process %.3f\n", $elapsed;
