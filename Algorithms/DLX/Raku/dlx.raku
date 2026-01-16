#!/usr/bin/env raku
use v6;

# DLX Node class
class DlxNode {
    has DlxNode $.left is rw;
    has DlxNode $.right is rw;
    has DlxNode $.up is rw;
    has DlxNode $.down is rw;
    has DlxNode $.column is rw;
    has Int $.size is rw = 0;
    has Int $.row-id is rw = -1;
    has Int $.col-id is rw = -1;

    method initialize() {
        $!left = self;
        $!right = self;
        $!up = self;
        $!down = self;
        $!column = self;
    }
}

# Row metadata
class RowInfo {
    has Int $.row is rw = 0;
    has Int $.col is rw = 0;
    has Int $.num is rw = 0;
}

# DLX Matrix
class DlxMatrix {
    has DlxNode $.root;
    has DlxNode @.columns[324];
    has Int $.iterations is rw = 0;
    has RowInfo @.row-info[729];
    has DlxNode @.row-starts[729];

    method initialize() {
        # Create root
        $!root = DlxNode.new;
        $!root.initialize;

        # Create 324 column headers
        for ^324 -> $i {
            my $col = DlxNode.new;
            $col.initialize;
            $col.size = 0;
            $col.col-id = $i;

            # Link into header list
            $col.left = $!root.left;
            $col.right = $!root;
            $!root.left.right = $col;
            $!root.left = $col;

            @!columns[$i] = $col;
        }

        # Initialize row info
        for ^729 -> $i {
            @!row-info[$i] = RowInfo.new;
        }
    }

    method add-node(DlxNode $col, Int $row-id --> DlxNode) {
        my $node = DlxNode.new;
        $node.column = $col;
        $node.row-id = $row-id;

        # Insert at end of column's circular list
        $node.down = $col;
        $node.up = $col.up;
        $col.up.down = $node;
        $col.up = $node;
        $col.size++;

        return $node;
    }

    method cover-column(DlxNode $c) {
        # Remove column header from the header list
        $c.right.left = $c.left;
        $c.left.right = $c.right;

        # For each row in this column
        my $row-node = $c.down;
        while $row-node !=== $c {
            # For each node in this row
            my $right-node = $row-node.right;
            while $right-node !=== $row-node {
                # Remove this node from its column
                $right-node.down.up = $right-node.up;
                $right-node.up.down = $right-node.down;
                $right-node.column.size--;
                $right-node = $right-node.right;
            }
            $row-node = $row-node.down;
        }
    }

    method uncover-column(DlxNode $c) {
        # For each row in this column (in reverse order)
        my $row-node = $c.up;
        while $row-node !=== $c {
            # For each node in this row (in reverse order)
            my $left-node = $row-node.left;
            while $left-node !=== $row-node {
                # Restore this node to its column
                $left-node.column.size++;
                $left-node.down.up = $left-node;
                $left-node.up.down = $left-node;
                $left-node = $left-node.left;
            }
            $row-node = $row-node.up;
        }

        # Restore column header to the header list
        $c.right.left = $c;
        $c.left.right = $c;
    }

    method choose-column(--> DlxNode) {
        my $best = Nil;
        my $min-size = 999999;

        my $col-node = $!root.right;
        while $col-node !=== $!root {
            if $col-node.size < $min-size {
                $min-size = $col-node.size;
                $best = $col-node;
            }
            $col-node = $col-node.right;
        }

        return $best;
    }

    method dlx-search(Int $k, @solution --> Bool) {
        $!iterations++;

        # If matrix is empty, we found a solution
        return True if $!root.right === $!root;

        # Choose column with minimum size
        my $col = self.choose-column;
        return False if !$col || $col.size == 0;

        # Cover this column
        self.cover-column($col);

        # Try each row in this column
        my $row-node = $col.down;
        while $row-node !=== $col {
            # Add row to partial solution
            @solution[$k] = $row-node.row-id;

            # Cover all other columns in this row
            my $right-node = $row-node.right;
            while $right-node !=== $row-node {
                self.cover-column($right-node.column);
                $right-node = $right-node.right;
            }

            # Recurse
            if self.dlx-search($k + 1, @solution) {
                return True;
            }

            # Backtrack: uncover all columns in this row
            my $left-node = $row-node.left;
            while $left-node !=== $row-node {
                self.uncover-column($left-node.column);
                $left-node = $left-node.left;
            }

            $row-node = $row-node.down;
        }

        # Uncover column
        self.uncover-column($col);

        return False;
    }

    method build-dlx-row(Int $r, Int $c, Int $n, Int $row-id) {
        # Store row metadata
        @!row-info[$row-id].row = $r;
        @!row-info[$row-id].col = $c;
        @!row-info[$row-id].num = $n;

        # Calculate column indices
        my $pos-col = $r * 9 + $c;
        my $row-col = 81 + $r * 9 + ($n - 1);
        my $col-col = 162 + $c * 9 + ($n - 1);
        my $box-idx = ($r div 3) * 3 + ($c div 3);
        my $box-col = 243 + $box-idx * 9 + ($n - 1);

        # Create nodes for the 4 constraints
        my $n1 = self.add-node(@!columns[$pos-col], $row-id);
        my $n2 = self.add-node(@!columns[$row-col], $row-id);
        my $n3 = self.add-node(@!columns[$col-col], $row-id);
        my $n4 = self.add-node(@!columns[$box-col], $row-id);

        # Link nodes horizontally in circular list
        $n1.right = $n2;
        $n2.right = $n3;
        $n3.right = $n4;
        $n4.right = $n1;

        $n1.left = $n4;
        $n2.left = $n1;
        $n3.left = $n2;
        $n4.left = $n3;

        @!row-starts[$row-id] = $n1;
    }

    method build-matrix-from-puzzle(@puzzle) {
        my $row-id = 0;

        for ^9 -> $r {
            for ^9 -> $c {
                if @puzzle[$r][$c] != 0 {
                    # Cell has a clue - create only one row
                    self.build-dlx-row($r, $c, @puzzle[$r][$c], $row-id++);
                } else {
                    # Cell is empty - create rows for all possible values
                    for 1..9 -> $n {
                        self.build-dlx-row($r, $c, $n, $row-id++);
                    }
                }
            }
        }
    }

    method cover-clues(@puzzle) {
        for ^9 -> $r {
            for ^9 -> $c {
                if @puzzle[$r][$c] != 0 {
                    my $n = @puzzle[$r][$c];

                    # Find the row for this clue
                    for ^729 -> $row-id {
                        if @!row-starts[$row-id] &&
                           @!row-info[$row-id].row == $r &&
                           @!row-info[$row-id].col == $c &&
                           @!row-info[$row-id].num == $n {
                            
                            # Cover all columns in this row
                            my $start = @!row-starts[$row-id];
                            my $curr = $start;
                            loop {
                                self.cover-column($curr.column);
                                $curr = $curr.right;
                                last if $curr === $start;
                            }
                            last;
                        }
                    }
                }
            }
        }
    }

    method extract-solution(@solution, @puzzle --> Array) {
        my @result = @puzzle.map(*.clone);

        for ^81 -> $i {
            my $row-id = @solution[$i];
            if $row-id.defined && $row-id >= 0 && $row-id < 729 {
                my $info = @!row-info[$row-id];
                @result[$info.row][$info.col] = $info.num;
            }
        }

        return @result;
    }
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

        # Initialize DLX matrix
        my $matrix = DlxMatrix.new;
        $matrix.initialize;

        # Build matrix from puzzle
        $matrix.build-matrix-from-puzzle(@puzzle);

        # Cover pre-filled clues
        $matrix.cover-clues(@puzzle);

        # Solve using DLX
        $matrix.iterations = 0;
        my @solution = 0 xx 81;
        my $result = $matrix.dlx-search(0, @solution);

        if $result {
            my @solved = $matrix.extract-solution(@solution, @puzzle);
            print-puzzle(@solved);
            say "\nSolved in Iterations={$matrix.iterations}\n";
        } else {
            say "\nNo solution found\n";
        }
    }

    my $elapsed = now - $start-time;
    printf("Seconds to process %.3f\n", $elapsed);
}
