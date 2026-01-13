const std = @import("std");

const CandidateSet = u16;

const CPGrid = struct {
    values: [9][9]i32,
    candidates: [9][9]CandidateSet,
};

var cp_iterations: i64 = 0;

fn hasCand(cs: CandidateSet, digit: i32) bool {
    return (cs & (@as(u16, 1) << @intCast(digit))) != 0;
}

fn countCand(cs: CandidateSet) usize {
    return @popCount(cs);
}

fn getPeers(row: usize, col: usize) [20][2]usize {
    var peers: [20][2]usize = undefined;
    var idx: usize = 0;

    // Same row (8 cells excluding self)
    for (0..9) |c| {
        if (c != col) {
            peers[idx] = [2]usize{ row, c };
            idx += 1;
        }
    }

    // Same column (8 cells excluding self)
    for (0..9) |r| {
        if (r != row) {
            peers[idx] = [2]usize{ r, col };
            idx += 1;
        }
    }

    // Same 3x3 box (4 cells not already counted)
    const box_row = (row / 3) * 3;
    const box_col = (col / 3) * 3;
    for (0..3) |br| {
        for (0..3) |bc| {
            const r = box_row + br;
            const c = box_col + bc;
            if (r != row and c != col) {
                peers[idx] = [2]usize{ r, c };
                idx += 1;
            }
        }
    }

    return peers;
}

fn initGrid(puzzle: [9][9]i32) CPGrid {
    var grid: CPGrid = undefined;

    for (0..9) |row| {
        for (0..9) |col| {
            if (puzzle[row][col] == 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid.values[row][col] = 0;
                grid.candidates[row][col] = 0x3FE; // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                const digit = puzzle[row][col];
                grid.values[row][col] = digit;
                grid.candidates[row][col] = @as(u16, 1) << @intCast(digit);
            }
        }
    }

    return grid;
}

fn eliminate(grid: *CPGrid, row: usize, col: usize, digit: i32) bool {
    // Check if digit is already eliminated
    if (!hasCand(grid.candidates[row][col], digit)) {
        return true; // Already eliminated, no change
    }

    // Remove digit from candidates
    grid.candidates[row][col] &= ~(@as(u16, 1) << @intCast(digit));

    // Check for contradiction (no candidates left)
    const remaining = countCand(grid.candidates[row][col]);
    if (remaining == 0) {
        return false; // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 and grid.values[row][col] == 0) {
        // Find the last remaining digit
        for (1..10) |d| {
            if (hasCand(grid.candidates[row][col], @as(i32, @intCast(d)))) {
                if (!assign(grid, row, col, @as(i32, @intCast(d)))) {
                    return false; // Assignment caused contradiction
                }
                break;
            }
        }
    }

    return true;
}

fn assign(grid: *CPGrid, row: usize, col: usize, digit: i32) bool {
    // Increment iteration counter (this is our benchmark metric)
    cp_iterations += 1;

    // Set value
    grid.values[row][col] = digit;
    grid.candidates[row][col] = @as(u16, 1) << @intCast(digit);

    // Eliminate digit from all peers
    const peers = getPeers(row, col);

    for (peers) |peer| {
        const peer_row = peer[0];
        const peer_col = peer[1];

        if (!eliminate(grid, peer_row, peer_col, digit)) {
            return false; // Contradiction in peer elimination
        }
    }

    return true;
}

fn propagate(grid: *CPGrid) bool {
    var changed: bool = true;

    while (changed) {
        changed = false;

        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for (0..9) |row| {
            for (0..9) |col| {
                if (grid.values[row][col] == 0) {
                    const num_candidates = countCand(grid.candidates[row][col]);
                    if (num_candidates == 0) {
                        return false; // Contradiction
                    }
                    if (num_candidates == 1) {
                        for (1..10) |digit| {
                            if (hasCand(grid.candidates[row][col], @as(i32, @intCast(digit)))) {
                                if (!assign(grid, row, col, @as(i32, @intCast(digit)))) {
                                    return false; // Assignment caused contradiction
                                }
                                changed = true;
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - Check rows
        for (0..9) |row| {
            for (1..10) |digit| {
                var count: usize = 0;
                var last_col: usize = 0;
                var already_assigned: bool = false;

                for (0..9) |col| {
                    if (grid.values[row][col] == @as(i32, @intCast(digit))) {
                        already_assigned = true;
                        break;
                    }
                    if (hasCand(grid.candidates[row][col], @as(i32, @intCast(digit)))) {
                        count += 1;
                        last_col = col;
                    }
                }

                if (!already_assigned) {
                    if (count == 1) {
                        if (!assign(grid, row, last_col, @as(i32, @intCast(digit)))) {
                            return false;
                        }
                        changed = true;
                    } else if (count == 0) {
                        return false; // Digit cannot be placed anywhere in row
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - Check columns
        for (0..9) |col| {
            for (1..10) |digit| {
                var count: usize = 0;
                var last_row: usize = 0;
                var already_assigned: bool = false;

                for (0..9) |row| {
                    if (grid.values[row][col] == @as(i32, @intCast(digit))) {
                        already_assigned = true;
                        break;
                    }
                    if (hasCand(grid.candidates[row][col], @as(i32, @intCast(digit)))) {
                        count += 1;
                        last_row = row;
                    }
                }

                if (!already_assigned) {
                    if (count == 1) {
                        if (!assign(grid, last_row, col, @as(i32, @intCast(digit)))) {
                            return false;
                        }
                        changed = true;
                    } else if (count == 0) {
                        return false; // Digit cannot be placed anywhere in column
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - Check boxes
        for (0..9) |box| {
            const box_row = (box / 3) * 3;
            const box_col = (box % 3) * 3;

            for (1..10) |digit| {
                var count: usize = 0;
                var last_r: usize = 0;
                var last_c: usize = 0;
                var already_assigned: bool = false;

                outer: for (0..3) |br| {
                    for (0..3) |bc| {
                        const r = box_row + br;
                        const c = box_col + bc;
                        if (grid.values[r][c] == @as(i32, @intCast(digit))) {
                            already_assigned = true;
                            break :outer;
                        }
                        if (hasCand(grid.candidates[r][c], @as(i32, @intCast(digit)))) {
                            count += 1;
                            last_r = r;
                            last_c = c;
                        }
                    }
                }

                if (!already_assigned) {
                    if (count == 1) {
                        if (!assign(grid, last_r, last_c, @as(i32, @intCast(digit)))) {
                            return false;
                        }
                        changed = true;
                    } else if (count == 0) {
                        return false; // Digit cannot be placed anywhere in box
                    }
                }
            }
        }
    }

    return true; // Success - reached fixpoint
}

fn findMrvCell(grid: *CPGrid) ?[2]usize {
    var min_candidates: usize = 10; // More than 9, so any cell will be smaller
    var found: bool = false;
    var result: [2]usize = undefined;

    for (0..9) |r| {
        for (0..9) |c| {
            if (grid.values[r][c] == 0) {
                const num_candidates = countCand(grid.candidates[r][c]);
                if (num_candidates < min_candidates) {
                    min_candidates = num_candidates;
                    result = [2]usize{ r, c };
                    found = true;
                }
            }
        }
    }

    if (found) {
        return result;
    } else {
        return null;
    }
}

fn cpSearch(grid: *CPGrid, solution: *[81]i32) bool {
    // Base case: check if grid is complete
    const mrv_cell = findMrvCell(grid);
    if (mrv_cell == null) {
        // No empty cells - grid is complete, extract solution
        for (0..9) |r| {
            for (0..9) |c| {
                solution[r * 9 + c] = grid.values[r][c];
            }
        }
        return true; // Solved
    }

    const mrv_row = mrv_cell.?[0];
    const mrv_col = mrv_cell.?[1];

    // Recursive case: try each candidate for the MRV cell
    const candidates = grid.candidates[mrv_row][mrv_col];

    for (1..10) |digit| {
        if (hasCand(candidates, @as(i32, @intCast(digit)))) {
            // Save grid state for backtracking
            const grid_copy = grid.*;

            // Try assigning this digit
            if (assign(grid, mrv_row, mrv_col, @as(i32, @intCast(digit)))) {
                // Assignment succeeded, propagate constraints
                if (propagate(grid)) {
                    // Propagation succeeded, recurse
                    if (cpSearch(grid, solution)) {
                        return true; // Found solution
                    }
                }
            }

            // Failed - restore grid state and try next candidate
            grid.* = grid_copy;
        }
    }

    // All candidates exhausted - dead end
    return false;
}

fn parseMatrixFile(allocator: std.mem.Allocator, filename: []const u8) ![9][9]i32 {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var puzzle: [9][9]i32 = undefined;
    var row_idx: usize = 0;

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (line[0] < '0' or line[0] > '9') continue;

        var col_idx: usize = 0;
        var it = std.mem.tokenizeAny(u8, line, " \r\t");
        while (it.next()) |token| {
            if (col_idx < 9) {
                puzzle[row_idx][col_idx] = try std.fmt.parseInt(i32, token, 10);
                col_idx += 1;
            }
        }
        if (col_idx > 0) {
            row_idx += 1;
        }
        if (row_idx >= 9) break;
    }

    return puzzle;
}

fn printToStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const slice = std.fmt.bufPrint(&buf, fmt, args) catch return;
    _ = std.posix.write(1, slice) catch {};
}

fn printGrid(grid: [9][9]i32) void {
    for (grid) |row| {
        for (row, 0..) |cell, i| {
            printToStdout("{d}", .{cell});
            if (i < 8) printToStdout(" ", .{});
        }
        printToStdout("\n", .{});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printToStdout("Usage: {s} <matrix_file>\n", .{args[0]});
        return;
    }

    // Parse input puzzle
    const puzzle = try parseMatrixFile(allocator, args[1]);

    // Initialize grid with candidates
    var grid = initGrid(puzzle);

    // Propagate initial constraints from given clues
    if (!propagate(&grid)) {
        printToStdout("Initial propagation failed - puzzle has no solution.\n", .{});
        return;
    }

    // Solve with search
    var solution: [81]i32 = undefined;
    cp_iterations = 0;

    if (cpSearch(&grid, &solution)) {
        // Extract solution into 2D grid
        var solved: [9][9]i32 = undefined;
        for (0..9) |r| {
            for (0..9) |c| {
                solved[r][c] = solution[r * 9 + c];
            }
        }
        printGrid(solved);
        printToStdout("Solved in Iterations={d}\n", .{cp_iterations});
    } else {
        printToStdout("No solution found.\n", .{});
    }
}
