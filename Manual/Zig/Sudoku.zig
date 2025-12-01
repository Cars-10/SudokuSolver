const std = @import("std");

var puzzle: [9][9]u8 = undefined;
var iterations: u64 = 0;

fn printBoard() void {
    std.debug.print("Puzzle:\n", .{});
    for (0..9) |i| {
        for (0..9) |j| {
            std.debug.print("{d} ", .{puzzle[i][j]});
        }
        std.debug.print("\n", .{});
    }
}

fn isPossible(row: usize, col: usize, num: u8) bool {
    for (0..9) |i| {
        if (puzzle[row][i] == num or puzzle[i][col] == num) return false;
    }

    const startRow = (row / 3) * 3;
    const startCol = (col / 3) * 3;
    for (0..3) |i| {
        for (0..3) |j| {
            if (puzzle[startRow + i][startCol + j] == num) return false;
        }
    }
    return true;
}

fn solve(row: usize, col: usize) bool {
    if (row == 9) return true;

    var nextRow = row;
    var nextCol = col + 1;
    if (nextCol == 9) {
        nextRow = row + 1;
        nextCol = 0;
    }

    if (puzzle[row][col] != 0) {
        return solve(nextRow, nextCol);
    }

    var num: u8 = 1;
    while (num <= 9) : (num += 1) {
        iterations += 1;
        if (isPossible(row, col, num)) {
            puzzle[row][col] = num;
            if (solve(nextRow, nextCol)) return true;
            puzzle[row][col] = 0;
        }
    }
    return false;
}

fn readBoard(filename: []const u8) !bool {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const content = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(content);

    var row: usize = 0;
    var it_lines = std.mem.tokenizeAny(u8, content, "\n\r");
    while (it_lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t");
        if (trimmed.len > 0 and trimmed[0] != '#') {
            var it_parts = std.mem.tokenizeAny(u8, trimmed, " ");
            var col: usize = 0;
            while (it_parts.next()) |part| {
                if (col < 9) {
                    puzzle[row][col] = try std.fmt.parseInt(u8, part, 10);
                    col += 1;
                }
            }
            row += 1;
            if (row == 9) return true;
        }
    }
    return true;
}

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: ./Sudoku <file1> <file2> ...\n", .{});
        return;
    }

    for (args[1..]) |filename| {
        std.debug.print("\nProcessing {s}\n", .{filename});
        if (readBoard(filename)) |_| {
            printBoard();
            iterations = 0;
            if (solve(0, 0)) {
                printBoard();
                std.debug.print("\nSolved in Iterations={d}\n", .{iterations});
            } else {
                std.debug.print("No solution found\n", .{});
            }
        } else |err| {
            std.debug.print("Error reading file {s}: {any}\n", .{filename, err});
        }
    }
}
