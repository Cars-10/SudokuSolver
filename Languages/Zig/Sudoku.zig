const std = @import("std");

const Grid = [9][9]u8;
var iterations: u64 = 0;

fn printToStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const slice = std.fmt.bufPrint(&buf, fmt, args) catch return;
    _ = std.posix.write(1, slice) catch {};
}

fn printGrid(grid: Grid) void {
    for (grid) |row| {
        for (row, 0..) |cell, i| {
            printToStdout("{d}", .{cell});
            if (i < 8) printToStdout(" ", .{});
        }
        printToStdout("\n", .{});
    }
}

fn isValid(grid: *Grid, row: usize, col: usize, num: u8) bool {
    // Check row and column
    for (0..9) |i| {
        if (grid[row][i] == num) return false;
        if (grid[i][col] == num) return false;
    }

    // Check 3x3 box
    const startRow = row - (row % 3);
    const startCol = col - (col % 3);
    for (0..3) |i| {
        for (0..3) |j| {
            if (grid[startRow + i][startCol + j] == num) return false;
        }
    }

    return true;
}

fn solve(grid: *Grid) bool {
    var row: usize = 0;
    var col: usize = 0;
    var isEmpty = false;

    // Find empty cell (row-major order)
    outer: for (0..9) |r| {
        for (0..9) |c| {
            if (grid[r][c] == 0) {
                row = r;
                col = c;
                isEmpty = true;
                break :outer;
            }
        }
    }

    if (!isEmpty) return true;

    // Try values 1-9 in order
    for (1..10) |num| {
        iterations += 1; // COUNT EVERY ATTEMPT - algorithm fingerprint
        const n = @as(u8, @intCast(num));
        if (isValid(grid, row, col, n)) {
            grid[row][col] = n;
            if (solve(grid)) return true;
            grid[row][col] = 0;
        }
    }

    return false;
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

    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var grid: Grid = undefined;
    var row: usize = 0;

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (line[0] < '0' or line[0] > '9') continue;

        var col: usize = 0;
        var it = std.mem.tokenizeAny(u8, line, " \r\t");
        while (it.next()) |token| {
            if (col < 9) {
                grid[row][col] = try std.fmt.parseInt(u8, token, 10);
                col += 1;
            }
        }
        if (col > 0) row += 1;
        if (row >= 9) break;
    }

    if (solve(&grid)) {
        printGrid(grid);
        printToStdout("Solved in Iterations={d}\n", .{iterations});
    } else {
        printToStdout("No solution found.\n", .{});
    }
}
