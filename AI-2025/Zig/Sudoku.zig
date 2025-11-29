const std = @import("std");
const fs = std.fs;
const io = std.io;
const process = std.process;
const time = std.time;

var puzzle: [9][9]u8 = undefined;
var count: u64 = 0;

fn printPuzzle() void {
    const stdout = io.getStdOut().writer();
    stdout.print("\nPuzzle:\n", .{}) catch {};
    var j: usize = 0;
    while (j < 9) : (j += 1) {
        var i: usize = 0;
        while (i < 9) : (i += 1) {
            stdout.print("{d} ", .{puzzle[j][i]}) catch {};
        }
        stdout.print("\n", .{}) catch {};
    }
}

fn readMatrixFile(filename: []const u8) !void {
    const stdout = io.getStdOut().writer();
    try stdout.print("{s}\n", .{filename});

    const file = try fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var row: usize = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0 or line[0] == '#') continue;

        var it = std.mem.tokenize(u8, line, " \t\r");
        var col: usize = 0;
        while (it.next()) |part| {
            if (col < 9) {
                puzzle[row][col] = try std.fmt.parseInt(u8, part, 10);
                col += 1;
            }
        }
        if (col == 9) {
            row += 1;
            if (row == 9) break;
        }
    }
}

fn isPossible(y: usize, x: usize, val: u8) bool {
    var i: usize = 0;
    while (i < 9) : (i += 1) {
        if (puzzle[i][x] == val) return false;
        if (puzzle[y][i] == val) return false;
    }

    const x0 = (x / 3) * 3;
    const y0 = (y / 3) * 3;

    i = 0;
    while (i < 3) : (i += 1) {
        var j: usize = 0;
        while (j < 3) : (j += 1) {
            if (puzzle[y0 + i][x0 + j] == val) return false;
        }
    }
    return true;
}

fn solve() bool {
    var j: usize = 0;
    while (j < 9) : (j += 1) {
        var i: usize = 0;
        while (i < 9) : (i += 1) {
            if (puzzle[j][i] == 0) {
                var val: u8 = 1;
                while (val <= 9) : (val += 1) {
                    count += 1;
                    if (isPossible(j, i, val)) {
                        puzzle[j][i] = val;
                        if (solve()) return true;
                        puzzle[j][i] = 0;
                    }
                }
                return false;
            }
        }
    }
    printPuzzle();
    const stdout = io.getStdOut().writer();
    stdout.print("\nSolved in Iterations={d}\n\n", .{count}) catch {};
    return true;
}

pub fn main() !void {
    const stdout = io.getStdOut().writer();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    const start = time.nanoTimestamp();

    for (args[1..]) |arg| {
        if (std.mem.endsWith(u8, arg, ".matrix")) {
            try readMatrixFile(arg);
            printPuzzle();
            count = 0;
            _ = solve();
        }
    }

    const end = time.nanoTimestamp();
    const elapsed_s = @as(f64, @floatFromInt(end - start)) / 1_000_000_000.0;
    try stdout.print("Seconds to process {d:.3}\n", .{elapsed_s});
}
