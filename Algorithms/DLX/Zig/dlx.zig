const std = @import("std");

// DLX node structure
const DlxNode = struct {
    left: ?*DlxNode,
    right: ?*DlxNode,
    up: ?*DlxNode,
    down: ?*DlxNode,
    column: ?*DlxNode,
    size: usize,
    row_id: i32,
    col_id: i32,
};

var dlx_iterations: usize = 0;

fn coverColumn(c: *DlxNode) void {
    // Remove column from header list
    c.right.?.*.left = c.left;
    c.left.?.*.right = c.right;

    // For each row in this column
    var row_node = c.down;
    while (row_node != null and row_node.? != c) {
        // For each node in this row
        var right_node = row_node.?.*.right;
        while (right_node != null and right_node.? != row_node.?) {
            // Remove from column
            right_node.?.*.down.?.*.up = right_node.?.*.up;
            right_node.?.*.up.?.*.down = right_node.?.*.down;
            right_node.?.*.column.?.*.size -= 1;
            right_node = right_node.?.*.right;
        }
        row_node = row_node.?.*.down;
    }
}

fn uncoverColumn(c: *DlxNode) void {
    // For each row in this column (in reverse order)
    var row_node = c.up;
    while (row_node != null and row_node.? != c) {
        // For each node in this row (in reverse order)
        var left_node = row_node.?.*.left;
        while (left_node != null and left_node.? != row_node.?) {
            // Restore to column
            left_node.?.*.column.?.*.size += 1;
            left_node.?.*.down.?.*.up = left_node;
            left_node.?.*.up.?.*.down = left_node;
            left_node = left_node.?.*.left;
        }
        row_node = row_node.?.*.up;
    }

    // Restore column to header list
    c.right.?.*.left = c;
    c.left.?.*.right = c;
}

fn chooseColumn(root: *DlxNode) ?*DlxNode {
    var best: ?*DlxNode = null;
    var min_size: usize = std.math.maxInt(usize);

    var col_node = root.right;
    while (col_node != null and col_node.? != root) {
        if (col_node.?.*.size < min_size) {
            min_size = col_node.?.*.size;
            best = col_node;
        }
        col_node = col_node.?.*.right;
    }

    return best;
}

fn dlxSearch(root: *DlxNode, k: usize, solution: []i32) bool {
    dlx_iterations += 1;

    // If matrix is empty, we found a solution
    if (root.right == null or root.right.? == root) {
        return true;
    }

    // Choose column with minimum size
    const col = chooseColumn(root) orelse return false;

    // If column has no rows, no solution possible
    if (col.size == 0) {
        return false;
    }

    // Cover this column
    coverColumn(col);

    // Try each row in this column
    var row_node = col.down;
    while (row_node != null and row_node.? != col) {
        // Add row to partial solution
        solution[k] = row_node.?.*.row_id;

        // Cover all other columns in this row
        var right_node = row_node.?.*.right;
        while (right_node != null and right_node.? != row_node.?) {
            if (right_node.?.*.column) |col_to_cover| {
                coverColumn(col_to_cover);
            }
            right_node = right_node.?.*.right;
        }

        // Recurse
        if (dlxSearch(root, k + 1, solution)) {
            return true;
        }

        // Backtrack: uncover all columns in this row
        var left_node = row_node.?.*.left;
        while (left_node != null and left_node.? != row_node.?) {
            if (left_node.?.*.column) |col_to_uncover| {
                uncoverColumn(col_to_uncover);
            }
            left_node = left_node.?.*.left;
        }

        row_node = row_node.?.*.down;
    }

    // Uncover column
    uncoverColumn(col);

    return false;
}

fn createNode(allocator: std.mem.Allocator) !*DlxNode {
    const node = try allocator.create(DlxNode);
    node.* = DlxNode{
        .left = null,
        .right = null,
        .up = null,
        .down = null,
        .column = null,
        .size = 0,
        .row_id = -1,
        .col_id = -1,
    };
    return node;
}

const MatrixResult = struct {
    root: *DlxNode,
    row_info: [729]RowInfo,
    row_starts: [729]?*DlxNode,
};

const RowInfo = struct {
    row: i32,
    col: i32,
    num: i32,
};

fn buildSudokuMatrix(allocator: std.mem.Allocator, puzzle: [9][9]i32) !MatrixResult {
    // Create root node
    const root = try createNode(allocator);
    root.left = root;
    root.right = root;
    root.up = root;
    root.down = root;

    // Create column headers (324 constraints: 81 cell + 81 row + 81 col + 81 box)
    var columns: [324]*DlxNode = undefined;
    for (0..324) |i| {
        const col = try createNode(allocator);
        col.size = 0;
        col.col_id = @as(i32, @intCast(i));
        col.up = col;
        col.down = col;
        col.column = col;

        // Add to header list
        col.left = root.left;
        col.right = root;
        root.left.?.*.right = col;
        root.left = col;

        columns[i] = col;
    }

    // Track row metadata
    var row_info: [729]RowInfo = undefined;
    var row_starts: [729]?*DlxNode = undefined;
    for (0..729) |i| {
        row_starts[i] = null;
    }
    var row_id: usize = 0;

    // Add rows to the matrix
    for (0..9) |row| {
        for (0..9) |col| {
            const val = puzzle[row][col];
            if (val == 0) {
                // Empty cell: add rows for all possible digits
                for (1..10) |digit| {
                    row_starts[row_id] = try addRow(allocator, &columns, &row_info, row_id, @as(i32, @intCast(row)), @as(i32, @intCast(col)), @as(i32, @intCast(digit)));
                    row_id += 1;
                }
            } else {
                // Fixed cell: add only one row
                row_starts[row_id] = try addRow(allocator, &columns, &row_info, row_id, @as(i32, @intCast(row)), @as(i32, @intCast(col)), val);
                row_id += 1;
            }
        }
    }

    return MatrixResult{
        .root = root,
        .row_info = row_info,
        .row_starts = row_starts,
    };
}

fn addRow(allocator: std.mem.Allocator, columns: *[324]*DlxNode, row_info: *[729]RowInfo, row_id: usize, row: i32, col: i32, digit: i32) !*DlxNode {
    // Store row metadata
    row_info[row_id] = RowInfo{
        .row = row,
        .col = col,
        .num = digit,
    };

    // Calculate constraint column indices
    const cell_idx = row * 9 + col;
    const row_idx = 81 + row * 9 + (digit - 1);
    const col_idx = 162 + col * 9 + (digit - 1);
    const box_idx = 243 + ((@divFloor(row, 3) * 3 + @divFloor(col, 3)) * 9 + (digit - 1));

    const indices = [4]usize{
        @as(usize, @intCast(cell_idx)),
        @as(usize, @intCast(row_idx)),
        @as(usize, @intCast(col_idx)),
        @as(usize, @intCast(box_idx)),
    };

    var first_node: ?*DlxNode = null;
    var prev_node: ?*DlxNode = null;

    // Create nodes for each constraint
    for (indices) |idx| {
        const node = try createNode(allocator);
        node.row_id = @as(i32, @intCast(row_id));
        node.col_id = @as(i32, @intCast(idx));
        node.column = columns[idx];

        // Add to column list
        node.up = columns[idx].up;
        node.down = columns[idx];
        columns[idx].up.?.*.down = node;
        columns[idx].up = node;
        columns[idx].size += 1;

        // Link horizontally
        if (first_node == null) {
            first_node = node;
            node.left = node;
            node.right = node;
        } else {
            node.left = prev_node;
            node.right = first_node;
            prev_node.?.*.right = node;
            first_node.?.*.left = node;
        }
        prev_node = node;
    }

    return first_node.?;
}

fn coverClues(puzzle: [9][9]i32, row_info: [729]RowInfo, row_starts: [729]?*DlxNode) void {
    for (0..9) |r| {
        for (0..9) |c| {
            if (puzzle[r][c] != 0) {
                const n = puzzle[r][c];
                // Find the row for this clue
                for (0..729) |row_id| {
                    if (row_starts[row_id]) |start_node| {
                        if (row_info[row_id].row == @as(i32, @intCast(r)) and
                            row_info[row_id].col == @as(i32, @intCast(c)) and
                            row_info[row_id].num == n)
                        {
                            // Cover all columns in this row
                            var curr = start_node;
                            var first = true;
                            while (first or curr != start_node) {
                                first = false;
                                if (curr.column) |col| {
                                    coverColumn(col);
                                }
                                curr = curr.right.?;
                            }
                            break;
                        }
                    }
                }
            }
        }
    }
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

fn extractSolution(solution: []i32, k: usize, puzzle: [9][9]i32, row_info: [729]RowInfo) [9][9]i32 {
    var grid = puzzle; // Start with the original puzzle (includes clues)

    for (0..k) |i| {
        const row_id = solution[i];
        if (row_id >= 0 and row_id < 729) {
            const info = row_info[@as(usize, @intCast(row_id))];
            grid[@as(usize, @intCast(info.row))][@as(usize, @intCast(info.col))] = info.num;
        }
    }

    return grid;
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

    // Build DLX matrix
    const matrix = try buildSudokuMatrix(allocator, puzzle);

    // Cover pre-filled clues
    coverClues(puzzle, matrix.row_info, matrix.row_starts);

    // Solve
    var solution: [81]i32 = undefined;
    dlx_iterations = 0;

    if (dlxSearch(matrix.root, 0, &solution)) {
        const solved = extractSolution(&solution, 81, puzzle, matrix.row_info);
        printGrid(solved);
        printToStdout("Solved in Iterations={d}\n", .{dlx_iterations});
    } else {
        printToStdout("No solution found.\n", .{});
    }
}
