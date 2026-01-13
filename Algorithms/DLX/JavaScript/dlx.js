#!/usr/bin/env node
/**
 * DLX (Dancing Links) Sudoku Solver - JavaScript Implementation
 * Direct port of C reference implementation using Algorithm X with Dancing Links.
 *
 * Algorithm:
 * - Exact cover problem solved with Knuth's Algorithm X
 * - Dancing Links technique for efficient backtracking
 * - Minimum column size heuristic
 */

const fs = require('fs');

// ============================================================================
// DATA STRUCTURES
// ============================================================================

class DlxNode {
    constructor() {
        this.left = null;
        this.right = null;
        this.up = null;
        this.down = null;
        this.column = null;
        this.rowId = -1;
    }
}

class DlxColumn {
    constructor(name) {
        this.node = new DlxNode();
        this.size = 0;
        this.name = name || "";

        // Initialize column node as circular list
        this.node.left = this.node;
        this.node.right = this.node;
        this.node.up = this.node;
        this.node.down = this.node;
        this.node.column = this;
    }
}

// ============================================================================
// GLOBAL STATE
// ============================================================================

let dlxIterations = 0;
let puzzle = Array.from({length: 9}, () => Array(9).fill(0));
let solutionGrid = Array.from({length: 9}, () => Array(9).fill(0));

// DLX matrix structures
let root = null;
let columns = [];  // 324 constraint columns
let rowInfo = [];  // Maps row IDs to (row, col, num)
let rowStarts = []; // First node of each DLX row

// ============================================================================
// CONSTRAINT COLUMN INDICES
// ============================================================================

function getPositionCol(r, c) {
    return r * 9 + c;
}

function getRowCol(r, n) {
    return 81 + r * 9 + (n - 1);
}

function getColCol(c, n) {
    return 162 + c * 9 + (n - 1);
}

function getBoxCol(r, c, n) {
    const box = Math.floor(r / 3) * 3 + Math.floor(c / 3);
    return 243 + box * 9 + (n - 1);
}

// ============================================================================
// DLX CORE OPERATIONS
// ============================================================================

function coverColumn(c) {
    const colNode = c.node;

    // Remove column header from the header list
    colNode.right.left = colNode.left;
    colNode.left.right = colNode.right;

    // For each row in this column
    let rowNode = colNode.down;
    while (rowNode !== colNode) {
        // For each node in this row (excluding the column itself)
        let rightNode = rowNode.right;
        while (rightNode !== rowNode) {
            // Remove this node from its column
            rightNode.down.up = rightNode.up;
            rightNode.up.down = rightNode.down;
            rightNode.column.size--;
            rightNode = rightNode.right;
        }
        rowNode = rowNode.down;
    }
}

function uncoverColumn(c) {
    const colNode = c.node;

    // For each row in this column (in reverse order)
    let rowNode = colNode.up;
    while (rowNode !== colNode) {
        // For each node in this row (in reverse order)
        let leftNode = rowNode.left;
        while (leftNode !== rowNode) {
            // Restore this node to its column
            leftNode.column.size++;
            leftNode.down.up = leftNode;
            leftNode.up.down = leftNode;
            leftNode = leftNode.left;
        }
        rowNode = rowNode.up;
    }

    // Restore column header to the header list
    colNode.right.left = colNode;
    colNode.left.right = colNode;
}

function chooseColumn() {
    const rootNode = root.node;
    let best = null;
    let minSize = Number.MAX_SAFE_INTEGER;

    let colNode = rootNode.right;
    while (colNode !== rootNode) {
        if (colNode.column.size < minSize) {
            minSize = colNode.column.size;
            best = colNode.column;
        }
        colNode = colNode.right;
    }

    return best;
}

function dlxSearch(k, solution) {
    dlxIterations++;  // Count every search call

    const rootNode = root.node;

    // If matrix is empty, we found a solution
    if (rootNode.right === rootNode) {
        return true;
    }

    // Choose column with minimum size
    const col = chooseColumn();

    // If column has no rows, no solution possible
    if (col.size === 0) {
        return false;
    }

    // Cover this column
    coverColumn(col);

    // Try each row in this column
    let rowNode = col.node.down;
    while (rowNode !== col.node) {
        // Add row to partial solution
        solution[k] = rowNode.rowId;

        // Cover all other columns in this row
        let rightNode = rowNode.right;
        while (rightNode !== rowNode) {
            coverColumn(rightNode.column);
            rightNode = rightNode.right;
        }

        // Recurse
        if (dlxSearch(k + 1, solution)) {
            return true;  // Solution found
        }

        // Backtrack: uncover all columns in this row
        let leftNode = rowNode.left;
        while (leftNode !== rowNode) {
            uncoverColumn(leftNode.column);
            leftNode = leftNode.left;
        }

        rowNode = rowNode.down;
    }

    // Uncover column
    uncoverColumn(col);

    return false;  // No solution found
}

// ============================================================================
// DLX MATRIX CONSTRUCTION
// ============================================================================

function initDlxMatrix() {
    // Create root column
    root = new DlxColumn("root");

    // Create 324 column headers
    columns = [];
    for (let i = 0; i < 324; i++) {
        const col = new DlxColumn(`C${i}`);

        // Link into header list
        col.node.left = root.node.left;
        col.node.right = root.node;
        root.node.left.right = col.node;
        root.node.left = col.node;

        columns.push(col);
    }

    rowInfo = [];
    rowStarts = [];
}

function addNode(col, rowId) {
    const node = new DlxNode();
    node.column = col;
    node.rowId = rowId;

    // Insert at end of column's circular list
    node.down = col.node;
    node.up = col.node.up;
    col.node.up.down = node;
    col.node.up = node;
    col.size++;

    return node;
}

function buildDlxRow(r, c, n, rowId) {
    // Store row metadata
    rowInfo[rowId] = { row: r, col: c, num: n };

    // Create nodes for the 4 constraints
    const n1 = addNode(columns[getPositionCol(r, c)], rowId);
    const n2 = addNode(columns[getRowCol(r, n)], rowId);
    const n3 = addNode(columns[getColCol(c, n)], rowId);
    const n4 = addNode(columns[getBoxCol(r, c, n)], rowId);

    // Link nodes horizontally in circular list
    n1.right = n2;
    n2.right = n3;
    n3.right = n4;
    n4.right = n1;

    n1.left = n4;
    n2.left = n1;
    n3.left = n2;
    n4.left = n3;

    // Store first node for this row
    rowStarts[rowId] = n1;
}

function buildDlxMatrixFromPuzzle() {
    let rowId = 0;

    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (puzzle[r][c] !== 0) {
                // Cell has a clue - create only one row for that value
                buildDlxRow(r, c, puzzle[r][c], rowId++);
            } else {
                // Cell is empty - create rows for all possible values
                for (let n = 1; n <= 9; n++) {
                    buildDlxRow(r, c, n, rowId++);
                }
            }
        }
    }
}

function coverClues() {
    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (puzzle[r][c] !== 0) {
                const n = puzzle[r][c];

                // Find the row for this clue
                for (let rowId = 0; rowId < rowStarts.length; rowId++) {
                    if (rowStarts[rowId] &&
                        rowInfo[rowId].row === r &&
                        rowInfo[rowId].col === c &&
                        rowInfo[rowId].num === n) {

                        // Cover all columns in this row
                        let node = rowStarts[rowId];
                        let curr = node;
                        do {
                            coverColumn(curr.column);
                            curr = curr.right;
                        } while (curr !== node);
                        break;
                    }
                }
            }
        }
    }
}

function extractSolution(solution, solutionLen) {
    // Initialize solution grid with original puzzle (includes clues)
    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            solutionGrid[r][c] = puzzle[r][c];
        }
    }

    // Each solution entry is a row_id
    for (let i = 0; i < solutionLen; i++) {
        const rowId = solution[i];
        if (rowId >= 0 && rowId < 729 && rowInfo[rowId]) {
            solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num;
        }
    }
}

// ============================================================================
// PUZZLE I/O
// ============================================================================

function printPuzzle(grid) {
    console.log("\nPuzzle:");
    for (let r = 0; r < 9; r++) {
        console.log(grid[r].join(" ") + " ");
    }
}

function readMatrixFile(filename) {
    // Normalize path for output
    let displayPath = filename;
    if (filename.startsWith("/app/Matrices/")) {
        displayPath = "../" + filename.substring(5);
    }
    console.log(displayPath);

    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.split('\n');
    let lineCount = 0;

    for (const line of lines) {
        const trimmed = line.trim();
        // Skip comments and empty lines
        if (trimmed === '' || trimmed.startsWith('#')) continue;

        // Parse 9 integers from line
        const values = trimmed.split(/\s+/).map(Number);
        if (values.length === 9 && lineCount < 9) {
            puzzle[lineCount] = values;
            console.log(values.join(" ") + " ");
            lineCount++;
        }
    }

    return lineCount === 9;
}

// ============================================================================
// MAIN EXECUTION
// ============================================================================

const startTime = performance.now();

// Process command line arguments
for (const arg of process.argv.slice(2)) {
    if (!arg.endsWith(".matrix")) continue;

    // Reset puzzle
    puzzle = Array.from({length: 9}, () => Array(9).fill(0));
    solutionGrid = Array.from({length: 9}, () => Array(9).fill(0));

    if (!readMatrixFile(arg)) {
        console.error(`Error reading ${arg}`);
        continue;
    }

    printPuzzle(puzzle);

    // Initialize DLX matrix
    initDlxMatrix();

    // Build matrix from puzzle
    buildDlxMatrixFromPuzzle();

    // Cover pre-filled clues
    coverClues();

    // Solve using DLX
    dlxIterations = 0;
    const solution = new Array(81);
    const result = dlxSearch(0, solution);

    if (result) {
        extractSolution(solution, 81);
        printPuzzle(solutionGrid);
        console.log(`\nSolved in Iterations=${dlxIterations}\n`);
    } else {
        console.log("\nNo solution found\n");
    }
}

const elapsed = (performance.now() - startTime) / 1000;
console.log(`Seconds to process ${elapsed.toFixed(3)}`);
