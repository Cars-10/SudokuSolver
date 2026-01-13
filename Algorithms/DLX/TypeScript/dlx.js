#!/usr/bin/env npx ts-node
"use strict";
/**
 * Sudoku Solver - TypeScript DLX (Dancing Links) Implementation
 * Port of C reference implementation using Algorithm X with Dancing Links.
 *
 * Algorithm:
 * - Exact cover problem: 324 constraints (position, row, column, box)
 * - Dancing Links data structure for efficient backtracking
 * - Choose column with minimum size heuristic
 */
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const process = require("process");
// Global iteration counter (matches C implementation)
let dlxIterations = 0;
// Node in the Dancing Links structure
class DlxNode {
    constructor() {
        this.left = this;
        this.right = this;
        this.up = this;
        this.down = this;
        this.column = null; // Will be set during construction
        this.rowId = -1;
    }
}
// Column header (inherits from DlxNode)
class DlxColumn {
    constructor(name) {
        this.node = new DlxNode();
        this.node.column = this;
        this.size = 0;
        this.name = name;
    }
}
const rowInfo = [];
const rowStarts = [];
// Calculate constraint column indices (matching C implementation)
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
// Cover a column in the DLX matrix
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
// Uncover a column (exact reverse of cover)
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
// Choose column with minimum size (Knuth's S heuristic)
function chooseColumn(root) {
    const rootNode = root.node;
    let best = null;
    let minSize = Number.MAX_SAFE_INTEGER;
    let colNode = rootNode.right;
    while (colNode !== rootNode) {
        const col = colNode.column;
        if (col.size < minSize) {
            minSize = col.size;
            best = col;
        }
        colNode = colNode.right;
    }
    return best;
}
// DLX Search - Algorithm X with Dancing Links
function dlxSearch(root, k, solution) {
    dlxIterations++; // Count every search call
    const rootNode = root.node;
    // If matrix is empty, we found a solution
    if (rootNode.right === rootNode) {
        return true;
    }
    // Choose column with minimum size
    const col = chooseColumn(root);
    // If column has no rows, no solution possible
    if (!col || col.size === 0) {
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
        if (dlxSearch(root, k + 1, solution)) {
            return true; // Solution found
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
    return false; // No solution found
}
// Initialize DLX matrix structure
function initDlxMatrix() {
    // Create root column
    const root = new DlxColumn("root");
    // Create 324 column headers
    const columns = [];
    for (let i = 0; i < 324; i++) {
        const col = new DlxColumn(`C${i}`);
        columns.push(col);
        // Link into header list (circular)
        col.node.left = root.node.left;
        col.node.right = root.node;
        root.node.left.right = col.node;
        root.node.left = col.node;
    }
    return { root, columns };
}
// Add a node to the DLX matrix
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
// Build a DLX row for Sudoku cell (r,c) with value n
function buildDlxRow(r, c, n, rowId, columns) {
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
// Build the complete DLX matrix from the puzzle
function buildDlxMatrixFromPuzzle(puzzle, columns) {
    let rowId = 0;
    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (puzzle[r][c] !== 0) {
                // Cell has a clue - create only one row for that value
                buildDlxRow(r, c, puzzle[r][c], rowId++, columns);
            }
            else {
                // Cell is empty - create rows for all possible values
                for (let n = 1; n <= 9; n++) {
                    buildDlxRow(r, c, n, rowId++, columns);
                }
            }
        }
    }
}
// Cover given clues (pre-selected rows)
function coverClues(puzzle) {
    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (puzzle[r][c] !== 0) {
                const n = puzzle[r][c];
                // Find the row for this clue
                for (let rowId = 0; rowId < 729; rowId++) {
                    if (rowStarts[rowId] &&
                        rowInfo[rowId].row === r &&
                        rowInfo[rowId].col === c &&
                        rowInfo[rowId].num === n) {
                        // Cover all columns in this row
                        const node = rowStarts[rowId];
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
// Extract solution from DLX and populate solution grid
function extractSolution(solution, solutionLen, puzzle) {
    // Initialize solution grid - start with the original puzzle (includes clues)
    const solutionGrid = puzzle.map(row => [...row]);
    // Each solution entry is a row_id
    for (let i = 0; i < solutionLen; i++) {
        const rowId = solution[i];
        if (rowId >= 0 && rowId < 729 && rowInfo[rowId]) {
            solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num;
        }
    }
    return solutionGrid;
}
// Print puzzle
function printPuzzle(grid) {
    console.log("\nPuzzle:");
    for (let r = 0; r < 9; r++) {
        console.log(grid[r].join(" ") + " ");
    }
}
// Read matrix file
function readMatrixFile(filename) {
    // Normalize path for output (match C format)
    let displayPath = filename;
    if (filename.startsWith("/app/Matrices/")) {
        displayPath = "../" + filename.substring(5); // Skip "/app/" to get "Matrices/..."
    }
    console.log(displayPath);
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.split('\n');
    const puzzle = [];
    for (const line of lines) {
        const trimmed = line.trim();
        // Skip comments and empty lines
        if (trimmed === '' || trimmed.startsWith('#'))
            continue;
        // Parse 9 integers from line
        const values = trimmed.split(/\s+/).map(Number);
        if (values.length === 9 && puzzle.length < 9) {
            puzzle.push(values);
            console.log(values.join(" ") + " ");
        }
    }
    return puzzle;
}
// Main program
const startTime = performance.now();
for (const arg of process.argv.slice(2)) {
    if (!arg.endsWith(".matrix"))
        continue;
    const puzzle = readMatrixFile(arg);
    printPuzzle(puzzle);
    // Initialize DLX matrix
    const { root, columns } = initDlxMatrix();
    // Build matrix from puzzle
    buildDlxMatrixFromPuzzle(puzzle, columns);
    // Cover pre-filled clues
    coverClues(puzzle);
    // Solve using DLX
    dlxIterations = 0;
    const solution = new Array(81);
    const result = dlxSearch(root, 0, solution);
    if (result) {
        const solutionGrid = extractSolution(solution, 81, puzzle);
        printPuzzle(solutionGrid);
        console.log(`\nSolved in Iterations=${dlxIterations}\n`);
    }
    else {
        console.log("\nNo solution found\n");
    }
}
const elapsed = (performance.now() - startTime) / 1000;
console.log(`Seconds to process ${elapsed.toFixed(3)}`);
