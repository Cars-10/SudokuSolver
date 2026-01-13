#!/usr/bin/env npx ts-node
"use strict";
/**
 * Sudoku Solver - TypeScript CP (Constraint Propagation) Implementation
 * Port of C reference implementation using constraint propagation with backtracking.
 *
 * Algorithm:
 * - Bitsets for candidate tracking (bits 1-9)
 * - Constraint propagation: singleton elimination + hidden singles
 * - Minimum Remaining Values (MRV) heuristic for search
 * - Backtracking with state restoration
 */
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const process = require("process");
// Global iteration counter (matches C implementation)
let cpIterations = 0;
// Bitset helper functions
function hasCandidate(set, digit) {
    return (set & (1 << digit)) !== 0;
}
function removeCandidate(set, digit) {
    return set & ~(1 << digit);
}
function countCandidates(set) {
    // JavaScript popcount implementation
    let count = 0;
    let n = set;
    while (n) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}
function getFirstCandidate(cs) {
    for (let digit = 1; digit <= 9; digit++) {
        if (hasCandidate(cs, digit)) {
            return digit;
        }
    }
    return 0;
}
// Get all 20 peers for a cell (row, col, box)
function getPeers(row, col) {
    const peers = [];
    // Same row (8 cells excluding self)
    for (let c = 0; c < 9; c++) {
        if (c !== col) {
            peers.push([row, c]);
        }
    }
    // Same column (8 cells excluding self)
    for (let r = 0; r < 9; r++) {
        if (r !== row) {
            peers.push([r, col]);
        }
    }
    // Same 3x3 box (4 cells excluding self and already counted)
    const boxRow = Math.floor(row / 3) * 3;
    const boxCol = Math.floor(col / 3) * 3;
    for (let r = boxRow; r < boxRow + 3; r++) {
        for (let c = boxCol; c < boxCol + 3; c++) {
            if (r !== row && c !== col) {
                peers.push([r, c]);
            }
        }
    }
    return peers;
}
// Initialize grid from puzzle
function initGrid(grid, puzzle) {
    for (let row = 0; row < 9; row++) {
        for (let col = 0; col < 9; col++) {
            if (puzzle[row][col] === 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid.values[row][col] = 0;
                grid.candidates[row][col] = 0x3FE; // Binary: 0011 1111 1110 (bits 1-9)
            }
            else {
                // Given clue: set single value
                const digit = puzzle[row][col];
                grid.values[row][col] = digit;
                grid.candidates[row][col] = (1 << digit);
            }
        }
    }
}
// Eliminate a digit from a cell's candidates
function eliminate(grid, row, col, digit) {
    // Check if digit is already eliminated
    if (!hasCandidate(grid.candidates[row][col], digit)) {
        return true; // Already eliminated, no change
    }
    // Remove digit from candidates
    grid.candidates[row][col] = removeCandidate(grid.candidates[row][col], digit);
    // Check for contradiction (no candidates left)
    const remaining = countCandidates(grid.candidates[row][col]);
    if (remaining === 0) {
        return false; // Contradiction
    }
    // If only one candidate left, assign it (singleton elimination)
    if (remaining === 1 && grid.values[row][col] === 0) {
        const lastDigit = getFirstCandidate(grid.candidates[row][col]);
        if (!assign(grid, row, col, lastDigit)) {
            return false; // Assignment caused contradiction
        }
    }
    return true;
}
// Assign a value to a cell
function assign(grid, row, col, digit) {
    // Increment iteration counter (this is our benchmark metric)
    cpIterations++;
    // Set value
    grid.values[row][col] = digit;
    grid.candidates[row][col] = (1 << digit);
    // Eliminate digit from all peers
    const peers = getPeers(row, col);
    for (const [peerRow, peerCol] of peers) {
        if (!eliminate(grid, peerRow, peerCol, digit)) {
            return false; // Contradiction in peer elimination
        }
    }
    return true;
}
// Apply constraint propagation
function propagate(grid) {
    let changed = true;
    while (changed) {
        changed = false;
        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for (let row = 0; row < 9; row++) {
            for (let col = 0; col < 9; col++) {
                if (grid.values[row][col] === 0) {
                    const numCandidates = countCandidates(grid.candidates[row][col]);
                    if (numCandidates === 0) {
                        return false; // Contradiction
                    }
                    if (numCandidates === 1) {
                        const digit = getFirstCandidate(grid.candidates[row][col]);
                        if (!assign(grid, row, col, digit)) {
                            return false; // Assignment caused contradiction
                        }
                        changed = true;
                    }
                }
            }
        }
        // Strategy 2: Hidden singles
        // For each unit (row, col, box), if a digit appears in only one cell, assign it
        // Check rows
        for (let row = 0; row < 9; row++) {
            for (let digit = 1; digit <= 9; digit++) {
                let count = 0;
                let lastCol = -1;
                for (let col = 0; col < 9; col++) {
                    if (grid.values[row][col] === digit) {
                        count = 0; // Already assigned
                        break;
                    }
                    if (hasCandidate(grid.candidates[row][col], digit)) {
                        count++;
                        lastCol = col;
                    }
                }
                if (count === 1) {
                    if (!assign(grid, row, lastCol, digit)) {
                        return false;
                    }
                    changed = true;
                }
                else if (count === 0) {
                    // Check if digit is already assigned in this row
                    let found = false;
                    for (let col = 0; col < 9; col++) {
                        if (grid.values[row][col] === digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false; // Digit cannot be placed anywhere in row
                    }
                }
            }
        }
        // Check columns
        for (let col = 0; col < 9; col++) {
            for (let digit = 1; digit <= 9; digit++) {
                let count = 0;
                let lastRow = -1;
                for (let row = 0; row < 9; row++) {
                    if (grid.values[row][col] === digit) {
                        count = 0; // Already assigned
                        break;
                    }
                    if (hasCandidate(grid.candidates[row][col], digit)) {
                        count++;
                        lastRow = row;
                    }
                }
                if (count === 1) {
                    if (!assign(grid, lastRow, col, digit)) {
                        return false;
                    }
                    changed = true;
                }
                else if (count === 0) {
                    // Check if digit is already assigned in this column
                    let found = false;
                    for (let row = 0; row < 9; row++) {
                        if (grid.values[row][col] === digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false; // Digit cannot be placed anywhere in column
                    }
                }
            }
        }
        // Check boxes
        for (let box = 0; box < 9; box++) {
            const boxRow = Math.floor(box / 3) * 3;
            const boxCol = (box % 3) * 3;
            for (let digit = 1; digit <= 9; digit++) {
                let count = 0;
                let lastR = -1, lastC = -1;
                let foundInBox = false;
                for (let r = boxRow; r < boxRow + 3; r++) {
                    for (let c = boxCol; c < boxCol + 3; c++) {
                        if (grid.values[r][c] === digit) {
                            foundInBox = true;
                            count = 0; // Already assigned
                            break;
                        }
                        if (hasCandidate(grid.candidates[r][c], digit)) {
                            count++;
                            lastR = r;
                            lastC = c;
                        }
                    }
                    if (foundInBox)
                        break;
                }
                if (count === 1) {
                    if (!assign(grid, lastR, lastC, digit)) {
                        return false;
                    }
                    changed = true;
                }
                else if (count === 0 && !foundInBox) {
                    return false; // Digit cannot be placed anywhere in box
                }
            }
        }
    }
    return true; // Success - reached fixpoint
}
// Find cell with Minimum Remaining Values
function findMrvCell(grid) {
    let minCandidates = 10; // More than 9, so any cell will be smaller
    let bestRow = -1, bestCol = -1;
    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (grid.values[r][c] === 0) {
                const numCandidates = countCandidates(grid.candidates[r][c]);
                if (numCandidates < minCandidates) {
                    minCandidates = numCandidates;
                    bestRow = r;
                    bestCol = c;
                }
            }
        }
    }
    if (bestRow === -1) {
        return null; // No empty cells (grid complete)
    }
    return [bestRow, bestCol];
}
// Deep copy grid for backtracking
function copyGrid(grid) {
    return {
        values: grid.values.map(row => [...row]),
        candidates: grid.candidates.map(row => [...row])
    };
}
// CP Search with backtracking
function cpSearch(grid, solution) {
    // Base case: check if grid is complete
    const mrvCell = findMrvCell(grid);
    if (mrvCell === null) {
        // No empty cells - grid is complete, extract solution
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                solution[r * 9 + c] = grid.values[r][c];
            }
        }
        return true; // Solved
    }
    const [mrvRow, mrvCol] = mrvCell;
    // Recursive case: try each candidate for the MRV cell
    const candidates = grid.candidates[mrvRow][mrvCol];
    for (let digit = 1; digit <= 9; digit++) {
        if (hasCandidate(candidates, digit)) {
            // Save grid state for backtracking
            const gridCopy = copyGrid(grid);
            // Try assigning this digit
            if (assign(grid, mrvRow, mrvCol, digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate(grid)) {
                    // Propagation succeeded, recurse
                    if (cpSearch(grid, solution)) {
                        return true; // Found solution
                    }
                }
            }
            // Failed - restore grid state and try next candidate
            grid.values = gridCopy.values;
            grid.candidates = gridCopy.candidates;
        }
    }
    // All candidates exhausted - dead end
    return false;
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
if (process.argv.length < 3) {
    console.error("Usage: ts-node cp.ts <matrix_file>");
    process.exit(1);
}
const filename = process.argv[2];
if (!filename.endsWith(".matrix")) {
    console.error("Error: File must end with .matrix");
    process.exit(1);
}
// Read puzzle from file
const puzzle = readMatrixFile(filename);
// Print initial puzzle
printPuzzle(puzzle);
// Initialize CP grid
const grid = {
    values: Array(9).fill(0).map(() => Array(9).fill(0)),
    candidates: Array(9).fill(0).map(() => Array(9).fill(0))
};
initGrid(grid, puzzle);
// Apply initial propagation
if (!propagate(grid)) {
    console.log("\nNo solution found (contradiction during initial propagation)\n");
    const elapsed = (performance.now() - startTime) / 1000;
    console.log(`Seconds to process ${elapsed.toFixed(3)}`);
    process.exit(0);
}
// Run search
cpIterations = 0;
const solution = new Array(81);
const solved = cpSearch(grid, solution);
if (solved) {
    // Convert solution array back to 2D for printing
    const solutionGrid = [];
    for (let r = 0; r < 9; r++) {
        const row = [];
        for (let c = 0; c < 9; c++) {
            row.push(solution[r * 9 + c]);
        }
        solutionGrid.push(row);
    }
    printPuzzle(solutionGrid);
    console.log(`\nSolved in Iterations=${cpIterations}\n`);
}
else {
    console.log("\nNo solution found\n");
}
const elapsed = (performance.now() - startTime) / 1000;
console.log(`Seconds to process ${elapsed.toFixed(3)}`);
