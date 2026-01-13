#!/usr/bin/env node
/**
 * CP (Constraint Propagation) Sudoku Solver - JavaScript Implementation
 * Direct port of C reference implementation with MRV heuristic and propagation.
 *
 * Algorithm:
 * - Constraint propagation (singleton elimination + hidden singles)
 * - Minimum Remaining Values (MRV) heuristic for cell selection
 * - Backtracking search with propagation at each step
 */

const fs = require('fs');

// ============================================================================
// DATA STRUCTURES
// ============================================================================

class CPGrid {
    constructor() {
        this.values = Array.from({length: 9}, () => Array(9).fill(0));
        this.candidates = Array.from({length: 9}, () => Array(9).fill(0));
    }

    copy() {
        const grid = new CPGrid();
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                grid.values[r][c] = this.values[r][c];
                grid.candidates[r][c] = this.candidates[r][c];
            }
        }
        return grid;
    }
}

// ============================================================================
// GLOBAL STATE
// ============================================================================

let cpIterations = 0;
let puzzle = Array.from({length: 9}, () => Array(9).fill(0));

// ============================================================================
// BITSET HELPERS
// ============================================================================

function hasCandidate(set, digit) {
    return (set & (1 << digit)) !== 0;
}

function addCandidate(set, digit) {
    return set | (1 << digit);
}

function removeCandidate(set, digit) {
    return set & ~(1 << digit);
}

function countCandidates(set) {
    let count = 0;
    while (set) {
        count += set & 1;
        set >>= 1;
    }
    return count;
}

function getFirstCandidate(set) {
    for (let digit = 1; digit <= 9; digit++) {
        if (hasCandidate(set, digit)) {
            return digit;
        }
    }
    return 0;
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function getPeers(row, col) {
    const peers = [];

    // Same row (8 cells)
    for (let c = 0; c < 9; c++) {
        if (c !== col) {
            peers.push([row, c]);
        }
    }

    // Same column (8 cells)
    for (let r = 0; r < 9; r++) {
        if (r !== row) {
            peers.push([r, col]);
        }
    }

    // Same 3x3 box (4 cells not already counted)
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

// ============================================================================
// INITIALIZATION
// ============================================================================

function initGrid(grid, puzzle) {
    for (let row = 0; row < 9; row++) {
        for (let col = 0; col < 9; col++) {
            if (puzzle[row][col] === 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid.values[row][col] = 0;
                grid.candidates[row][col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                const digit = puzzle[row][col];
                grid.values[row][col] = digit;
                grid.candidates[row][col] = (1 << digit);
            }
        }
    }
}

// ============================================================================
// CONSTRAINT PROPAGATION
// ============================================================================

function eliminate(grid, row, col, digit) {
    // Check if digit is already eliminated
    if (!hasCandidate(grid.candidates[row][col], digit)) {
        return true;  // Already eliminated, no change
    }

    // Remove digit from candidates
    grid.candidates[row][col] = removeCandidate(grid.candidates[row][col], digit);

    // Check for contradiction (no candidates left)
    const remaining = countCandidates(grid.candidates[row][col]);
    if (remaining === 0) {
        return false;  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining === 1 && grid.values[row][col] === 0) {
        const lastDigit = getFirstCandidate(grid.candidates[row][col]);
        if (!assign(grid, row, col, lastDigit)) {
            return false;  // Assignment caused contradiction
        }
    }

    return true;
}

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
            return false;  // Contradiction in peer elimination
        }
    }

    return true;
}

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
                        return false;  // Contradiction
                    }
                    if (numCandidates === 1) {
                        const digit = getFirstCandidate(grid.candidates[row][col]);
                        if (!assign(grid, row, col, digit)) {
                            return false;  // Assignment caused contradiction
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
                        count = 0;  // Already assigned
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
                } else if (count === 0) {
                    // Check if digit is already assigned in this row
                    let found = false;
                    for (let col = 0; col < 9; col++) {
                        if (grid.values[row][col] === digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false;  // Digit cannot be placed anywhere in row
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
                        count = 0;  // Already assigned
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
                } else if (count === 0) {
                    // Check if digit is already assigned in this column
                    let found = false;
                    for (let row = 0; row < 9; row++) {
                        if (grid.values[row][col] === digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false;  // Digit cannot be placed anywhere in column
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

                let foundAssigned = false;
                for (let r = boxRow; r < boxRow + 3; r++) {
                    for (let c = boxCol; c < boxCol + 3; c++) {
                        if (grid.values[r][c] === digit) {
                            foundAssigned = true;
                            count = 0;
                            break;
                        }
                        if (hasCandidate(grid.candidates[r][c], digit)) {
                            count++;
                            lastR = r;
                            lastC = c;
                        }
                    }
                    if (foundAssigned) break;
                }

                if (count === 1) {
                    if (!assign(grid, lastR, lastC, digit)) {
                        return false;
                    }
                    changed = true;
                } else if (count === 0) {
                    // Check if digit is already assigned in this box
                    let found = false;
                    for (let r = boxRow; r < boxRow + 3; r++) {
                        for (let c = boxCol; c < boxCol + 3; c++) {
                            if (grid.values[r][c] === digit) {
                                found = true;
                                break;
                            }
                        }
                        if (found) break;
                    }
                    if (!found) {
                        return false;  // Digit cannot be placed anywhere in box
                    }
                }
            }
        }
    }

    return true;  // Success - reached fixpoint
}

// ============================================================================
// SEARCH
// ============================================================================

function findMrvCell(grid) {
    let minCandidates = 10;  // More than 9, so any cell will be smaller
    let mrvRow = -1, mrvCol = -1;

    for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (grid.values[r][c] === 0) {
                const numCandidates = countCandidates(grid.candidates[r][c]);
                if (numCandidates < minCandidates) {
                    minCandidates = numCandidates;
                    mrvRow = r;
                    mrvCol = c;
                }
            }
        }
    }

    return (mrvRow === -1) ? null : {row: mrvRow, col: mrvCol};
}

function cpSearch(grid, solution) {
    // Base case: check if grid is complete
    const mrvCell = findMrvCell(grid);
    if (!mrvCell) {
        // No empty cells - grid is complete, extract solution
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                solution[r * 9 + c] = grid.values[r][c];
            }
        }
        return true;  // Solved
    }

    // Recursive case: try each candidate for the MRV cell
    const candidates = grid.candidates[mrvCell.row][mrvCell.col];

    for (let digit = 1; digit <= 9; digit++) {
        if (hasCandidate(candidates, digit)) {
            // Save grid state for backtracking
            const gridCopy = grid.copy();

            // Try assigning this digit
            if (assign(grid, mrvCell.row, mrvCell.col, digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate(grid)) {
                    // Propagation succeeded, recurse
                    if (cpSearch(grid, solution)) {
                        return true;  // Found solution
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

    if (!readMatrixFile(arg)) {
        console.error(`Failed to read matrix file ${arg}`);
        continue;
    }

    // Print initial puzzle
    printPuzzle(puzzle);

    // Initialize CP grid
    const grid = new CPGrid();
    initGrid(grid, puzzle);

    // Apply initial propagation
    if (!propagate(grid)) {
        console.log("\nNo solution found (contradiction during initial propagation)\n");
        continue;
    }

    // Run search
    const solution = new Array(81);
    cpIterations = 0;
    const solved = cpSearch(grid, solution);

    if (solved) {
        // Convert solution array back to 2D for printing
        const solutionGrid = Array.from({length: 9}, () => Array(9).fill(0));
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                solutionGrid[r][c] = solution[r * 9 + c];
            }
        }

        printPuzzle(solutionGrid);
        console.log(`\nSolved in Iterations=${cpIterations}\n`);
    } else {
        console.log("\nNo solution found\n");
    }
}

const elapsed = (performance.now() - startTime) / 1000;
console.log(`Seconds to process ${elapsed.toFixed(3)}`);
