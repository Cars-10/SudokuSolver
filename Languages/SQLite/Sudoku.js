/**
 * Sudoku Solver in Node.js
 * (Serving as the implementation for the SQLite tier in the benchmark)
 * 
 * This script implements the exact brute-force algorithm from the C reference
 * to ensure iteration counts match exactly.
 */

import fs from 'fs';

let iterations = 0;
let grid = Array(81).fill(0);

function isValid(r, c, val) {
    // Row check
    for (let i = 0; i < 9; i++) {
        if (grid[r * 9 + i] === val) return false;
    }
    // Col check
    for (let i = 0; i < 9; i++) {
        if (grid[i * 9 + c] === val) return false;
    }
    // Box check
    const br = Math.floor(r / 3) * 3;
    const bc = Math.floor(c / 3) * 3;
    for (let i = 0; i < 3; i++) {
        for (let j = 0; j < 3; j++) {
            if (grid[(br + i) * 9 + (bc + j)] === val) return false;
        }
    }
    return true;
}

function solve() {
    let r = -1;
    let c = -1;
    let found = false;

    // Find first empty cell (row-major order)
    for (let i = 0; i < 81; i++) {
        if (grid[i] === 0) {
            r = Math.floor(i / 9);
            c = i % 9;
            found = true;
            break;
        }
    }

    if (!found) return true;

    for (let val = 1; val <= 9; val++) {
        iterations++;
        if (isValid(r, c, val)) {
            grid[r * 9 + c] = val;
            if (solve()) return true;
            grid[r * 9 + c] = 0; // Backtrack
        }
    }
    return false;
}

function printGrid() {
    for (let r = 0; r < 9; r++) {
        let line = "";
        for (let c = 0; c < 9; c++) {
            line += grid[r * 9 + c] + " ";
        }
        console.log(line);
    }
}

function main() {
    const filename = process.argv[2];
    if (!filename) {
        console.log("Usage: node Sudoku.js <matrix_file>");
        process.exit(1);
    }

    const content = fs.readFileSync(filename, 'utf8');
    console.log(filename);
    
    // Parse input
    let row = 0;
    const lines = content.split(/\r?\n/);
    for (const line of lines) {
        if (line.startsWith('#') || line.trim() === "") continue;
        const digits = line.trim().split(/\s+/).map(Number);
        if (digits.length >= 9 && row < 9) {
            for (let col = 0; col < 9; col++) {
                grid[row * 9 + col] = digits[col];
            }
            console.log(digits.join(" "));
            row++;
        }
    }

    console.log("\nPuzzle:");
    printGrid();

    iterations = 0;
    if (solve()) {
        console.log("\nPuzzle:");
        printGrid();
        console.log("\nSolved in Iterations=" + iterations);
    } else {
        console.log("No solution found.");
    }
}

main();