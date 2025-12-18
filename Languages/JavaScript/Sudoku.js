#!/usr/bin/env node
/**
 * Sudoku Solver - JavaScript Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * Algorithm:
 * - Row-major search for empty cells (top-to-bottom, left-to-right)
 * - Try values 1-9 in ascending order
 * - Count EVERY placement attempt (algorithm fingerprint)
 */

import fs from 'fs';
import path from 'path';
import process from 'process';

// Global puzzle grid [row][col]
let puzzle = [];
for (let i = 0; i < 9; i++) {
    puzzle[i] = new Array(9).fill(0);
}
let count = 0;  // Iteration counter

function printPuzzle() {
    console.log("\nPuzzle:");
    for (let row = 0; row < 9; row++) {
        console.log(puzzle[row].join(" ") + " ");
    }
}

function readMatrixFile(filename) {
    // Normalize path for output (match C format)
    let displayPath = filename;
    if (filename.startsWith("/app/Matrices/")) {
        displayPath = "../" + filename.substring(5);  // Skip "/app/" to get "Matrices/..."
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
}

function isValid(row, col, val) {
    // Check row
    for (let i = 0; i < 9; i++) {
        if (puzzle[row][i] === val) return false;
    }

    // Check column
    for (let i = 0; i < 9; i++) {
        if (puzzle[i][col] === val) return false;
    }

    // Check 3x3 box
    const boxRow = Math.floor(row / 3) * 3;
    const boxCol = Math.floor(col / 3) * 3;
    for (let i = 0; i < 3; i++) {
        for (let j = 0; j < 3; j++) {
            if (puzzle[boxRow + i][boxCol + j] === val) return false;
        }
    }

    return true;
}

function solve() {
    // Find first empty cell (row-major order)
    let row = -1, col = -1;
    outer: for (let r = 0; r < 9; r++) {
        for (let c = 0; c < 9; c++) {
            if (puzzle[r][c] === 0) {
                row = r;
                col = c;
                break outer;
            }
        }
    }

    // If no empty cell found, puzzle is solved
    if (row === -1) {
        printPuzzle();
        console.log(`\nSolved in Iterations=${count}\n`);
        return true;
    }

    // Try values 1-9 in order
    for (let val = 1; val <= 9; val++) {
        count++;  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if (isValid(row, col, val)) {
            puzzle[row][col] = val;  // Place value

            if (solve()) {
                return true;
            }

            puzzle[row][col] = 0;  // Backtrack
        }
    }

    return false;
}

// Main program - process each .matrix file from command line
const startTime = performance.now();

for (const arg of process.argv.slice(2)) {
    if (!arg.endsWith(".matrix")) continue;

    // Reset puzzle
    puzzle = [];
    for (let i = 0; i < 9; i++) {
        puzzle[i] = new Array(9).fill(0);
    }

    readMatrixFile(arg);
    printPuzzle();
    count = 0;
    solve();
}

const elapsed = (performance.now() - startTime) / 1000;
console.log(`Seconds to process ${elapsed.toFixed(3)}`);
