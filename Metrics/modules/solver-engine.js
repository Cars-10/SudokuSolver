/**
 * Sudoku Solver - Browser-Compatible JavaScript Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * This solver is adapted from Algorithms/BruteForce/JavaScript/Sudoku.js
 * for browser execution with state emission capabilities.
 *
 * Key differences from Node.js version:
 * - No fs, process, or Node-specific APIs
 * - State callback emission at each iteration
 * - Support for both sync solving and async/generator-based stepping
 * - Deep copy grid snapshots for immutable state history
 *
 * Algorithm Requirements (matches C reference):
 * - Row-major search order: top-to-bottom, left-to-right
 * - Candidate order: 1-9 ascending
 * - Iteration counting: Increment BEFORE validity check
 * - Expected iterations for Matrix 1: 656
 */

export class BruteForceSolver {
    constructor() {
        this.puzzle = [];       // 9x9 grid [row][col]
        this.iteration = 0;     // Total iteration count (algorithm fingerprint)
        this.depth = 0;         // Current recursion depth (backtrack indicator)
        this.onStateChange = null; // Callback function: (state) => void
        this.solved = false;

        // Initialize empty grid
        this.reset();
    }

    /**
     * Load puzzle from 2D array or matrix string
     * @param {Array|string} input - 9x9 2D array or matrix string format
     */
    loadPuzzle(input) {
        if (typeof input === 'string') {
            this.puzzle = BruteForceSolver.parseMatrixString(input);
        } else if (Array.isArray(input)) {
            // Deep copy to prevent external mutation
            this.puzzle = input.map(row => [...row]);
        } else {
            throw new Error('Invalid input: expected 2D array or matrix string');
        }

        // Validate puzzle dimensions
        if (this.puzzle.length !== 9 || !this.puzzle.every(row => row.length === 9)) {
            throw new Error('Invalid puzzle: must be 9x9 grid');
        }

        this.iteration = 0;
        this.solved = false;
        this.depth = 0;
    }

    /**
     * Parse matrix file format string into 9x9 grid
     * Format: 9 lines of 9 space-separated digits (0 = empty)
     * @param {string} matrixString - Matrix file contents
     * @returns {Array} 9x9 2D array
     */
    static parseMatrixString(matrixString) {
        const lines = matrixString.split('\n')
            .map(line => line.trim())
            .filter(line => line && !line.startsWith('#')); // Skip empty/comment lines

        if (lines.length < 9) {
            throw new Error(`Invalid matrix: expected 9 lines, got ${lines.length}`);
        }

        const grid = [];
        for (let i = 0; i < 9; i++) {
            const values = lines[i].split(/\s+/).map(Number);
            if (values.length !== 9) {
                throw new Error(`Invalid matrix line ${i + 1}: expected 9 values, got ${values.length}`);
            }
            grid.push(values);
        }

        return grid;
    }

    /**
     * Check if value is valid at position (row, col)
     * Validates against row, column, and 3x3 box constraints
     */
    isValid(row, col, val) {
        // Check row
        for (let i = 0; i < 9; i++) {
            if (this.puzzle[row][i] === val) return false;
        }

        // Check column
        for (let i = 0; i < 9; i++) {
            if (this.puzzle[i][col] === val) return false;
        }

        // Check 3x3 box
        const boxRow = Math.floor(row / 3) * 3;
        const boxCol = Math.floor(col / 3) * 3;
        for (let i = 0; i < 3; i++) {
            for (let j = 0; j < 3; j++) {
                if (this.puzzle[boxRow + i][boxCol + j] === val) return false;
            }
        }

        return true;
    }

    /**
     * Get deep copy of current puzzle grid
     * Used for immutable state history
     */
    getGridSnapshot() {
        return this.puzzle.map(row => [...row]);
    }

    /**
     * Main solving algorithm with state emission
     * Returns true if solved, false if unsolvable
     */
    solve() {
        // Find first empty cell (row-major order)
        let row = -1, col = -1;
        outer: for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (this.puzzle[r][c] === 0) {
                    row = r;
                    col = c;
                    break outer;
                }
            }
        }

        // If no empty cell found, puzzle is solved
        if (row === -1) {
            this.solved = true;

            // Emit final solved state
            if (this.onStateChange) {
                this.onStateChange({
                    grid: this.getGridSnapshot(),
                    row: -1,
                    col: -1,
                    value: 0,
                    iteration: this.iteration,
                    depth: this.depth,
                    isBacktrack: false,
                    isSolved: true
                });
            }

            return true;
        }

        // Try values 1-9 in ascending order
        for (let val = 1; val <= 9; val++) {
            // COUNT EVERY ATTEMPT - this is the algorithm fingerprint
            this.iteration++;

            // Emit state BEFORE validity check (matches C reference timing)
            if (this.onStateChange) {
                this.onStateChange({
                    grid: this.getGridSnapshot(),
                    row: row,
                    col: col,
                    value: val,
                    iteration: this.iteration,
                    depth: this.depth,
                    isBacktrack: false,
                    isSolved: false
                });
            }

            if (this.isValid(row, col, val)) {
                // Place value and recurse
                this.puzzle[row][col] = val;
                this.depth++;

                if (this.solve()) {
                    return true;
                }

                // Backtrack - restore cell to empty
                this.puzzle[row][col] = 0;
                this.depth--;

                // Emit backtrack state
                if (this.onStateChange) {
                    this.onStateChange({
                        grid: this.getGridSnapshot(),
                        row: row,
                        col: col,
                        value: val,
                        iteration: this.iteration,
                        depth: this.depth,
                        isBacktrack: true,
                        isSolved: false
                    });
                }
            }
        }

        return false;
    }

    /**
     * Async solving algorithm that yields to UI periodically
     * Returns true if solved, false if unsolvable
     */
    async solveAsync(yieldInterval = 100) {
        this._yieldCounter = 0;
        this._yieldInterval = yieldInterval;
        return await this._solveAsyncRecursive();
    }

    async _solveAsyncRecursive() {
        // Find first empty cell (row-major order)
        let row = -1, col = -1;
        outer: for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (this.puzzle[r][c] === 0) {
                    row = r;
                    col = c;
                    break outer;
                }
            }
        }

        // If no empty cell found, puzzle is solved
        if (row === -1) {
            this.solved = true;

            // Emit final solved state
            if (this.onStateChange) {
                this.onStateChange({
                    grid: this.getGridSnapshot(),
                    row: -1,
                    col: -1,
                    value: 0,
                    iteration: this.iteration,
                    depth: this.depth,
                    isBacktrack: false,
                    isSolved: true
                });
            }

            return true;
        }

        // Try values 1-9 in ascending order
        for (let val = 1; val <= 9; val++) {
            // COUNT EVERY ATTEMPT - this is the algorithm fingerprint
            this.iteration++;

            // Yield to UI periodically (every N iterations)
            this._yieldCounter++;
            if (this._yieldCounter % this._yieldInterval === 0) {
                await new Promise(resolve => setTimeout(resolve, 0));
            }

            // Emit state BEFORE validity check (matches C reference timing)
            if (this.onStateChange) {
                this.onStateChange({
                    grid: this.getGridSnapshot(),
                    row: row,
                    col: col,
                    value: val,
                    iteration: this.iteration,
                    depth: this.depth,
                    isBacktrack: false,
                    isSolved: false
                });
            }

            if (this.isValid(row, col, val)) {
                // Place value and recurse
                this.puzzle[row][col] = val;
                this.depth++;

                if (await this._solveAsyncRecursive()) {
                    return true;
                }

                // Backtrack - restore cell to empty
                this.puzzle[row][col] = 0;
                this.depth--;

                // Emit backtrack state
                if (this.onStateChange) {
                    this.onStateChange({
                        grid: this.getGridSnapshot(),
                        row: row,
                        col: col,
                        value: val,
                        iteration: this.iteration,
                        depth: this.depth,
                        isBacktrack: true,
                        isSolved: false
                    });
                }
            }
        }

        return false;
    }

    /**
     * Reset solver to initial state
     */
    reset() {
        this.puzzle = Array(9).fill(null).map(() => Array(9).fill(0));
        this.iteration = 0;
        this.depth = 0;
        this.solved = false;
    }

    /**
     * Get current puzzle state as string (for debugging)
     */
    toString() {
        return this.puzzle.map(row => row.join(' ')).join('\n');
    }
}

// ============================================================================
// Testing Code (commented out - uncomment to run in Node.js)
// ============================================================================

/*
// Matrix 1 puzzle (should solve in 656 iterations)
// Must match Matrices/1.matrix exactly for C-reference compatibility
const matrix1 = [
    [9, 2, 0, 0, 0, 0, 5, 8, 4],
    [0, 0, 0, 5, 0, 0, 0, 0, 3],
    [0, 8, 3, 0, 9, 2, 0, 0, 0],
    [2, 6, 0, 8, 5, 4, 0, 0, 1],
    [0, 0, 5, 3, 6, 1, 0, 9, 0],
    [1, 0, 0, 0, 0, 9, 0, 0, 0],
    [8, 5, 0, 2, 0, 3, 0, 1, 0],
    [4, 1, 2, 9, 8, 0, 0, 3, 0],
    [3, 9, 0, 0, 0, 6, 8, 0, 0]
];

console.log('Testing BruteForceSolver...');

const solver = new BruteForceSolver();
solver.loadPuzzle(matrix1);

let stateCount = 0;
solver.onStateChange = (state) => {
    stateCount++;
    // Uncomment to see all states:
    // console.log(`Iteration ${state.iteration}: (${state.row},${state.col}) = ${state.value} [depth=${state.depth}, backtrack=${state.isBacktrack}]`);
};

const startTime = performance.now();
const solved = solver.solve();
const elapsed = (performance.now() - startTime).toFixed(3);

console.log('\n=== Results ===');
console.log('Solved:', solved);
console.log('Iterations:', solver.iteration);
console.log('States emitted:', stateCount);
console.log('Time:', elapsed, 'ms');

console.assert(solver.iteration === 656, `Expected 656 iterations for Matrix 1, got ${solver.iteration}`);
console.assert(solved === true, 'Matrix 1 should be solvable');
console.assert(stateCount > 0, 'Should emit state callbacks');

console.log('\n✓ All tests passed!');

console.log('\nSolved puzzle:');
console.log(solver.toString());
*/
