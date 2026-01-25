// Interactive Sudoku Solver - Browser Bundle (No ES6 Modules)
// Auto-generated bundle - works with file:// protocol
(function(window) {
    'use strict';

    // ======================================== solver-engine.js
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

class BruteForceSolver {
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

    // ======================================== solver-state.js
/**
 * Sudoku Solver State History Management
 *
 * Provides immutable state history with memory limits for step-by-step
 * animation playback. Uses circular buffer pattern to cap memory usage.
 *
 * Key Features:
 * - Immutable state storage via Object.freeze
 * - Configurable memory limit (default: 10,000 states)
 * - Forward/backward navigation with position pointer
 * - Circular buffer removes oldest states when limit exceeded
 * - Stats tracking for UI display
 *
 * Usage:
 *   const history = new SolverHistory(10000);
 *   solver.onStateChange = (state) => history.push(state);
 *   solver.solve();
 *
 *   // Navigate through history
 *   history.stepBack();
 *   history.stepForward();
 *   history.goToStart();
 *   history.goToEnd();
 */

class SolverHistory {
    constructor(maxStates = 10000) {
        this.states = [];           // Array of frozen state objects
        this.position = -1;         // Current playback position (-1 = no states)
        this.maxStates = maxStates; // Memory limit
        this.totalPushed = 0;       // Track total states seen (even if discarded)
    }

    /**
     * Add new state to history (freezes for immutability)
     * Truncates any future states beyond current position
     * Enforces memory limit by removing oldest states
     *
     * @param {Object} state - State object with grid, row, col, value, iteration, depth, isBacktrack
     */
    push(state) {
        // When pushing, truncate any future states beyond current position
        // This handles the case where user steps back and then solver continues from there
        this.states.length = this.position + 1;

        // Create frozen copy for immutability
        // Deep freeze the grid (both outer array and inner row arrays)
        const frozenState = Object.freeze({
            grid: Object.freeze(state.grid.map(row => Object.freeze([...row]))),
            row: state.row,
            col: state.col,
            value: state.value,
            iteration: state.iteration,
            depth: state.depth,
            isBacktrack: state.isBacktrack,
            isSolved: state.isSolved || false
        });

        this.states.push(frozenState);
        this.position++;
        this.totalPushed++;

        // Enforce memory limit: remove oldest states if exceeded
        if (this.states.length > this.maxStates) {
            const excess = this.states.length - this.maxStates;
            this.states.splice(0, excess);
            this.position -= excess;

            // Safety: ensure position doesn't go negative
            if (this.position < 0) {
                this.position = 0;
            }
        }
    }

    /**
     * Check if backward navigation is possible
     */
    canStepBack() {
        return this.position > 0;
    }

    /**
     * Check if forward navigation is possible
     */
    canStepForward() {
        return this.position < this.states.length - 1;
    }

    /**
     * Step backward to previous state
     * @returns {Object|null} Previous state or null if at start
     */
    stepBack() {
        if (this.canStepBack()) {
            this.position--;
        }
        return this.getCurrentState();
    }

    /**
     * Step forward to next state
     * @returns {Object|null} Next state or null if at end
     */
    stepForward() {
        if (this.canStepForward()) {
            this.position++;
        }
        return this.getCurrentState();
    }

    /**
     * Get state at current position
     * @returns {Object|null} Current state or null if no states
     */
    getCurrentState() {
        if (this.position >= 0 && this.position < this.states.length) {
            return this.states[this.position];
        }
        return null;
    }

    /**
     * Jump to specific position in history
     * @param {number} index - Target position (0-based)
     * @returns {Object|null} State at index or null if invalid
     */
    goTo(index) {
        if (index >= 0 && index < this.states.length) {
            this.position = index;
        }
        return this.getCurrentState();
    }

    /**
     * Jump to first state in history
     * @returns {Object|null} First state or null if no states
     */
    goToStart() {
        if (this.states.length > 0) {
            this.position = 0;
        }
        return this.getCurrentState();
    }

    /**
     * Jump to last state in history
     * @returns {Object|null} Last state or null if no states
     */
    goToEnd() {
        if (this.states.length > 0) {
            this.position = this.states.length - 1;
        }
        return this.getCurrentState();
    }

    /**
     * Clear all history and reset to initial state
     */
    clear() {
        this.states = [];
        this.position = -1;
        this.totalPushed = 0;
    }

    /**
     * Get statistics for UI display
     * @returns {Object} Stats object with totalStates, currentPosition, totalPushed, memoryLimitReached
     */
    getStats() {
        return {
            totalStates: this.states.length,
            currentPosition: this.position,
            totalPushed: this.totalPushed,
            memoryLimitReached: this.totalPushed > this.maxStates,
            percentage: this.states.length > 0
                ? Math.round((this.position / (this.states.length - 1)) * 100)
                : 0
        };
    }

    /**
     * Get all states (for debugging or export)
     * Note: Returns reference to internal array - do not mutate!
     * @returns {Array} Array of frozen state objects
     */
    getAllStates() {
        return this.states;
    }

    /**
     * Get total number of states in history
     * @returns {number} Number of states
     */
    getLength() {
        return this.states.length;
    }
}

// ============================================================================
// Shared Instance Management
// ============================================================================

/**
 * Singleton instance for shared use across components
 * Use getSharedHistory() to access
 */
let sharedInstance = null;

/**
 * Get or create shared history instance
 * Useful for components that need to share the same history
 *
 * @param {number} maxStates - Maximum states to store (only used on first call)
 * @returns {SolverHistory} Shared instance
 */
function getSharedHistory(maxStates = 10000) {
    if (!sharedInstance) {
        sharedInstance = new SolverHistory(maxStates);
    }
    return sharedInstance;
}

/**
 * Reset shared history instance
 * Call this when starting a new solve to clear previous history
 */
function resetSharedHistory() {
    if (sharedInstance) {
        sharedInstance.clear();
    }
    sharedInstance = null;
}

// ============================================================================
// Testing Code (commented out - uncomment to run in browser console)
// ============================================================================

/*
// Test 1: Memory limit enforcement
console.log('=== Test 1: Memory Limit ===');
const history1 = new SolverHistory(100); // Small limit for testing
for (let i = 0; i < 200; i++) {
    history1.push({
        grid: [[i]],
        row: 0,
        col: 0,
        value: i % 9 + 1,
        iteration: i,
        depth: 0,
        isBacktrack: false
    });
}
console.assert(history1.states.length === 100, 'Should cap at 100 states');
console.assert(history1.totalPushed === 200, 'Should track total pushed');
console.assert(history1.getStats().memoryLimitReached === true, 'Should indicate memory limit reached');
console.log('✓ Memory limit test passed');

// Test 2: Navigation
console.log('\n=== Test 2: Navigation ===');
const history2 = new SolverHistory();
history2.push({ grid: [[1]], row: 0, col: 0, value: 1, iteration: 1, depth: 0, isBacktrack: false });
history2.push({ grid: [[2]], row: 0, col: 1, value: 2, iteration: 2, depth: 0, isBacktrack: false });
history2.push({ grid: [[3]], row: 0, col: 2, value: 3, iteration: 3, depth: 0, isBacktrack: false });

console.assert(history2.canStepBack() === true, 'Should be able to step back');
console.assert(history2.position === 2, 'Should be at position 2');

history2.stepBack();
console.assert(history2.getCurrentState().iteration === 2, 'Should be at iteration 2');
console.assert(history2.canStepForward() === true, 'Should be able to step forward');

history2.stepForward();
console.assert(history2.getCurrentState().iteration === 3, 'Should be at iteration 3');

history2.goToStart();
console.assert(history2.getCurrentState().iteration === 1, 'Should be at iteration 1');

history2.goToEnd();
console.assert(history2.getCurrentState().iteration === 3, 'Should be at iteration 3');
console.log('✓ Navigation test passed');

// Test 3: Immutability
console.log('\n=== Test 3: Immutability ===');
const history3 = new SolverHistory();
const originalGrid = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
history3.push({ grid: originalGrid, row: 0, col: 0, value: 1, iteration: 1, depth: 0, isBacktrack: false });

const state = history3.getCurrentState();
let immutable = false;
try {
    state.grid[0][0] = 999; // Should throw in strict mode or fail silently
    immutable = state.grid[0][0] !== 999; // If frozen, value won't change
} catch (e) {
    immutable = true; // Threw error because frozen
}
console.assert(immutable, 'Grid should be frozen/immutable');
console.log('✓ Immutability test passed');

// Test 4: Stats
console.log('\n=== Test 4: Stats ===');
const history4 = new SolverHistory(10);
for (let i = 0; i < 5; i++) {
    history4.push({ grid: [[i]], row: 0, col: 0, value: i, iteration: i, depth: 0, isBacktrack: false });
}
const stats = history4.getStats();
console.assert(stats.totalStates === 5, 'Should have 5 states');
console.assert(stats.currentPosition === 4, 'Should be at position 4');
console.assert(stats.totalPushed === 5, 'Should have pushed 5 states');
console.assert(stats.memoryLimitReached === false, 'Should not have reached limit');
console.assert(stats.percentage === 100, 'Should be at 100%');
console.log('✓ Stats test passed');

console.log('\n=== All Tests Passed ===');
*/

    // ======================================== solver-grid.js
// solver-grid.js - 3D Grid Renderer for Interactive Solver

class SolverGridRenderer {
    constructor(containerElement) {
        this.container = containerElement;
        this.cells = [];
        this.initialPuzzle = null; // To mark fixed cells
        this.currentAnimationSpeed = 1; // For adaptive animation duration
    }

    // Initialize the 9x9 grid
    initGrid() {
        this.container.innerHTML = `
            <div class="solver-grid-container">
                <div class="solver-grid">
                    ${Array(81).fill(0).map((_, i) => {
                        const row = Math.floor(i / 9);
                        const col = i % 9;
                        return `
                            <div class="solver-cell" data-row="${row}" data-col="${col}">
                                <span class="cell-value" data-value=""></span>
                            </div>
                        `;
                    }).join('')}
                </div>
            </div>
        `;
        this.cells = Array.from(this.container.querySelectorAll('.solver-cell'));
        this.gridContainer = this.container.querySelector('.solver-grid-container');
    }

    // Set initial puzzle (marks fixed cells)
    setInitialPuzzle(grid) {
        this.initialPuzzle = grid.map(row => [...row]);
        this.renderGrid(grid);

        // Mark fixed cells
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (grid[r][c] !== 0) {
                    const idx = r * 9 + c;
                    this.cells[idx].classList.add('fixed');
                }
            }
        }
    }

    // Render current grid state (values only, no animations)
    renderGrid(grid) {
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                const idx = r * 9 + c;
                const cell = this.cells[idx];
                const valueSpan = cell.querySelector('.cell-value');
                const val = grid[r][c];
                valueSpan.textContent = val === 0 ? '' : val;
                valueSpan.setAttribute('data-value', val === 0 ? '' : val);
            }
        }
    }

    // Render state with animations and styling
    // The state contains:
    // - grid: puzzle state BEFORE this iteration's attempt
    // - row, col: cell being tried
    // - value: value being attempted (1-9)
    // - iteration: C-reference compatible iteration count
    // - isBacktrack: true if this is a backtrack (removing a failed value)
    render(state, options = {}) {
        const { grid, row, col, value, isBacktrack, depth, isSolved } = state;
        const skipAnimation = options.skipAnimation || false;

        // Update all cell values from grid
        this.renderGrid(grid);

        // Clear previous active/backtrack states (except fixed)
        this.cells.forEach(cell => {
            cell.classList.remove('active', 'success', 'backtrack', 'spinning', 'spinning-reverse', 'chromatic', 'pink-glow', 'cooling-down', 'matrix-solve');
        });

        // Mark cells that have values in the grid as success (they were valid placements)
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (grid[r][c] !== 0) {
                    const idx = r * 9 + c;
                    const cell = this.cells[idx];
                    // Only mark as success if it's not a fixed (initial) cell
                    if (!cell.classList.contains('fixed')) {
                        cell.classList.add('success');
                    }
                }
            }
        }

        // Highlight active cell if specified
        if (row !== undefined && col !== undefined && row >= 0 && col >= 0) {
            const idx = row * 9 + col;
            const cell = this.cells[idx];
            const valueSpan = cell.querySelector('.cell-value');

            if (isBacktrack) {
                cell.classList.add('backtrack');
                cell.classList.remove('success'); // Remove success state on backtrack
                // On backtrack, show the value that failed
                if (value && valueSpan) {
                    valueSpan.textContent = value;
                    valueSpan.setAttribute('data-value', value);
                }
                if (!skipAnimation && this.currentAnimationSpeed <= 10) {
                    cell.classList.add('spinning-reverse');
                }
            } else {
                cell.classList.add('active');
                cell.classList.remove('success'); // Currently being tried, not yet confirmed

                // SYNC FIX: Show the value being tried in the cell
                // The grid snapshot is taken BEFORE the value is placed,
                // but state.value contains what we're trying at this iteration
                if (value && valueSpan) {
                    valueSpan.textContent = value;
                    valueSpan.setAttribute('data-value', value);
                }

                if (!skipAnimation && this.currentAnimationSpeed <= 10) {
                    cell.classList.add('spinning');
                }
            }
        }

        // If puzzle is solved, mark all non-fixed cells as success
        if (isSolved) {
            this.cells.forEach((cell, idx) => {
                if (!cell.classList.contains('fixed')) {
                    cell.classList.add('success');
                }
            });
        }
    }

    // Set animation speed multiplier (disables animations at high speeds)
    setAnimationSpeed(speed) {
        this.currentAnimationSpeed = speed;

        // Adjust animation durations based on speed
        const duration = Math.max(0.1, 0.6 / Math.sqrt(speed));
        this.container.style.setProperty('--spin-duration', `${duration}s`);
    }

    // Get cell element by row/col
    getCell(row, col) {
        return this.cells[row * 9 + col];
    }

    // Get grid container for effects
    getGridContainer() {
        return this.gridContainer;
    }

    // Cleanup
    cleanup() {
        this.container.innerHTML = '';
        this.cells = [];
        this.initialPuzzle = null;
    }
}

    // ======================================== solver-effects.js
// solver-effects.js - Glitch Effects for Interactive Solver

class GlitchEffects {
    constructor(gridContainer) {
        this.container = gridContainer;
        this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
        this.activeEffects = new Set();
        this.enabled = true;
    }

    // Enable/disable all effects
    setEnabled(enabled) {
        this.enabled = enabled;
    }

    // Screen shake effect
    // Trigger on deep backtracks (depth > threshold)
    screenShake(duration = 200) {
        if (!this.enabled || this.activeEffects.has('shake')) return;

        this.activeEffects.add('shake');
        this.container.classList.add('glitch-screen-shake');

        setTimeout(() => {
            this.container.classList.remove('glitch-screen-shake');
            this.activeEffects.delete('shake');
        }, duration);
    }

    // Alien character scramble on a specific cell
    // Shows random alien characters briefly before settling
    alienScramble(cellElement, finalValue, duration = 300) {
        if (!this.enabled) return;

        const valueSpan = cellElement.querySelector('.cell-value');
        if (!valueSpan) return;

        const startTime = Date.now();
        const originalValue = finalValue || valueSpan.textContent;

        const scrambleInterval = setInterval(() => {
            const elapsed = Date.now() - startTime;
            if (elapsed >= duration) {
                clearInterval(scrambleInterval);
                valueSpan.textContent = originalValue;
                valueSpan.setAttribute('data-value', originalValue);
                return;
            }

            // Random alien character
            const randomChar = this.aliens[Math.floor(Math.random() * this.aliens.length)];
            valueSpan.textContent = randomChar;
        }, 50);
    }

    // Chromatic aberration effect on cell
    chromaticAberration(cellElement, duration = 500) {
        if (!this.enabled) return;

        cellElement.classList.add('chromatic');

        setTimeout(() => {
            cellElement.classList.remove('chromatic');
        }, duration);
    }

    // Color inversion flash (entire grid)
    colorInvert(duration = 100) {
        if (!this.enabled) return;

        this.container.style.filter = 'invert(1)';

        setTimeout(() => {
            this.container.style.filter = '';
        }, duration);
    }

    // Trigger combined glitch based on intensity level
    // intensity: 0-1 (0 = subtle, 1 = maximum chaos)
    triggerGlitch(intensity, cellElement = null) {
        if (!this.enabled) return;

        if (intensity > 0.7) {
            this.screenShake(200 + intensity * 100);
        }

        if (intensity > 0.5 && cellElement) {
            this.chromaticAberration(cellElement, 300 + intensity * 200);
        }

        if (intensity > 0.8) {
            this.colorInvert(50);
        }
    }

    // Smart trigger based on solver state
    // Call this from animation controller when state changes
    onStateChange(state, prevState) {
        if (!this.enabled) return;

        const { isBacktrack, depth, row, col } = state;

        // Screen shake on deep backtrack
        if (isBacktrack && depth > 5) {
            const intensity = Math.min(1, depth / 15);
            this.screenShake(150 + intensity * 150);
        }

        // Alien scramble on backtrack
        if (isBacktrack && row !== undefined && col !== undefined) {
            const cellIdx = row * 9 + col;
            const cells = this.container.querySelectorAll('.solver-cell');
            if (cells[cellIdx]) {
                // Only scramble occasionally to avoid chaos
                if (Math.random() > 0.7) {
                    this.alienScramble(cells[cellIdx], '', 150);
                }
            }
        }
    }

    // Clear all active effects
    clear() {
        this.container.classList.remove('glitch-screen-shake');
        this.container.style.filter = '';
        this.container.querySelectorAll('.chromatic').forEach(el => {
            el.classList.remove('chromatic');
        });
        this.activeEffects.clear();
    }
}

// Factory function
function createGlitchEffects(gridContainer) {
    return new GlitchEffects(gridContainer);
}

    // ======================================== solver-animation.js
/**
 * Animation Controller for Interactive Solver
 *
 * Manages playback of solver state history using requestAnimationFrame
 * with delta-time based speed control (1x - 100x).
 *
 * Key Features:
 * - requestAnimationFrame loop with delta-time calculation
 * - Speed control from 1x to 100x
 * - Adaptive animation skipping at high speeds (>10x)
 * - Play/pause/step forward/back navigation
 * - Reset and skip to end controls
 * - Auto-pause at end of history
 *
 * Usage:
 *   const controller = new AnimationController(history, gridRenderer, effects);
 *   controller.onStateChange = (state, stats) => updateInfoDisplay(state, stats);
 *   controller.setSpeed(10);
 *   controller.play();
 */

class AnimationController {
    constructor(history, gridRenderer, effects = null) {
        this.history = history;         // SolverHistory instance
        this.gridRenderer = gridRenderer; // SolverGridRenderer instance
        this.effects = effects;         // GlitchEffects instance (optional)

        this.playing = false;
        this.speed = 1.0;               // 1x to 100x multiplier
        this.lastTimestamp = null;
        this.stepInterval = 100;        // ms between steps at 1x speed
        this.accumulated = 0;
        this.requestId = null;

        // Callbacks
        this.onStateChange = null;      // Called when state changes (for info display)
        this.onPlayStateChange = null;  // Called when play/pause changes
        this.onComplete = null;         // Called when animation reaches end
    }

    /**
     * Start animation playback
     */
    play() {
        if (this.playing) return;

        this.playing = true;
        this.lastTimestamp = null;
        this.accumulated = 0;

        if (this.onPlayStateChange) {
            this.onPlayStateChange(true);
        }

        this.requestId = requestAnimationFrame((t) => this.tick(t));
    }

    /**
     * Pause animation
     */
    pause() {
        this.playing = false;

        if (this.requestId) {
            cancelAnimationFrame(this.requestId);
            this.requestId = null;
        }

        if (this.onPlayStateChange) {
            this.onPlayStateChange(false);
        }
    }

    /**
     * Toggle play/pause
     */
    toggle() {
        if (this.playing) {
            this.pause();
        } else {
            this.play();
        }
    }

    /**
     * Animation loop tick using delta-time calculation
     * @param {number} timestamp - High-resolution timestamp from requestAnimationFrame
     */
    tick(timestamp) {
        if (!this.playing) return;

        if (this.lastTimestamp === null) {
            this.lastTimestamp = timestamp;
        }

        // Calculate delta time, scaled by speed multiplier
        const delta = (timestamp - this.lastTimestamp) * this.speed;
        this.lastTimestamp = timestamp;
        this.accumulated += delta;

        // Step forward when accumulated time exceeds interval
        while (this.accumulated >= this.stepInterval && this.playing) {
            if (this.history.canStepForward()) {
                const prevState = this.history.getCurrentState();
                const state = this.history.stepForward();

                // Determine if we should skip animations at high speeds
                const skipAnimation = this.speed > 10;
                this.gridRenderer.render(state, { skipAnimation });

                // Trigger effects based on state
                if (this.effects && !skipAnimation) {
                    this.effects.onStateChange(state, prevState);
                }

                // Notify listeners
                if (this.onStateChange) {
                    this.onStateChange(state, this.history.getStats());
                }
            } else {
                // Reached end of history
                this.pause();
                if (this.onComplete) {
                    this.onComplete();
                }
                return;
            }

            this.accumulated -= this.stepInterval;
        }

        // Continue animation loop
        this.requestId = requestAnimationFrame((t) => this.tick(t));
    }

    /**
     * Set speed multiplier (1x - 100x)
     * @param {number} multiplier - Speed multiplier
     */
    setSpeed(multiplier) {
        this.speed = Math.max(1, Math.min(100, multiplier));

        // Update grid renderer animation speed
        this.gridRenderer.setAnimationSpeed(this.speed);

        // Disable effects at very high speeds
        if (this.effects) {
            this.effects.setEnabled(this.speed <= 20);
        }
    }

    /**
     * Manual step forward (pauses animation)
     */
    stepForward() {
        this.pause();

        if (this.history.canStepForward()) {
            const prevState = this.history.getCurrentState();
            const state = this.history.stepForward();
            this.gridRenderer.render(state, { skipAnimation: false });

            if (this.effects) {
                this.effects.onStateChange(state, prevState);
            }

            if (this.onStateChange) {
                this.onStateChange(state, this.history.getStats());
            }
        }
    }

    /**
     * Manual step backward (pauses animation)
     */
    stepBackward() {
        this.pause();

        if (this.history.canStepBack()) {
            const state = this.history.stepBack();
            this.gridRenderer.render(state, { skipAnimation: false });

            if (this.onStateChange) {
                this.onStateChange(state, this.history.getStats());
            }
        }
    }

    /**
     * Jump to start (pauses animation)
     */
    reset() {
        this.pause();

        const state = this.history.goToStart();
        if (state) {
            this.gridRenderer.render(state, { skipAnimation: true });

            if (this.onStateChange) {
                this.onStateChange(state, this.history.getStats());
            }
        }
    }

    /**
     * Jump to end (pauses animation)
     */
    skipToEnd() {
        this.pause();

        const state = this.history.goToEnd();
        if (state) {
            this.gridRenderer.render(state, { skipAnimation: true });

            if (this.onStateChange) {
                this.onStateChange(state, this.history.getStats());
            }

            if (this.onComplete) {
                this.onComplete();
            }
        }
    }

    /**
     * Cleanup
     */
    cleanup() {
        this.pause();
        this.onStateChange = null;
        this.onPlayStateChange = null;
        this.onComplete = null;
    }
}

// ============================================================================
// Factory Function
// ============================================================================

/**
 * Factory function to create an AnimationController instance
 * @param {SolverHistory} history - Solver history instance
 * @param {SolverGridRenderer} gridRenderer - Grid renderer instance
 * @param {GlitchEffects} effects - Optional effects instance
 * @returns {AnimationController} New animation controller
 */
function createAnimationController(history, gridRenderer, effects = null) {
    return new AnimationController(history, gridRenderer, effects);
}

    // ======================================== solver-controls.js
/**
 * Playback Controls UI for Interactive Solver
 *
 * Provides user interface for controlling solver animation playback including:
 * - Play/Pause button
 * - Step Forward/Back buttons
 * - Reset and Skip to End buttons
 * - Speed slider (1x - 100x)
 * - Speed presets (1x, 10x, 50x, Max)
 * - Progress bar
 * - Live info display (iteration, cell, value, depth)
 *
 * Usage:
 *   const controls = new SolverControls(containerElement, animationController);
 *   controls.init();
 */

class SolverControls {
    constructor(containerElement, animationController) {
        this.container = containerElement;
        this.controller = animationController;
        this.isPlaying = false;
        this.currentSpeed = 1;

        // UI element references
        this.playPauseBtn = null;
        this.stepBackBtn = null;
        this.stepForwardBtn = null;
        this.resetBtn = null;
        this.skipToEndBtn = null;
        this.speedSlider = null;
        this.speedValue = null;
        this.progressBar = null;
        this.infoDisplay = null;

        // Bind methods
        this.handlePlayPause = this.handlePlayPause.bind(this);
        this.handleStepBack = this.handleStepBack.bind(this);
        this.handleStepForward = this.handleStepForward.bind(this);
        this.handleReset = this.handleReset.bind(this);
        this.handleSkipToEnd = this.handleSkipToEnd.bind(this);
        this.handleSpeedChange = this.handleSpeedChange.bind(this);
    }

    /**
     * Initialize controls UI
     */
    init() {
        this.container.innerHTML = `
            <div class="solver-controls">
                <div class="solver-controls-row">
                    <button class="solver-btn" id="solver-reset" title="Reset">
                        <span class="icon">⏮</span> Reset
                    </button>
                    <button class="solver-btn" id="solver-step-back" title="Step Back">
                        <span class="icon">⏪</span>
                    </button>
                    <button class="solver-btn primary" id="solver-play-pause" title="Play/Pause">
                        <span class="icon" id="play-icon">▶</span>
                    </button>
                    <button class="solver-btn" id="solver-step-forward" title="Step Forward">
                        <span class="icon">⏩</span>
                    </button>
                    <button class="solver-btn" id="solver-skip-end" title="Skip to End">
                        <span class="icon">⏭</span> End
                    </button>
                </div>

                <div class="solver-speed-container">
                    <span class="solver-speed-label">Speed:</span>
                    <input type="range" class="solver-speed-slider" id="solver-speed"
                           min="1" max="100" value="1" step="1">
                    <span class="solver-speed-value" id="solver-speed-value">1x</span>
                </div>

                <div class="solver-speed-presets">
                    <span class="solver-speed-preset active" data-speed="1">1x</span>
                    <span class="solver-speed-preset" data-speed="10">10x</span>
                    <span class="solver-speed-preset" data-speed="50">50x</span>
                    <span class="solver-speed-preset" data-speed="100">Max</span>
                </div>

                <div class="solver-progress">
                    <div class="solver-progress-bar" id="solver-progress"></div>
                </div>

                <div class="solver-info" id="solver-info">
                    <div class="solver-info-item">
                        <span class="solver-info-label">Iteration</span>
                        <span class="solver-info-value" id="info-iteration">0</span>
                    </div>
                    <div class="solver-info-item">
                        <span class="solver-info-label">Cell</span>
                        <span class="solver-info-value" id="info-cell">-</span>
                    </div>
                    <div class="solver-info-item">
                        <span class="solver-info-label">Value</span>
                        <span class="solver-info-value" id="info-value">-</span>
                    </div>
                    <div class="solver-info-item">
                        <span class="solver-info-label">Depth</span>
                        <span class="solver-info-value" id="info-depth">0</span>
                    </div>
                </div>
            </div>
        `;

        // Get references
        this.playPauseBtn = this.container.querySelector('#solver-play-pause');
        this.stepBackBtn = this.container.querySelector('#solver-step-back');
        this.stepForwardBtn = this.container.querySelector('#solver-step-forward');
        this.resetBtn = this.container.querySelector('#solver-reset');
        this.skipToEndBtn = this.container.querySelector('#solver-skip-end');
        this.speedSlider = this.container.querySelector('#solver-speed');
        this.speedValue = this.container.querySelector('#solver-speed-value');
        this.progressBar = this.container.querySelector('#solver-progress');
        this.infoDisplay = this.container.querySelector('#solver-info');

        // Add event listeners
        this.playPauseBtn.addEventListener('click', this.handlePlayPause);
        this.stepBackBtn.addEventListener('click', this.handleStepBack);
        this.stepForwardBtn.addEventListener('click', this.handleStepForward);
        this.resetBtn.addEventListener('click', this.handleReset);
        this.skipToEndBtn.addEventListener('click', this.handleSkipToEnd);
        this.speedSlider.addEventListener('input', this.handleSpeedChange);

        // Speed presets
        this.container.querySelectorAll('.solver-speed-preset').forEach(preset => {
            preset.addEventListener('click', () => {
                const speed = parseInt(preset.dataset.speed, 10);
                this.setSpeed(speed);
            });
        });

        // Register controller callbacks
        this.controller.onPlayStateChange = (playing) => {
            this.updatePlayState(playing);
        };

        this.controller.onStateChange = (state, stats) => {
            this.updateInfo(state, stats);
            this.updateProgress(stats);
        };

        this.controller.onComplete = () => {
            this.updatePlayState(false);
        };
    }

    // ========================================================================
    // Event Handlers
    // ========================================================================

    handlePlayPause() {
        this.controller.toggle();
    }

    handleStepBack() {
        this.controller.stepBackward();
    }

    handleStepForward() {
        this.controller.stepForward();
    }

    handleReset() {
        this.controller.reset();
    }

    handleSkipToEnd() {
        this.controller.skipToEnd();
    }

    handleSpeedChange(e) {
        const speed = parseInt(e.target.value, 10);
        this.setSpeed(speed);
    }

    // ========================================================================
    // UI Update Methods
    // ========================================================================

    /**
     * Set speed and update UI
     * @param {number} speed - Speed multiplier (1-100)
     */
    setSpeed(speed) {
        this.currentSpeed = speed;
        this.controller.setSpeed(speed);

        // Update slider
        this.speedSlider.value = speed;
        this.speedValue.textContent = speed === 100 ? 'Max' : `${speed}x`;

        // Update slider visual progress
        const progress = (speed - 1) / 99 * 100;
        this.speedSlider.style.setProperty('--progress', `${progress}%`);

        // Update preset highlights
        this.container.querySelectorAll('.solver-speed-preset').forEach(preset => {
            const presetSpeed = parseInt(preset.dataset.speed, 10);
            preset.classList.toggle('active', presetSpeed === speed);
        });
    }

    /**
     * Update play/pause button state
     * @param {boolean} playing - Whether animation is playing
     */
    updatePlayState(playing) {
        this.isPlaying = playing;
        const icon = this.playPauseBtn.querySelector('.icon');
        icon.textContent = playing ? '⏸' : '▶';
        this.playPauseBtn.title = playing ? 'Pause' : 'Play';
    }

    /**
     * Update info display
     * @param {Object} state - Current solver state
     * @param {Object} stats - History stats
     */
    updateInfo(state, stats) {
        if (!state) return;

        const iteration = this.container.querySelector('#info-iteration');
        const cell = this.container.querySelector('#info-cell');
        const value = this.container.querySelector('#info-value');
        const depth = this.container.querySelector('#info-depth');

        iteration.textContent = state.iteration.toLocaleString();
        cell.textContent = state.row !== undefined ? `(${state.row}, ${state.col})` : '-';
        value.textContent = state.value || '-';
        depth.textContent = state.depth || '0';

        // Color depth based on backtrack intensity
        if (state.depth > 5) {
            depth.style.color = '#ff0064';
        } else if (state.depth > 2) {
            depth.style.color = '#ff9d00';
        } else {
            depth.style.color = '#00ff9d';
        }
    }

    /**
     * Update progress bar
     * @param {Object} stats - History stats from SolverHistory.getStats()
     */
    updateProgress(stats) {
        if (!stats || stats.totalStates === 0) return;

        const progress = (stats.currentPosition / (stats.totalStates - 1)) * 100;
        this.progressBar.style.width = `${progress}%`;
    }

    /**
     * Enable/disable controls
     * @param {boolean} enabled - Whether controls should be enabled
     */
    setEnabled(enabled) {
        const buttons = [
            this.playPauseBtn,
            this.stepBackBtn,
            this.stepForwardBtn,
            this.resetBtn,
            this.skipToEndBtn
        ];

        buttons.forEach(btn => {
            if (btn) btn.disabled = !enabled;
        });

        if (this.speedSlider) {
            this.speedSlider.disabled = !enabled;
        }
    }

    /**
     * Cleanup
     */
    cleanup() {
        this.playPauseBtn?.removeEventListener('click', this.handlePlayPause);
        this.stepBackBtn?.removeEventListener('click', this.handleStepBack);
        this.stepForwardBtn?.removeEventListener('click', this.handleStepForward);
        this.resetBtn?.removeEventListener('click', this.handleReset);
        this.skipToEndBtn?.removeEventListener('click', this.handleSkipToEnd);
        this.speedSlider?.removeEventListener('input', this.handleSpeedChange);

        this.container.innerHTML = '';
    }
}

// ============================================================================
// Factory Function
// ============================================================================

/**
 * Factory function to create and initialize SolverControls
 * @param {HTMLElement} containerElement - Container element for controls
 * @param {AnimationController} animationController - Animation controller instance
 * @returns {SolverControls} Initialized controls instance
 */
function createSolverControls(containerElement, animationController) {
    const controls = new SolverControls(containerElement, animationController);
    controls.init();
    return controls;
}

    // ======================================== interactive-solver.js
// interactive-solver.js - Main Orchestrator for Interactive Solver


// Matrix puzzle data - will be populated from window.matrixPuzzles
const MATRIX_PUZZLES = {
    '1': null, // Loaded dynamically
    '2': null,
    '3': null,
    '4': null,
    '5': null,
    '6': null
};

// Reference iteration counts for validation
const REFERENCE_ITERATIONS = {
    '1': 656,
    '2': 439269,
    '3': 98847,
    '4': 9085,
    '5': 445778,
    '6': 622577597
};

class InteractiveSolver {
    constructor(containerElement) {
        this.container = containerElement;
        this.gridContainer = null;
        this.controlsContainer = null;
        this.statusContainer = null;

        // Module instances
        this.solver = null;
        this.history = null;
        this.gridRenderer = null;
        this.effects = null;
        this.animationController = null;
        this.controls = null;

        // State
        this.currentMatrix = '1';
        this.currentAlgorithm = 'BruteForce';
        this.isLoaded = false;
        this.isSolving = false;
    }

    // Initialize the UI structure
    init() {
        console.log('[Interactive Solver] init() called, container:', this.container);
        this.container.innerHTML = `
            <div class="solver-section">
                <div class="solver-header-row">
                    <div class="solver-header">
                        <h2>Interactive Solver</h2>
                        <p class="solver-subtitle">Watch the algorithm think</p>
                    </div>
                    <div class="solver-algo-description" id="algo-description">
                        <div class="algo-desc-header">How It Works</div>
                        <div class="algo-desc-content" id="algo-desc-content">
                            <strong>Brute Force Backtracking:</strong> Tries every possible number (1-9) in each empty cell, row by row. If a number violates Sudoku rules, it backtracks and tries the next number. Guaranteed to find a solution but explores many dead ends.
                        </div>
                    </div>
                </div>

                <div class="solver-setup">
                    <div class="solver-setup-row">
                        <label for="matrix-select">Matrix:</label>
                        <select id="matrix-select" class="solver-select">
                            <option value="1">Matrix 1 (656 iterations)</option>
                            <option value="2">Matrix 2 (439,269 iterations)</option>
                            <option value="3">Matrix 3 (98,847 iterations)</option>
                            <option value="4">Matrix 4 (9,085 iterations)</option>
                            <option value="5">Matrix 5 (445,778 iterations)</option>
                            <option value="6" disabled>Matrix 6 (622M - too large)</option>
                        </select>
                    </div>

                    <div class="solver-setup-row">
                        <label for="algorithm-select">Algorithm:</label>
                        <select id="algorithm-select" class="solver-select">
                            <option value="BruteForce" selected>Brute Force Backtracking</option>
                            <option value="DLX" disabled>Dancing Links (coming soon)</option>
                        </select>
                    </div>

                    <button id="start-solving" class="solver-btn primary solver-start-btn">
                        <span class="icon">▶</span> Start Solving
                    </button>
                </div>

                <div class="solver-status" id="solver-status" style="display: none;">
                    <span class="status-text"></span>
                </div>

                <div class="solver-live-stats" id="solver-live-stats" style="display: none;">
                    <div class="live-stat">
                        <span class="live-stat-label">Iterations:</span>
                        <span class="live-stat-value" id="live-iterations">0</span>
                    </div>
                    <div class="live-stat">
                        <span class="live-stat-label">Depth:</span>
                        <span class="live-stat-value" id="live-depth">0</span>
                    </div>
                    <div class="live-stat">
                        <span class="live-stat-label">Current Cell:</span>
                        <span class="live-stat-value" id="live-cell">-</span>
                    </div>
                </div>

                <div id="solver-grid-area"></div>

                <div id="solver-controls-area" style="display: none;"></div>

                <div class="solver-memory-warning" id="memory-warning" style="display: none;">
                    <span class="warning-icon">⚠</span>
                    <span class="warning-text">History limit reached. Older states have been discarded.</span>
                </div>
            </div>
        `;

        console.log('[Interactive Solver] HTML inserted, length:', this.container.innerHTML.length);

        // Get container references
        this.gridContainer = this.container.querySelector('#solver-grid-area');
        this.controlsContainer = this.container.querySelector('#solver-controls-area');
        this.statusContainer = this.container.querySelector('#solver-status');
        this.liveStatsContainer = this.container.querySelector('#solver-live-stats');

        console.log('[Interactive Solver] Containers found:', {
            grid: !!this.gridContainer,
            controls: !!this.controlsContainer,
            status: !!this.statusContainer
        });

        // Setup event listeners
        this.container.querySelector('#start-solving').addEventListener('click', () => {
            this.startSolving();
        });

        this.container.querySelector('#matrix-select').addEventListener('change', (e) => {
            this.currentMatrix = e.target.value;
            this.reset();
        });

        this.container.querySelector('#algorithm-select').addEventListener('change', (e) => {
            this.currentAlgorithm = e.target.value;
            this.updateAlgorithmDescription(e.target.value);
            this.reset();
        });

        // Load matrix data from window if available
        if (window.matrixPuzzles) {
            this.loadMatrixData(window.matrixPuzzles);
        }

        // Show initial puzzle immediately
        this.showInitialPuzzle();
    }

    // Load matrix puzzle data
    loadMatrixData(puzzles) {
        // puzzles is an array of puzzle strings (from HTMLGenerator)
        puzzles.forEach((puzzleStr, index) => {
            if (index < 6) {
                MATRIX_PUZZLES[String(index + 1)] = this.parseMatrixString(puzzleStr);
            }
        });
    }

    // Update algorithm description
    updateAlgorithmDescription(algo) {
        const descriptions = {
            'BruteForce': '<strong>Brute Force Backtracking:</strong> Tries every possible number (1-9) in each empty cell, row by row. If a number violates Sudoku rules, it backtracks and tries the next number. Guaranteed to find a solution but explores many dead ends.',
            'DLX': '<strong>Dancing Links (Algorithm X):</strong> Uses Donald Knuth\'s Algorithm X to efficiently solve the exact cover problem. Represents the puzzle as a matrix of constraints and uses clever pointer manipulation to quickly explore valid solutions. Much faster than brute force.'
        };

        const descEl = this.container.querySelector('#algo-desc-content');
        if (descEl && descriptions[algo]) {
            descEl.innerHTML = descriptions[algo];
        }
    }

    // Parse matrix string to 2D array
    parseMatrixString(matrixStr) {
        const lines = matrixStr.split('\n').filter(line => {
            const trimmed = line.trim();
            return trimmed && !trimmed.startsWith('#');
        });

        const grid = [];
        for (let i = 0; i < 9 && i < lines.length; i++) {
            const values = lines[i].trim().split(/\s+/).map(Number);
            if (values.length === 9) {
                grid.push(values);
            }
        }

        return grid.length === 9 ? grid : null;
    }

    // Show initial puzzle in grid
    showInitialPuzzle() {
        const puzzle = MATRIX_PUZZLES[this.currentMatrix];
        if (!puzzle) return;

        // Initialize grid renderer if not already done
        if (!this.gridRenderer) {
            this.gridRenderer = new SolverGridRenderer(this.gridContainer);
            this.gridRenderer.initGrid();
        }

        // Show the initial puzzle
        this.gridRenderer.setInitialPuzzle(puzzle);

        // Create a simple state representation for rendering
        const initialState = {
            grid: puzzle.map(row => [...row]),
            row: -1,
            col: -1,
            value: 0,
            iteration: 0
        };

        this.gridRenderer.render(initialState, { skipAnimation: true });
    }

    // Show status message
    showStatus(message, type = 'info') {
        this.statusContainer.style.display = 'block';
        const textEl = this.statusContainer.querySelector('.status-text');
        textEl.textContent = message;
        textEl.className = `status-text status-${type}`;
    }

    // Hide status
    hideStatus() {
        this.statusContainer.style.display = 'none';
    }

    // Start solving process
    async startSolving() {
        if (this.isSolving) return;

        const puzzle = MATRIX_PUZZLES[this.currentMatrix];
        if (!puzzle) {
            this.showStatus(`Matrix ${this.currentMatrix} data not loaded`, 'error');
            return;
        }

        this.isSolving = true;
        this.showStatus('Solving puzzle...', 'info');

        // Show live stats
        this.liveStatsContainer.style.display = 'flex';

        // Disable start button
        const startBtn = this.container.querySelector('#start-solving');
        startBtn.disabled = true;
        startBtn.innerHTML = '<span class="icon">⏳</span> Solving...';

        // Reset any previous state
        this.cleanupModules();

        // Initialize modules
        this.history = getSharedHistory(10000); // 10K state limit

        // Reuse or initialize grid renderer
        if (!this.gridRenderer) {
            this.gridRenderer = new SolverGridRenderer(this.gridContainer);
            this.gridRenderer.initGrid();
        }

        // Set initial puzzle
        this.gridRenderer.setInitialPuzzle(puzzle);

        // Initialize effects
        this.effects = createGlitchEffects(this.gridRenderer.getGridContainer());

        // Initialize solver
        this.solver = new BruteForceSolver();
        this.solver.loadPuzzle(puzzle);

        // Adaptive state sampling based on expected iteration count
        const expectedIterations = REFERENCE_ITERATIONS[this.currentMatrix];
        const maxStoredStates = 10000;
        const samplingInterval = Math.max(1, Math.floor(expectedIterations / maxStoredStates));

        console.log(`[Solver] Matrix ${this.currentMatrix}: Expected ${expectedIterations} iterations, sampling every ${samplingInterval} states`);

        // Show grid and initialize renderer for live updates
        this.gridRenderer.setInitialPuzzle(puzzle);

        // Live rendering variables
        let lastRenderTime = Date.now();
        const renderInterval = 50; // Render every 50ms for live animation

        // Capture states during solving with adaptive sampling AND live rendering
        let stateCounter = 0;
        this.solver.onStateChange = (state) => {
            stateCounter++;

            // Sample states for history (playback later)
            if (stateCounter === 1 || stateCounter % samplingInterval === 0 || state.isSolved) {
                this.history.push(state);
            }

            // Live render every 50ms for immediate visual feedback
            const now = Date.now();
            if (now - lastRenderTime > renderInterval || state.isSolved) {
                this.gridRenderer.render(state, { skipAnimation: true });

                // Update live stats
                const iterEl = this.container.querySelector('#live-iterations');
                const depthEl = this.container.querySelector('#live-depth');
                const cellEl = this.container.querySelector('#live-cell');

                if (iterEl) iterEl.textContent = state.iteration.toLocaleString();
                if (depthEl) {
                    depthEl.textContent = state.depth;
                    // Color depth based on value
                    if (state.depth > 5) {
                        depthEl.style.color = '#ff0064';
                    } else if (state.depth > 2) {
                        depthEl.style.color = '#ff9d00';
                    } else {
                        depthEl.style.color = '#00ff9d';
                    }
                }
                if (cellEl && state.row >= 0) {
                    cellEl.textContent = `(${state.row}, ${state.col}) = ${state.value}`;
                } else if (cellEl) {
                    cellEl.textContent = '-';
                }

                lastRenderTime = now;
            }
        };

        // Run solver (synchronous for small puzzles)
        // For large puzzles, we'd want to chunk this
        try {
            console.log(`[Solver] Starting to solve Matrix ${this.currentMatrix}...`);

            // Solve in chunks to keep UI responsive
            await this.solveInChunks();

            console.log(`[Solver] Solving complete. Iterations: ${this.solver.iteration}, States captured: ${this.history.getLength()}`);

            // Hide live stats now that we're done
            this.liveStatsContainer.style.display = 'none';

            // Validate iteration count
            if (this.solver.iteration !== expectedIterations) {
                console.warn(`[Solver] Iteration mismatch: expected ${expectedIterations}, got ${this.solver.iteration}`);
                this.showStatus(`⚠ Solved in ${this.solver.iteration.toLocaleString()} iterations (expected ${expectedIterations.toLocaleString()})`, 'warning');
            } else {
                this.showStatus(`✓ Solved in ${this.solver.iteration.toLocaleString()} iterations - Use controls to replay`, 'success');
            }

        } catch (error) {
            console.error('[Solver] Fatal error:', error);
            this.liveStatsContainer.style.display = 'none';
            this.showStatus(`Error: ${error.message}`, 'error');
            this.isSolving = false;
            startBtn.disabled = false;
            startBtn.innerHTML = '<span class="icon">▶</span> Start Solving';
            return;
        }

        // Go to start of history
        this.history.goToStart();

        // Initialize animation controller
        this.animationController = createAnimationController(
            this.history,
            this.gridRenderer,
            this.effects
        );

        // Initialize controls
        this.controlsContainer.style.display = 'block';
        this.controls = createSolverControls(this.controlsContainer, this.animationController);

        // Check if memory limit was reached
        const stats = this.history.getStats();
        if (stats.memoryLimitReached) {
            this.container.querySelector('#memory-warning').style.display = 'block';
        }

        // Update start button
        startBtn.innerHTML = '<span class="icon">🔄</span> Restart';
        startBtn.disabled = false;

        // Render initial state
        const initialState = this.history.getCurrentState();
        if (initialState) {
            this.gridRenderer.render(initialState, { skipAnimation: true });
            this.controls.updateInfo(initialState, stats);
        }

        this.isLoaded = true;
        this.isSolving = false;

        // Don't auto-play - user saw it live! But they can replay if they want
        this.showStatus('✓ Complete! Use controls below to replay', 'success');
    }

    // Solve puzzle asynchronously with UI updates
    async solveInChunks() {
        try {
            // Use async solver that yields every 100 iterations
            await this.solver.solveAsync(100);
        } catch (error) {
            console.error('[Solver] Error during solve:', error);
            throw error;
        }
    }

    // Reset to initial state
    reset() {
        // Only cleanup solving-related modules, keep grid renderer
        if (this.animationController) {
            this.animationController.cleanup();
            this.animationController = null;
        }

        if (this.controls) {
            this.controls.cleanup();
            this.controls = null;
        }

        if (this.effects) {
            this.effects.clear();
            this.effects = null;
        }

        resetSharedHistory();
        this.history = null;
        this.solver = null;

        this.controlsContainer.style.display = 'none';
        this.liveStatsContainer.style.display = 'none';
        this.container.querySelector('#memory-warning').style.display = 'none';
        this.hideStatus();

        const startBtn = this.container.querySelector('#start-solving');
        startBtn.disabled = false;
        startBtn.innerHTML = '<span class="icon">▶</span> Start Solving';

        this.isLoaded = false;

        // Show the new puzzle
        this.showInitialPuzzle();
    }

    // Cleanup module instances
    cleanupModules() {
        if (this.animationController) {
            this.animationController.cleanup();
            this.animationController = null;
        }

        if (this.controls) {
            this.controls.cleanup();
            this.controls = null;
        }

        if (this.effects) {
            this.effects.clear();
            this.effects = null;
        }

        if (this.gridRenderer) {
            this.gridRenderer.cleanup();
            this.gridRenderer = null;
        }

        resetSharedHistory();
        this.history = null;
        this.solver = null;
    }

    // Full cleanup when leaving tab
    cleanup() {
        this.cleanupModules();
        this.container.innerHTML = '';
    }
}

// Initialize interactive solver when DOM ready
function initInteractiveSolver() {
    console.log('[Interactive Solver] initInteractiveSolver called');
    const container = document.getElementById('interactive-solver-section');
    if (!container) {
        console.error('[Interactive Solver] Container not found!');
        return null;
    }

    console.log('[Interactive Solver] Container found, creating solver instance');
    const solver = new InteractiveSolver(container);
    solver.init();
    console.log('[Interactive Solver] Solver initialized successfully');

    // Store reference for cleanup
    window.interactiveSolver = solver;

    return solver;
}

// Note: No auto-initialization - solver is launched via modal from INFO menu
// The launchInteractiveSolver() function in HTMLGenerator.ts handles initialization

    // Expose to window
    window.InteractiveSolver = InteractiveSolver;
    window.initInteractiveSolver = initInteractiveSolver;
})(window);
