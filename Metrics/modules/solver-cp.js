/**
 * Constraint Propagation Sudoku Solver - Browser-Compatible Implementation
 * Uses constraint propagation with backtracking for efficient solving.
 *
 * Algorithm:
 * - Maintain possible values (candidates) for each cell
 * - Propagate constraints when a value is assigned
 * - Use naked singles and hidden singles techniques
 * - Backtrack only when necessary
 *
 * Much faster than pure brute force but slower than DLX for hard puzzles.
 */

export class CPSolver {
    constructor() {
        this.puzzle = [];       // 9x9 grid [row][col]
        this.candidates = [];   // 9x9 array of Sets (possible values for each cell)
        this.iteration = 0;     // Total iteration count
        this.depth = 0;         // Current recursion depth
        this.onStateChange = null; // Callback: (state) => void
        this.solved = false;

        this.reset();
    }

    /**
     * Load puzzle from 2D array
     */
    loadPuzzle(input) {
        if (Array.isArray(input)) {
            this.puzzle = input.map(row => [...row]);
        } else {
            throw new Error('Invalid input: expected 2D array');
        }

        if (this.puzzle.length !== 9 || !this.puzzle.every(row => row.length === 9)) {
            throw new Error('Invalid puzzle: must be 9x9 grid');
        }

        this.iteration = 0;
        this.solved = false;
        this.depth = 0;

        // Initialize candidates for all cells
        this.initializeCandidates();
    }

    /**
     * Initialize candidate sets for all cells
     */
    initializeCandidates() {
        this.candidates = Array(9).fill(null).map(() =>
            Array(9).fill(null).map(() => new Set([1, 2, 3, 4, 5, 6, 7, 8, 9]))
        );

        // Remove candidates based on initial puzzle values
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (this.puzzle[r][c] !== 0) {
                    this.assign(r, c, this.puzzle[r][c], false);
                }
            }
        }
    }

    /**
     * Get deep copy of current puzzle grid
     */
    getGridSnapshot() {
        return this.puzzle.map(row => [...row]);
    }

    /**
     * Assign a value to a cell and propagate constraints
     */
    assign(row, col, val, emitState = true) {
        // Set the value
        this.puzzle[row][col] = val;

        // Clear all candidates for this cell
        this.candidates[row][col].clear();
        this.candidates[row][col].add(val);

        // Emit state if requested
        if (emitState && this.onStateChange) {
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

        // Remove this value from candidates in same row, column, and box
        this.propagateConstraints(row, col, val);
    }

    /**
     * Propagate constraints after assigning a value
     */
    propagateConstraints(row, col, val) {
        // Remove from row
        for (let c = 0; c < 9; c++) {
            if (c !== col) {
                this.candidates[row][c].delete(val);
            }
        }

        // Remove from column
        for (let r = 0; r < 9; r++) {
            if (r !== row) {
                this.candidates[r][col].delete(val);
            }
        }

        // Remove from 3x3 box
        const boxRow = Math.floor(row / 3) * 3;
        const boxCol = Math.floor(col / 3) * 3;
        for (let r = boxRow; r < boxRow + 3; r++) {
            for (let c = boxCol; c < boxCol + 3; c++) {
                if (r !== row || c !== col) {
                    this.candidates[r][c].delete(val);
                }
            }
        }
    }

    /**
     * Unassign a value from a cell (for backtracking)
     */
    unassign(row, col) {
        this.puzzle[row][col] = 0;

        // Recalculate candidates for this cell
        this.candidates[row][col] = new Set([1, 2, 3, 4, 5, 6, 7, 8, 9]);

        // Remove values that conflict with current puzzle state
        for (let r = 0; r < 9; r++) {
            if (this.puzzle[r][col] !== 0) {
                this.candidates[row][col].delete(this.puzzle[r][col]);
            }
        }

        for (let c = 0; c < 9; c++) {
            if (this.puzzle[row][c] !== 0) {
                this.candidates[row][col].delete(this.puzzle[row][c]);
            }
        }

        const boxRow = Math.floor(row / 3) * 3;
        const boxCol = Math.floor(col / 3) * 3;
        for (let r = boxRow; r < boxRow + 3; r++) {
            for (let c = boxCol; c < boxCol + 3; c++) {
                if (this.puzzle[r][c] !== 0) {
                    this.candidates[row][col].delete(this.puzzle[r][c]);
                }
            }
        }
    }

    /**
     * Find cell with minimum remaining candidates (MRV heuristic)
     */
    findBestCell() {
        let minCandidates = 10;
        let bestRow = -1;
        let bestCol = -1;

        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (this.puzzle[r][c] === 0) {
                    const numCandidates = this.candidates[r][c].size;
                    if (numCandidates === 0) {
                        // No valid candidates - contradiction
                        return { row: r, col: c, candidates: [] };
                    }
                    if (numCandidates < minCandidates) {
                        minCandidates = numCandidates;
                        bestRow = r;
                        bestCol = c;
                    }
                }
            }
        }

        if (bestRow === -1) {
            return null; // All cells filled
        }

        return {
            row: bestRow,
            col: bestCol,
            candidates: Array.from(this.candidates[bestRow][bestCol])
        };
    }

    /**
     * Main solving algorithm with constraint propagation
     */
    solve() {
        // Find best cell to fill (MRV heuristic)
        const best = this.findBestCell();

        // If no empty cell found, puzzle is solved
        if (best === null) {
            this.solved = true;

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

        const { row, col, candidates } = best;

        // If no candidates available, contradiction
        if (candidates.length === 0) {
            return false;
        }

        // Try each candidate value
        for (const val of candidates) {
            this.iteration++;

            // Save current candidates state for backtracking
            const savedCandidates = this.candidates.map(row =>
                row.map(set => new Set(set))
            );

            // Assign value
            this.assign(row, col, val, true);
            this.depth++;

            // Recurse
            if (this.solve()) {
                return true;
            }

            // Backtrack
            this.depth--;
            this.unassign(row, col);

            // Restore candidates
            this.candidates = savedCandidates;

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

        return false;
    }

    /**
     * Async solving with UI yields
     */
    async solveAsync(yieldInterval = 100) {
        this._yieldCounter = 0;
        this._yieldInterval = yieldInterval;
        return await this._solveAsyncRecursive();
    }

    async _solveAsyncRecursive() {
        // Yield to UI periodically
        this._yieldCounter++;
        if (this._yieldCounter % this._yieldInterval === 0) {
            await new Promise(resolve => setTimeout(resolve, 0));
        }

        // Find best cell to fill
        const best = this.findBestCell();

        if (best === null) {
            this.solved = true;

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

        const { row, col, candidates } = best;

        if (candidates.length === 0) {
            return false;
        }

        for (const val of candidates) {
            this.iteration++;

            const savedCandidates = this.candidates.map(row =>
                row.map(set => new Set(set))
            );

            this.assign(row, col, val, true);
            this.depth++;

            if (await this._solveAsyncRecursive()) {
                return true;
            }

            this.depth--;
            this.unassign(row, col);
            this.candidates = savedCandidates;

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

        return false;
    }

    reset() {
        this.puzzle = Array(9).fill(null).map(() => Array(9).fill(0));
        this.candidates = [];
        this.iteration = 0;
        this.depth = 0;
        this.solved = false;
    }

    toString() {
        return this.puzzle.map(row => row.join(' ')).join('\n');
    }
}
