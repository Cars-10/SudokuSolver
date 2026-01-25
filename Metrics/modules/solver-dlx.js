/**
 * DLX (Dancing Links) Sudoku Solver - Browser-Compatible Implementation
 * Based on Knuth's Algorithm X with Dancing Links technique.
 *
 * Algorithm:
 * - Exact cover problem solved with Algorithm X
 * - Dancing Links technique for efficient backtracking
 * - Minimum column size heuristic
 * - State emission for visualization
 */

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
// DLX SOLVER CLASS
// ============================================================================

export class DLXSolver {
    constructor() {
        this.puzzle = [];
        this.solutionGrid = [];
        this.iteration = 0;
        this.depth = 0;
        this.onStateChange = null; // Callback for state emission
        this.solved = false;

        // DLX structures
        this.root = null;
        this.columns = [];
        this.rowInfo = [];
        this.rowStarts = [];

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
        this.solutionGrid = this.puzzle.map(row => [...row]);
    }

    /**
     * Get deep copy of current puzzle grid
     */
    getGridSnapshot() {
        return this.solutionGrid.map(row => [...row]);
    }

    // ========================================================================
    // CONSTRAINT COLUMN INDICES
    // ========================================================================

    getPositionCol(r, c) {
        return r * 9 + c;
    }

    getRowCol(r, n) {
        return 81 + r * 9 + (n - 1);
    }

    getColCol(c, n) {
        return 162 + c * 9 + (n - 1);
    }

    getBoxCol(r, c, n) {
        const box = Math.floor(r / 3) * 3 + Math.floor(c / 3);
        return 243 + box * 9 + (n - 1);
    }

    // ========================================================================
    // DLX CORE OPERATIONS
    // ========================================================================

    coverColumn(c) {
        const colNode = c.node;

        // Remove column header from the header list
        colNode.right.left = colNode.left;
        colNode.left.right = colNode.right;

        // For each row in this column
        let rowNode = colNode.down;
        while (rowNode !== colNode) {
            // For each node in this row
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

    uncoverColumn(c) {
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

    chooseColumn() {
        const rootNode = this.root.node;
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

    dlxSearch(k, solution) {
        this.iteration++;

        const rootNode = this.root.node;

        // If matrix is empty, we found a solution
        if (rootNode.right === rootNode) {
            this.extractSolution(solution, k);
            this.solved = true;

            // Emit final state
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

        // Choose column with minimum size
        const col = this.chooseColumn();

        // If column has no rows, no solution possible
        if (col.size === 0) {
            return false;
        }

        // Cover this column
        this.coverColumn(col);

        // Try each row in this column
        let rowNode = col.node.down;
        while (rowNode !== col.node) {
            // Add row to partial solution
            solution[k] = rowNode.rowId;

            // Update solution grid for visualization
            const info = this.rowInfo[rowNode.rowId];
            if (info) {
                this.solutionGrid[info.row][info.col] = info.num;

                // Emit state
                if (this.onStateChange) {
                    this.onStateChange({
                        grid: this.getGridSnapshot(),
                        row: info.row,
                        col: info.col,
                        value: info.num,
                        iteration: this.iteration,
                        depth: k,
                        isBacktrack: false,
                        isSolved: false
                    });
                }
            }

            // Cover all other columns in this row
            let rightNode = rowNode.right;
            while (rightNode !== rowNode) {
                this.coverColumn(rightNode.column);
                rightNode = rightNode.right;
            }

            this.depth++;

            // Recurse
            if (this.dlxSearch(k + 1, solution)) {
                return true;
            }

            this.depth--;

            // Backtrack: uncover all columns in this row
            let leftNode = rowNode.left;
            while (leftNode !== rowNode) {
                this.uncoverColumn(leftNode.column);
                leftNode = leftNode.left;
            }

            // Restore grid for backtrack visualization
            if (info && this.puzzle[info.row][info.col] === 0) {
                this.solutionGrid[info.row][info.col] = 0;

                // Emit backtrack state
                if (this.onStateChange) {
                    this.onStateChange({
                        grid: this.getGridSnapshot(),
                        row: info.row,
                        col: info.col,
                        value: info.num,
                        iteration: this.iteration,
                        depth: k,
                        isBacktrack: true,
                        isSolved: false
                    });
                }
            }

            rowNode = rowNode.down;
        }

        // Uncover column
        this.uncoverColumn(col);

        return false;
    }

    // ========================================================================
    // DLX MATRIX CONSTRUCTION
    // ========================================================================

    initDlxMatrix() {
        // Create root column
        this.root = new DlxColumn("root");

        // Create 324 column headers
        this.columns = [];
        for (let i = 0; i < 324; i++) {
            const col = new DlxColumn(`C${i}`);

            // Link into header list
            col.node.left = this.root.node.left;
            col.node.right = this.root.node;
            this.root.node.left.right = col.node;
            this.root.node.left = col.node;

            this.columns.push(col);
        }

        this.rowInfo = [];
        this.rowStarts = [];
    }

    addNode(col, rowId) {
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

    buildDlxRow(r, c, n, rowId) {
        // Store row metadata
        this.rowInfo[rowId] = { row: r, col: c, num: n };

        // Create nodes for the 4 constraints
        const n1 = this.addNode(this.columns[this.getPositionCol(r, c)], rowId);
        const n2 = this.addNode(this.columns[this.getRowCol(r, n)], rowId);
        const n3 = this.addNode(this.columns[this.getColCol(c, n)], rowId);
        const n4 = this.addNode(this.columns[this.getBoxCol(r, c, n)], rowId);

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
        this.rowStarts[rowId] = n1;
    }

    buildDlxMatrixFromPuzzle() {
        let rowId = 0;

        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (this.puzzle[r][c] !== 0) {
                    // Cell has a clue - create only one row for that value
                    this.buildDlxRow(r, c, this.puzzle[r][c], rowId++);
                } else {
                    // Cell is empty - create rows for all possible values
                    for (let n = 1; n <= 9; n++) {
                        this.buildDlxRow(r, c, n, rowId++);
                    }
                }
            }
        }
    }

    coverClues() {
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                if (this.puzzle[r][c] !== 0) {
                    const n = this.puzzle[r][c];

                    // Find the row for this clue
                    for (let rowId = 0; rowId < this.rowStarts.length; rowId++) {
                        if (this.rowStarts[rowId] &&
                            this.rowInfo[rowId].row === r &&
                            this.rowInfo[rowId].col === c &&
                            this.rowInfo[rowId].num === n) {

                            // Cover all columns in this row
                            let node = this.rowStarts[rowId];
                            let curr = node;
                            do {
                                this.coverColumn(curr.column);
                                curr = curr.right;
                            } while (curr !== node);
                            break;
                        }
                    }
                }
            }
        }
    }

    extractSolution(solution, solutionLen) {
        // Initialize solution grid with original puzzle
        for (let r = 0; r < 9; r++) {
            for (let c = 0; c < 9; c++) {
                this.solutionGrid[r][c] = this.puzzle[r][c];
            }
        }

        // Each solution entry is a row_id
        for (let i = 0; i < solutionLen; i++) {
            const rowId = solution[i];
            if (rowId >= 0 && rowId < 729 && this.rowInfo[rowId]) {
                this.solutionGrid[this.rowInfo[rowId].row][this.rowInfo[rowId].col] = this.rowInfo[rowId].num;
            }
        }
    }

    // ========================================================================
    // SOLVING INTERFACE
    // ========================================================================

    /**
     * Synchronous solve
     */
    solve() {
        // Initialize DLX matrix
        this.initDlxMatrix();

        // Build matrix from puzzle
        this.buildDlxMatrixFromPuzzle();

        // Cover pre-filled clues
        this.coverClues();

        // Solve using DLX
        this.iteration = 0;
        const solution = new Array(81);
        return this.dlxSearch(0, solution);
    }

    /**
     * Async solving with UI yields
     */
    async solveAsync(yieldInterval = 100) {
        this._yieldCounter = 0;
        this._yieldInterval = yieldInterval;

        // Initialize DLX matrix
        this.initDlxMatrix();
        this.buildDlxMatrixFromPuzzle();
        this.coverClues();

        this.iteration = 0;
        const solution = new Array(81);
        return await this._dlxSearchAsync(0, solution);
    }

    async _dlxSearchAsync(k, solution) {
        this.iteration++;

        // Yield to UI periodically
        this._yieldCounter++;
        if (this._yieldCounter % this._yieldInterval === 0) {
            await new Promise(resolve => setTimeout(resolve, 0));
        }

        const rootNode = this.root.node;

        if (rootNode.right === rootNode) {
            this.extractSolution(solution, k);
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

        const col = this.chooseColumn();
        if (col.size === 0) return false;

        this.coverColumn(col);

        let rowNode = col.node.down;
        while (rowNode !== col.node) {
            solution[k] = rowNode.rowId;

            const info = this.rowInfo[rowNode.rowId];
            if (info) {
                this.solutionGrid[info.row][info.col] = info.num;

                if (this.onStateChange) {
                    this.onStateChange({
                        grid: this.getGridSnapshot(),
                        row: info.row,
                        col: info.col,
                        value: info.num,
                        iteration: this.iteration,
                        depth: k,
                        isBacktrack: false,
                        isSolved: false
                    });
                }
            }

            let rightNode = rowNode.right;
            while (rightNode !== rowNode) {
                this.coverColumn(rightNode.column);
                rightNode = rightNode.right;
            }

            this.depth++;

            if (await this._dlxSearchAsync(k + 1, solution)) {
                return true;
            }

            this.depth--;

            let leftNode = rowNode.left;
            while (leftNode !== rowNode) {
                this.uncoverColumn(leftNode.column);
                leftNode = leftNode.left;
            }

            if (info && this.puzzle[info.row][info.col] === 0) {
                this.solutionGrid[info.row][info.col] = 0;

                if (this.onStateChange) {
                    this.onStateChange({
                        grid: this.getGridSnapshot(),
                        row: info.row,
                        col: info.col,
                        value: info.num,
                        iteration: this.iteration,
                        depth: k,
                        isBacktrack: true,
                        isSolved: false
                    });
                }
            }

            rowNode = rowNode.down;
        }

        this.uncoverColumn(col);
        return false;
    }

    reset() {
        this.puzzle = Array(9).fill(null).map(() => Array(9).fill(0));
        this.solutionGrid = Array(9).fill(null).map(() => Array(9).fill(0));
        this.iteration = 0;
        this.depth = 0;
        this.solved = false;
        this.root = null;
        this.columns = [];
        this.rowInfo = [];
        this.rowStarts = [];
    }
}
