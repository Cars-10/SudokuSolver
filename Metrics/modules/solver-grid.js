// solver-grid.js - 3D Grid Renderer for Interactive Solver

export class SolverGridRenderer {
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

    // Matrix-style solve animation with expanding, spinning, and random characters
    animateMatrixSolve(cell, finalValue) {
        const valueSpan = cell.querySelector('.cell-value');
        if (!valueSpan) return;

        // Matrix characters (Japanese katakana)
        const matrixChars = ['ア', 'イ', 'ウ', 'エ', 'オ', 'カ', 'キ', 'ク', 'ケ', 'コ', 'サ', 'シ', 'ス', 'セ', 'ソ', 'タ', 'チ', 'ツ', 'テ', 'ト'];

        // Start spinning and showing random characters
        cell.classList.add('matrix-solve');

        let charIndex = 0;
        const charInterval = setInterval(() => {
            const randomChar = matrixChars[Math.floor(Math.random() * matrixChars.length)];
            valueSpan.textContent = randomChar;
            charIndex++;

            if (charIndex > 10) { // Show ~10 random characters for better effect
                clearInterval(charInterval);

                // Show final value and transition
                valueSpan.textContent = finalValue;

                setTimeout(() => {
                    cell.classList.remove('active', 'spinning', 'matrix-solve');
                    cell.classList.add('pink-glow');

                    // Cool down to green
                    setTimeout(() => {
                        cell.classList.remove('pink-glow');
                        cell.classList.add('cooling-down');

                        // Final green state
                        setTimeout(() => {
                            cell.classList.remove('cooling-down');
                            cell.classList.add('success');
                        }, 400);
                    }, 150);
                }, 100);
            }
        }, 40); // Change character every 40ms for faster cycling
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
