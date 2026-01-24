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
    render(state, options = {}) {
        const { grid, row, col, value, isBacktrack, depth } = state;
        const skipAnimation = options.skipAnimation || false;

        // Update all cell values
        this.renderGrid(grid);

        // Clear previous active/backtrack states (except fixed)
        this.cells.forEach(cell => {
            cell.classList.remove('active', 'success', 'backtrack', 'spinning', 'spinning-reverse', 'chromatic');
        });

        // Highlight active cell if specified
        if (row !== undefined && col !== undefined && row >= 0 && col >= 0) {
            const idx = row * 9 + col;
            const cell = this.cells[idx];

            if (isBacktrack) {
                cell.classList.add('backtrack');
                if (!skipAnimation && this.currentAnimationSpeed <= 10) {
                    cell.classList.add('spinning-reverse');
                }
            } else {
                cell.classList.add('active');
                if (!skipAnimation && this.currentAnimationSpeed <= 10) {
                    cell.classList.add('spinning');
                }

                // Success if value was placed (non-zero in grid at that position)
                if (grid[row][col] !== 0) {
                    // Transition to success after spin
                    setTimeout(() => {
                        cell.classList.remove('active');
                        cell.classList.add('success');
                    }, skipAnimation ? 0 : 300);
                }
            }
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
