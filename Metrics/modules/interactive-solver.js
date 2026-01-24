// interactive-solver.js - Main Orchestrator for Interactive Solver

import { BruteForceSolver } from './solver-engine.js';
import { SolverHistory, getSharedHistory, resetSharedHistory } from './solver-state.js';
import { SolverGridRenderer } from './solver-grid.js';
import { GlitchEffects, createGlitchEffects } from './solver-effects.js';
import { AnimationController, createAnimationController } from './solver-animation.js';
import { SolverControls, createSolverControls } from './solver-controls.js';

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

export class InteractiveSolver {
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
                <div class="solver-header">
                    <h2>Interactive Solver</h2>
                    <p class="solver-subtitle">Watch the algorithm think</p>
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
                            <option value="BruteForce">Brute Force Backtracking</option>
                        </select>
                    </div>

                    <button id="start-solving" class="solver-btn primary solver-start-btn">
                        <span class="icon">▶</span> Start Solving
                    </button>
                </div>

                <div class="solver-status" id="solver-status" style="display: none;">
                    <span class="status-text"></span>
                </div>

                <div id="solver-grid-area" style="display: none;"></div>

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
            this.reset();
        });

        // Load matrix data from window if available
        if (window.matrixPuzzles) {
            this.loadMatrixData(window.matrixPuzzles);
        }
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

        // Disable start button
        const startBtn = this.container.querySelector('#start-solving');
        startBtn.disabled = true;
        startBtn.innerHTML = '<span class="icon">⏳</span> Solving...';

        // Reset any previous state
        this.cleanupModules();

        // Initialize modules
        this.history = getSharedHistory(10000); // 10K state limit

        // Initialize grid renderer
        this.gridRenderer = new SolverGridRenderer(this.gridContainer);
        this.gridRenderer.initGrid();
        this.gridContainer.style.display = 'block';

        // Set initial puzzle
        this.gridRenderer.setInitialPuzzle(puzzle);

        // Initialize effects
        this.effects = createGlitchEffects(this.gridRenderer.getGridContainer());

        // Initialize solver
        this.solver = new BruteForceSolver();
        this.solver.loadPuzzle(puzzle);

        // Capture states during solving
        this.solver.onStateChange = (state) => {
            this.history.push(state);
        };

        // Run solver (synchronous for small puzzles)
        // For large puzzles, we'd want to chunk this
        const expectedIterations = REFERENCE_ITERATIONS[this.currentMatrix];

        try {
            // Solve in chunks to keep UI responsive
            await this.solveInChunks();

            // Validate iteration count
            if (this.solver.iteration !== expectedIterations) {
                console.warn(`Iteration mismatch: expected ${expectedIterations}, got ${this.solver.iteration}`);
            }

            this.showStatus(`Solved in ${this.solver.iteration.toLocaleString()} iterations`, 'success');

        } catch (error) {
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
    }

    // Solve puzzle in chunks to keep UI responsive
    solveInChunks() {
        return new Promise((resolve, reject) => {
            // For Matrix 1-5, synchronous solving is fast enough
            // For very large puzzles, we'd chunk this differently
            try {
                this.solver.solve();
                resolve();
            } catch (error) {
                reject(error);
            }
        });
    }

    // Reset to initial state
    reset() {
        this.cleanupModules();
        this.gridContainer.style.display = 'none';
        this.controlsContainer.style.display = 'none';
        this.container.querySelector('#memory-warning').style.display = 'none';
        this.hideStatus();

        const startBtn = this.container.querySelector('#start-solving');
        startBtn.disabled = false;
        startBtn.innerHTML = '<span class="icon">▶</span> Start Solving';

        this.isLoaded = false;
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
export function initInteractiveSolver() {
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
