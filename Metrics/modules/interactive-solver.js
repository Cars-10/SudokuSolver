// interactive-solver.js - Main Orchestrator for Interactive Solver

import { BruteForceSolver } from './solver-engine.js';
import { DLXSolver } from './solver-dlx.js';
import { CPSolver } from './solver-cp.js';
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
                <!-- Two-column header -->
                <div class="solver-header-grid" style="grid-template-columns: 1fr 1fr; gap: 2rem;">
                    <div class="solver-header-title">
                        <h2>Interactive <span class="persona-word">Solver</span></h2>
                        <p class="solver-subtitle">Watch the algorithm think</p>
                    </div>

                    <div class="solver-header-description">
                        <h4>How It Works</h4>
                        <p id="algo-desc-content">
                            <strong>Brute Force Backtracking:</strong> Tries every possible number (1-9) in each empty cell, row by row. If a number violates Sudoku rules, it backtracks and tries the next number. Guaranteed to find a solution but explores many dead ends.
                        </p>
                    </div>
                </div>

                <!-- Main content area: Grid on left, Controls on right -->
                <div class="solver-main-content">
                    <div class="solver-grid-section">
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
                    </div>

                    <div class="solver-controls-section">
                        <!-- Matrix and Algorithm selection above controls -->
                        <div class="solver-header-controls" style="margin-bottom: 1rem; padding: 1rem; background: rgba(0, 255, 157, 0.05); border: 1px solid rgba(0, 255, 157, 0.2); border-radius: 8px;">
                            <div class="solver-control-group">
                                <label>Matrix:</label>
                                <select id="matrix-select" class="solver-select">
                                    <option value="1">Matrix 1 (656 iterations)</option>
                                    <option value="2">Matrix 2 (439,269 iterations)</option>
                                    <option value="3">Matrix 3 (98,847 iterations)</option>
                                    <option value="4">Matrix 4 (9,085 iterations)</option>
                                    <option value="5">Matrix 5 (445,778 iterations)</option>
                                    <option value="6" disabled>Matrix 6 (622M - too large)</option>
                                </select>
                            </div>
                            <div class="solver-control-group">
                                <label>Algorithm:</label>
                                <select id="algorithm-select" class="solver-select">
                                    <option value="BruteForce" selected>Brute Force</option>
                                    <option value="CP">Constraint Propagation</option>
                                    <option value="DLX">Dancing Links (DLX)</option>
                                </select>
                            </div>
                        </div>

                        <!-- Controls area (visible from start) -->
                        <div id="solver-controls-area"></div>

                        <div class="solver-memory-warning" id="memory-warning" style="display: none;">
                            <span class="warning-icon">⚠</span>
                            <span class="warning-text">History limit reached. Older states have been discarded.</span>
                        </div>
                    </div>
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

        // Initialize controls immediately (before solving)
        this.initializeControls();
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
            'CP': '<strong>Constraint Propagation:</strong> Maintains candidate values for each cell and propagates constraints when values are assigned. Uses the Minimum Remaining Values (MRV) heuristic to choose the best cell to fill next. Much smarter than brute force, reducing search space by 10-100x.',
            'DLX': '<strong>Dancing Links (Algorithm X):</strong> Uses Donald Knuth\'s Algorithm X with Dancing Links to efficiently solve the exact cover problem. Represents the puzzle as a 324-column constraint matrix and uses circular doubly-linked lists for fast covering/uncovering operations. Typically 10-100x faster than brute force.'
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

    // Initialize controls before solving (with Play button to start)
    initializeControls() {
        console.log('[Interactive Solver] Initializing controls');

        // Create a minimal history with just the initial state
        this.history = getSharedHistory(10000);

        const puzzle = MATRIX_PUZZLES[this.currentMatrix];
        if (puzzle) {
            const initialState = {
                grid: puzzle.map(row => [...row]),
                row: -1,
                col: -1,
                value: 0,
                iteration: 0,
                depth: 0,
                isBacktrack: false,
                isSolved: false
            };
            this.history.push(initialState);
        }

        // Initialize grid renderer if needed
        if (!this.gridRenderer) {
            this.gridRenderer = new SolverGridRenderer(this.gridContainer);
            this.gridRenderer.initGrid();
        }

        // Initialize effects
        if (!this.effects) {
            this.effects = createGlitchEffects(this.gridRenderer.getGridContainer());
        }

        // Initialize animation controller
        this.animationController = createAnimationController(
            this.history,
            this.gridRenderer,
            this.effects
        );

        // Initialize controls with a custom onPlay callback
        this.controls = createSolverControls(
            this.controlsContainer,
            this.animationController,
            () => {
                // Custom Play button handler - start solving if not yet solved
                if (!this.isLoaded && !this.isSolving) {
                    console.log('[Interactive Solver] Play button clicked - starting solve');
                    this.startSolving();
                    return true; // Indicate that we handled the action
                }
                return false; // Normal playback should proceed
            }
        );

        // Set initial speed to 1x to show animations when solving starts
        // User can adjust speed during playback
        if (this.controls) {
            this.controls.setSpeed(1);
        }

        console.log('[Interactive Solver] Controls initialized');
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

        // Disable controls during solving
        if (this.controls) {
            this.controls.setEnabled(false);
        }

        // Reset any previous state (but keep controls)
        this.cleanupSolvingState();

        // Initialize modules
        this.history = getSharedHistory(10000); // 10K state limit

        // Reuse or initialize grid renderer
        if (!this.gridRenderer) {
            this.gridRenderer = new SolverGridRenderer(this.gridContainer);
            this.gridRenderer.initGrid();
        }

        // Set initial puzzle
        this.gridRenderer.setInitialPuzzle(puzzle);

        // Set animation speed to 1x for live solving (to show matrix animations)
        this.gridRenderer.setAnimationSpeed(1);

        // Initialize effects
        this.effects = createGlitchEffects(this.gridRenderer.getGridContainer());

        // Initialize solver based on selected algorithm
        if (this.currentAlgorithm === 'DLX') {
            this.solver = new DLXSolver();
        } else if (this.currentAlgorithm === 'CP') {
            this.solver = new CPSolver();
        } else {
            this.solver = new BruteForceSolver();
        }
        this.solver.loadPuzzle(puzzle);

        // Adaptive state sampling based on expected iteration count (BruteForce reference)
        const expectedIterations = REFERENCE_ITERATIONS[this.currentMatrix];
        const maxStoredStates = 10000;
        const samplingInterval = Math.max(1, Math.floor(expectedIterations / maxStoredStates));

        if (this.currentAlgorithm === 'BruteForce') {
            console.log(`[Solver] Matrix ${this.currentMatrix}: Expected ${expectedIterations} iterations, sampling every ${samplingInterval} states`);
        } else {
            console.log(`[Solver] Matrix ${this.currentMatrix} using ${this.currentAlgorithm}: Sampling every ${samplingInterval} states (BruteForce ref: ${expectedIterations} iterations)`);
        }

        // Show grid and initialize renderer for live updates
        this.gridRenderer.setInitialPuzzle(puzzle);

        // Live rendering variables
        let lastRenderTime = Date.now();
        // Longer interval at low speeds to allow animations to complete
        const renderInterval = this.gridRenderer.currentAnimationSpeed <= 10 ? 200 : 50;

        // Capture states during solving with adaptive sampling AND live rendering
        let stateCounter = 0;
        this.solver.onStateChange = (state) => {
            stateCounter++;

            // Sample states for history (playback later)
            if (stateCounter === 1 || stateCounter % samplingInterval === 0 || state.isSolved) {
                this.history.push(state);
            }

            // Live render with throttling based on animation speed
            const now = Date.now();
            if (now - lastRenderTime > renderInterval || state.isSolved) {
                // Only skip animation at very high speeds (>10x)
                const skipAnim = this.gridRenderer.currentAnimationSpeed > 10;
                this.gridRenderer.render(state, { skipAnimation: skipAnim });

                // Update live stats (top display)
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

                // Update controls info display (bottom display)
                if (this.controls) {
                    this.controls.updateInfo(state, this.history.getStats());
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

            // Validate iteration count (only for BruteForce algorithm - DLX and CP have different iteration counts)
            if (this.currentAlgorithm === 'BruteForce') {
                if (this.solver.iteration !== expectedIterations) {
                    console.warn(`[Solver] Iteration mismatch: expected ${expectedIterations}, got ${this.solver.iteration}`);
                    this.showStatus(`⚠ Solved in ${this.solver.iteration.toLocaleString()} iterations (expected ${expectedIterations.toLocaleString()})`, 'warning');
                } else {
                    this.showStatus(`✓ Solved in ${this.solver.iteration.toLocaleString()} iterations - Use controls to replay`, 'success');
                }
            } else {
                // For DLX and CP, just show completion without validation
                this.showStatus(`✓ Solved in ${this.solver.iteration.toLocaleString()} iterations using ${this.currentAlgorithm} - Use controls to replay`, 'success');
            }

        } catch (error) {
            console.error('[Solver] Fatal error:', error);
            this.liveStatsContainer.style.display = 'none';
            this.showStatus(`Error: ${error.message}`, 'error');
            this.isSolving = false;

            // Re-enable controls
            if (this.controls) {
                this.controls.setEnabled(true);
            }
            return;
        }

        // Go to start of history
        this.history.goToStart();

        // Recreate animation controller with new history
        if (this.animationController) {
            this.animationController.cleanup();
        }
        this.animationController = createAnimationController(
            this.history,
            this.gridRenderer,
            this.effects
        );

        // Re-wire the animation controller to the controls
        if (this.controls) {
            this.controls.controller = this.animationController;

            // Re-register controller callbacks
            this.animationController.onPlayStateChange = (playing) => {
                this.controls.updatePlayState(playing);
            };

            this.animationController.onStateChange = (state, stats) => {
                this.controls.updateInfo(state, stats);
                this.controls.updateProgress(stats);
            };

            this.animationController.onComplete = () => {
                this.controls.updatePlayState(false);
            };

            // Re-enable controls
            this.controls.setEnabled(true);
        }

        // Check if memory limit was reached
        const stats = this.history.getStats();
        if (stats.memoryLimitReached) {
            this.container.querySelector('#memory-warning').style.display = 'block';
        }

        // Render initial state
        const initialState = this.history.getCurrentState();
        if (initialState) {
            this.gridRenderer.render(initialState, { skipAnimation: true });
            if (this.controls) {
                this.controls.updateInfo(initialState, stats);
            }
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
        // Cleanup solving state
        this.cleanupSolvingState();

        this.liveStatsContainer.style.display = 'none';
        this.container.querySelector('#memory-warning').style.display = 'none';
        this.hideStatus();

        this.isLoaded = false;

        // Show the new puzzle
        this.showInitialPuzzle();

        // Reinitialize controls for the new puzzle
        this.initializeControls();
    }

    // Cleanup solving-related state (but keep controls)
    cleanupSolvingState() {
        if (this.solver) {
            this.solver = null;
        }

        resetSharedHistory();
        this.history = null;
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
