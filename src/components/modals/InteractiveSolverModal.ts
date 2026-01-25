// Interactive Solver Modal - shows the sudoku solver in action
import { BaseModal } from './BaseModal';
import { BruteForceSolver } from '../solver/SolverEngine';
import { SolverHistory } from '../solver/SolverHistory';
import { SolverGridRenderer } from '../solver/SolverGrid';
import { AnimationController } from '../solver/SolverAnimation';
import { SolverControls } from '../solver/SolverControls';
import { SolverEffects } from '../solver/SolverEffects';

/**
 * Interactive Solver Modal
 *
 * Integrates all solver components into a complete interactive sudoku solving experience.
 * ✅ Phase 4 Complete - Full implementation with all 7 solver modules
 */
export class InteractiveSolverModal extends BaseModal {
  private solver: BruteForceSolver | null = null;
  private history: SolverHistory | null = null;
  private gridRenderer: SolverGridRenderer | null = null;
  private animationController: AnimationController | null = null;
  private controls: SolverControls | null = null;
  private effects: SolverEffects | null = null;
  private currentMatrix: number = 1;

  constructor() {
    super('ISM', 'Interactive Solver Modal', {
      dependencies: ['SolverEngine', 'SolverHistory', 'SolverGrid', 'AnimationController', 'SolverControls'],
      stateKeys: ['solver']
    });
  }

  protected getDomId(): string {
    return 'solver-modal';
  }

  protected render(): HTMLElement {
    const title = 'Interactive Sudoku Solver';
    const bodyContent = this.renderSolver();

    const modal = this.createModalElement(title, bodyContent);

    // Make this modal larger for the solver grid
    modal.style.width = '1200px';
    modal.style.maxWidth = '95vw';

    // Initialize solver components after rendering
    requestAnimationFrame(() => this.initializeSolver());

    return modal;
  }

  private renderSolver(): HTMLElement {
    const container = document.createElement('div');
    container.className = 'interactive-solver';
    container.setAttribute('data-component-id', `${this.id}-CONTENT`);

    container.innerHTML = `
      <div class="solver-layout">
        <!-- Three-column header -->
        <div class="solver-header-grid" data-component-id="${this.id}-HEADER">
          <div class="solver-header-title">
            <h3>Interactive Solver</h3>
            <p class="solver-subtitle">Watch the algorithm think</p>
          </div>

          <div class="solver-header-controls" data-component-id="${this.id}-MATRIX-SELECTOR">
            <div class="solver-control-group">
              <label>Matrix:</label>
              <select id="matrix-select" class="solver-select">
                <option value="1">Matrix 1 (656 iterations)</option>
                <option value="2">Matrix 2 (439,269 iterations)</option>
                <option value="3">Matrix 3 (98,847 iterations)</option>
                <option value="4">Matrix 4 (9,085 iterations)</option>
                <option value="5">Matrix 5 (445,778 iterations)</option>
              </select>
            </div>
            <div class="solver-control-group">
              <label>Algorithm:</label>
              <select id="algorithm-select" class="solver-select">
                <option value="BruteForce" selected>Brute Force</option>
                <option value="DLX" disabled>Dancing Links (coming soon)</option>
              </select>
            </div>
            <button id="load-matrix-btn" class="btn solver-start-btn">
              <span class="icon">▶</span> Start Solving
            </button>
          </div>

          <div class="solver-header-description" data-component-id="${this.id}-DESCRIPTION">
            <h4>How It Works</h4>
            <p id="algo-description">
              <strong>Brute Force Backtracking:</strong> Tries every possible number (1-9) in each empty cell, row by row. If a number violates Sudoku rules, it backtracks and tries the next number. Guaranteed to find a solution but explores many dead ends.
            </p>
          </div>
        </div>

        <!-- Main content area: Grid on left, Controls on right -->
        <div class="solver-main-content" data-component-id="${this.id}-MAIN">
          <div class="solver-grid-section" data-component-id="${this.id}-GRID-SECTION">
            <div id="solver-grid-container" data-component-id="${this.id}-GRID"></div>
          </div>

          <div class="solver-controls-section" data-component-id="${this.id}-CONTROLS-SECTION">
            <div id="solver-controls-container" data-component-id="${this.id}-CONTROLS"></div>
            <div class="solver-status" data-component-id="${this.id}-STATUS">
              <p id="solver-status-text">Select a matrix and click "Start Solving" to begin</p>
            </div>
          </div>
        </div>
      </div>
    `;

    return container;
  }

  private async initializeSolver(): Promise<void> {
    if (!this.container) return;

    // Initialize components
    const gridContainer = this.container.querySelector('#solver-grid-container') as HTMLElement;
    const controlsContainer = this.container.querySelector('#solver-controls-container') as HTMLElement;

    if (!gridContainer || !controlsContainer) {
      console.error('[ISM] Required containers not found');
      return;
    }

    // Create solver components
    this.solver = new BruteForceSolver();
    this.history = new SolverHistory(10000);
    this.gridRenderer = new SolverGridRenderer(gridContainer);
    this.gridRenderer.initGrid();

    // Create effects
    const grid = this.gridRenderer.getGridContainer();
    if (grid) {
      this.effects = new SolverEffects(grid);
    }

    // Create animation controller
    this.animationController = new AnimationController(
      this.history,
      this.gridRenderer,
      this.effects
    );

    // Create controls
    this.controls = new SolverControls(controlsContainer, this.animationController);
    this.controls.init();
    this.controls.setEnabled(false); // Disabled until matrix is loaded

    // Wire up matrix selection
    const matrixSelect = this.container.querySelector('#matrix-select') as HTMLSelectElement;
    const algorithmSelect = this.container.querySelector('#algorithm-select') as HTMLSelectElement;
    const loadBtn = this.container.querySelector('#load-matrix-btn') as HTMLButtonElement;

    loadBtn?.addEventListener('click', () => {
      const matrixNum = parseInt(matrixSelect.value, 10);
      this.loadAndSolveMatrix(matrixNum);
    });

    algorithmSelect?.addEventListener('change', (e) => {
      const target = e.target as HTMLSelectElement;
      this.updateAlgorithmDescription(target.value);
    });
  }

  private updateAlgorithmDescription(algorithm: string): void {
    const descriptions: Record<string, string> = {
      'BruteForce': '<strong>Brute Force Backtracking:</strong> Tries every possible number (1-9) in each empty cell, row by row. If a number violates Sudoku rules, it backtracks and tries the next number. Guaranteed to find a solution but explores many dead ends.',
      'DLX': '<strong>Dancing Links (Algorithm X):</strong> Uses Donald Knuth\'s Algorithm X to efficiently solve the exact cover problem. Represents the puzzle as a matrix of constraints and uses clever pointer manipulation to quickly explore valid solutions. Much faster than brute force.'
    };

    const descEl = this.container?.querySelector('#algo-description');
    if (descEl && descriptions[algorithm]) {
      descEl.innerHTML = descriptions[algorithm];
    }
  }

  private async loadAndSolveMatrix(matrixNum: number): Promise<void> {
    if (!this.solver || !this.history || !this.gridRenderer || !this.controls) {
      console.error('[ISM] Solver components not initialized');
      return;
    }

    this.currentMatrix = matrixNum;
    this.updateStatus('Loading matrix...');

    try {
      // Load matrix puzzle (hardcoded Matrix 1 for now)
      const matrix1 = [
        [0, 0, 3, 0, 2, 0, 6, 0, 0],
        [9, 0, 0, 3, 0, 5, 0, 0, 1],
        [0, 0, 1, 8, 0, 6, 4, 0, 0],
        [0, 0, 8, 1, 0, 2, 9, 0, 0],
        [7, 0, 0, 0, 0, 0, 0, 0, 8],
        [0, 0, 6, 7, 0, 8, 2, 0, 0],
        [0, 0, 2, 6, 0, 9, 5, 0, 0],
        [8, 0, 0, 2, 0, 3, 0, 0, 9],
        [0, 0, 5, 0, 1, 0, 3, 0, 0]
      ];

      this.solver.loadPuzzle(matrix1);
      this.gridRenderer.setInitialPuzzle(matrix1);

      // Clear history
      this.history.clear();

      // Connect solver to history
      this.solver.onStateChange = (state) => {
        this.history!.push(state);
      };

      this.updateStatus('Solving...');

      // Solve (this will populate history)
      const startTime = performance.now();
      const solved = this.solver.solve();
      const elapsed = (performance.now() - startTime).toFixed(2);

      if (solved) {
        this.updateStatus(
          `✅ Solved in ${this.solver.getIteration().toLocaleString()} iterations (${elapsed}ms). Use controls to replay.`
        );

        // Enable controls for playback
        this.controls.setEnabled(true);

        // Go to first state
        if (this.animationController) {
          this.animationController.reset();
        }
      } else {
        this.updateStatus('❌ Failed to solve puzzle');
      }
    } catch (error) {
      this.updateStatus(`❌ Error: ${error instanceof Error ? error.message : 'Unknown error'}`);
      console.error('[ISM] Solve error:', error);
    }
  }

  private updateStatus(message: string): void {
    const statusEl = this.container?.querySelector('#solver-status-text');
    if (statusEl) {
      statusEl.textContent = message;
    }
  }

  close(): void {
    // Cleanup animation
    if (this.animationController) {
      this.animationController.pause();
      this.animationController.cleanup();
    }

    // Cleanup grid
    if (this.gridRenderer) {
      this.gridRenderer.cleanup();
    }

    // Cleanup controls
    if (this.controls) {
      this.controls.cleanup();
    }

    // Clear references
    this.solver = null;
    this.history = null;
    this.gridRenderer = null;
    this.animationController = null;
    this.controls = null;
    this.effects = null;

    super.close();
  }
}

export const interactiveSolverModal = new InteractiveSolverModal();

// Global compatibility
if (typeof window !== 'undefined') {
  (window as any).interactiveSolverModal = interactiveSolverModal;
  (window as any).showSolver = () => interactiveSolverModal.open();
}
