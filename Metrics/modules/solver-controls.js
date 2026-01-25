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

export class SolverControls {
    constructor(containerElement, animationController, onPlayCallback = null) {
        this.container = containerElement;
        this.controller = animationController;
        this.isPlaying = false;
        this.currentSpeed = 1;
        this.onPlayCallback = onPlayCallback; // Custom callback for Play button

        // UI element references
        this.playPauseBtn = null;
        this.playIcon = null;
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
                    <button class="solver-btn" id="solver-reset" title="Reset to Start">
                        <span class="media-icon">⏮</span>
                    </button>
                    <button class="solver-btn primary" id="solver-step-back" title="Step Backward">
                        <span class="media-icon">◀</span>
                    </button>
                    <button class="solver-btn primary" id="solver-play-pause" title="Play">
                        <span class="media-icon play-icon">▶</span>
                    </button>
                    <button class="solver-btn primary" id="solver-step-forward" title="Step Forward">
                        <span class="media-icon">▶</span>
                    </button>
                    <button class="solver-btn" id="solver-skip-end" title="Skip to End">
                        <span class="media-icon">⏭</span>
                    </button>
                </div>

                <div class="solver-speed-container">
                    <span class="solver-speed-label">Speed:</span>
                    <input type="range" class="solver-speed-slider" id="solver-speed"
                           min="1" max="1000" value="1" step="1">
                    <span class="solver-speed-value" id="solver-speed-value">1x</span>
                </div>

                <div class="solver-speed-presets">
                    <span class="solver-speed-preset active" data-speed="1">1x</span>
                    <span class="solver-speed-preset" data-speed="10">10x</span>
                    <span class="solver-speed-preset" data-speed="100">100x</span>
                    <span class="solver-speed-preset" data-speed="1000">Max</span>
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
        this.playIcon = this.playPauseBtn?.querySelector('.play-icon');
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
        // If onPlayCallback is provided and controller is not playing, call it first
        // This allows the solver to start solving when Play is pressed initially
        if (this.onPlayCallback && !this.isPlaying) {
            const callbackResult = this.onPlayCallback();
            // If callback handled the action (e.g., started solving), don't toggle
            // The callback should return true to indicate it handled the action
            if (callbackResult === true) {
                return;
            }
        }
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
     * @param {number} speed - Speed multiplier (1-1000)
     */
    setSpeed(speed) {
        this.currentSpeed = speed;
        this.controller.setSpeed(speed);

        // Update slider
        this.speedSlider.value = speed;
        this.speedValue.textContent = speed === 1000 ? 'Max' : `${speed}x`;

        // Update slider visual progress
        const progress = (speed - 1) / 999 * 100;
        this.speedSlider.style.setProperty('--progress', `${progress}%`);

        // Update preset highlights
        this.container.querySelectorAll('.solver-speed-preset').forEach(preset => {
            const presetSpeed = parseInt(preset.dataset.speed, 10);
            preset.classList.toggle('active', presetSpeed === speed);
        });
    }

    /**
     * Set recommended speed based on algorithm and expected iterations
     * @param {string} algorithm - Algorithm name (BruteForce, CP, or DLX)
     * @param {number} expectedIterations - Expected iteration count
     */
    setRecommendedSpeed(algorithm, expectedIterations) {
        let recommendedSpeed = 1;

        // For brute force, speed depends on iteration count
        if (algorithm === 'BruteForce') {
            if (expectedIterations < 1000) {
                recommendedSpeed = 1; // Slow for small puzzles
            } else if (expectedIterations < 10000) {
                recommendedSpeed = 10;
            } else if (expectedIterations < 100000) {
                recommendedSpeed = 50;
            } else {
                recommendedSpeed = 100;
            }
        } else if (algorithm === 'CP') {
            // CP is faster than brute force, typically 10-100x reduction
            recommendedSpeed = 50;
        } else if (algorithm === 'DLX') {
            // DLX is the fastest
            recommendedSpeed = 100;
        }

        this.setSpeed(recommendedSpeed);
    }

    /**
     * Update play/pause button state
     * @param {boolean} playing - Whether animation is playing
     */
    updatePlayState(playing) {
        this.isPlaying = playing;

        // Toggle icon between play and pause
        if (this.playIcon) {
            this.playIcon.textContent = playing ? '⏸' : '▶';
            this.playPauseBtn.title = playing ? 'Pause' : 'Play';
        }
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

        // Show cell position only if valid (0-8 range)
        if (state.row !== undefined && state.row >= 0 && state.row <= 8 &&
            state.col !== undefined && state.col >= 0 && state.col <= 8) {
            cell.textContent = `(${state.row}, ${state.col})`;
        } else {
            cell.textContent = '-';
        }

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
 * @param {Function} onPlayCallback - Optional callback when Play is clicked (before solving starts)
 * @returns {SolverControls} Initialized controls instance
 */
export function createSolverControls(containerElement, animationController, onPlayCallback = null) {
    const controls = new SolverControls(containerElement, animationController, onPlayCallback);
    controls.init();
    return controls;
}
