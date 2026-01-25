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
                        <span class="icon">⏪</span> Back
                    </button>
                    <button class="solver-btn primary" id="solver-play-pause" title="Play/Pause">
                        <span class="icon" id="play-icon">▶</span> Play
                    </button>
                    <button class="solver-btn" id="solver-step-forward" title="Step Forward">
                        <span class="icon">⏩</span> Fwd
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

        // Update button text as well
        const textNode = Array.from(this.playPauseBtn.childNodes).find(
            node => node.nodeType === Node.TEXT_NODE
        );
        if (textNode) {
            textNode.textContent = playing ? ' Pause' : ' Play';
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
export function createSolverControls(containerElement, animationController) {
    const controls = new SolverControls(containerElement, animationController);
    controls.init();
    return controls;
}
