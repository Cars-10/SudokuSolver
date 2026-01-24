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

export class AnimationController {
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
export function createAnimationController(history, gridRenderer, effects = null) {
    return new AnimationController(history, gridRenderer, effects);
}
