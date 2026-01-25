/**
 * Animation Controller for Interactive Solver
 *
 * Manages playback of solver state history using requestAnimationFrame
 * with delta-time based speed control (1x - 100x).
 *
 * Migrated from Metrics/modules/solver-animation.js to TypeScript
 */

import { componentRegistry } from '../../core/ComponentRegistry';
import type {
  SolverState,
  InfoUpdateCallback,
  PlayStateChangeCallback,
  CompletionCallback
} from '../../types/solver';
import type { SolverHistory } from './SolverHistory';
import type { SolverGridRenderer } from './SolverGrid';
import type { SolverEffects } from './SolverEffects';

export class AnimationController {
  private history: SolverHistory;
  private gridRenderer: SolverGridRenderer;
  private effects: SolverEffects | null;
  private playing: boolean = false;
  private speed: number = 1.0;
  private lastTimestamp: number | null = null;
  private stepInterval: number = 100; // ms between steps at 1x speed
  private accumulated: number = 0;
  private requestId: number | null = null;

  // Callbacks
  public onStateChange: InfoUpdateCallback | null = null;
  public onPlayStateChange: PlayStateChangeCallback | null = null;
  public onComplete: CompletionCallback | null = null;

  constructor(
    history: SolverHistory,
    gridRenderer: SolverGridRenderer,
    effects: SolverEffects | null = null
  ) {
    this.history = history;
    this.gridRenderer = gridRenderer;
    this.effects = effects;

    // Register with ComponentRegistry
    componentRegistry.register({
      id: 'SAC',
      fullName: 'Solver Animation Controller',
      type: 'solver',
      path: '/src/components/solver/SolverAnimation.ts',
      dependencies: ['SolverHistory', 'SolverGridRenderer']
    });
  }

  /**
   * Start animation playback
   */
  play(): void {
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
  pause(): void {
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
  toggle(): void {
    if (this.playing) {
      this.pause();
    } else {
      this.play();
    }
  }

  /**
   * Animation loop tick using delta-time calculation
   */
  private tick(timestamp: number): void {
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

        if (state) {
          // Determine if we should skip animations at high speeds
          const skipAnimation = this.speed > 10;
          this.gridRenderer.render(state, { skipAnimation });

          // Trigger effects based on state
          if (this.effects && !skipAnimation && prevState) {
            this.effects.onStateChange(state, prevState);
          }

          // Notify listeners
          if (this.onStateChange) {
            this.onStateChange(state, this.history.getStats());
          }
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
   */
  setSpeed(multiplier: number): void {
    this.speed = Math.max(1, Math.min(100, multiplier));

    // Update grid renderer animation speed
    this.gridRenderer.setAnimationSpeed(this.speed);

    // Disable effects at very high speeds
    if (this.effects) {
      this.effects.setEnabled(this.speed <= 20);
    }
  }

  /**
   * Get current speed
   */
  getSpeed(): number {
    return this.speed;
  }

  /**
   * Check if playing
   */
  isPlaying(): boolean {
    return this.playing;
  }

  /**
   * Step backward one state
   */
  stepBack(): void {
    if (this.playing) {
      this.pause();
    }

    const state = this.history.stepBack();
    if (state) {
      this.gridRenderer.render(state, { skipAnimation: true });

      if (this.onStateChange) {
        this.onStateChange(state, this.history.getStats());
      }
    }
  }

  /**
   * Step forward one state
   */
  stepForward(): void {
    if (this.playing) {
      this.pause();
    }

    const state = this.history.stepForward();
    if (state) {
      this.gridRenderer.render(state, { skipAnimation: true });

      if (this.onStateChange) {
        this.onStateChange(state, this.history.getStats());
      }
    }
  }

  /**
   * Reset to start
   */
  reset(): void {
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
   * Skip to end
   */
  skipToEnd(): void {
    this.pause();
    const state = this.history.goToEnd();
    if (state) {
      this.gridRenderer.render(state, { skipAnimation: true });

      if (this.onStateChange) {
        this.onStateChange(state, this.history.getStats());
      }
    }
  }

  /**
   * Cleanup
   */
  cleanup(): void {
    this.pause();
    this.onStateChange = null;
    this.onPlayStateChange = null;
    this.onComplete = null;
  }
}
