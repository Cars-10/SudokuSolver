/**
 * Playback Controls UI for Interactive Solver
 *
 * Provides user interface for controlling solver animation playback.
 * Migrated from Metrics/modules/solver-controls.js to TypeScript
 */

import { componentRegistry } from '../../core/ComponentRegistry';
import type { SolverState, HistoryStats } from '../../types/solver';
import type { AnimationController } from './SolverAnimation';

export class SolverControls {
  private container: HTMLElement;
  private controller: AnimationController;
  private isPlaying: boolean = false;
  private currentSpeed: number = 1;

  // UI element references
  private playPauseBtn: HTMLElement | null = null;
  private stepBackBtn: HTMLElement | null = null;
  private stepForwardBtn: HTMLElement | null = null;
  private resetBtn: HTMLElement | null = null;
  private skipToEndBtn: HTMLElement | null = null;
  private speedSlider: HTMLInputElement | null = null;
  private speedValue: HTMLElement | null = null;
  private progressBar: HTMLElement | null = null;
  private infoDisplay: HTMLElement | null = null;

  constructor(containerElement: HTMLElement, animationController: AnimationController) {
    this.container = containerElement;
    this.controller = animationController;

    // Register with ComponentRegistry
    componentRegistry.register({
      id: 'SC',
      fullName: 'Solver Controls',
      type: 'solver',
      path: '/src/components/solver/SolverControls.ts',
      dependencies: ['AnimationController']
    });
  }

  /**
   * Initialize controls UI
   */
  init(): void {
    this.container.innerHTML = `
      <div class="solver-controls" data-component-id="SC-CONTROLS">
        <div class="solver-controls-row" data-component-id="SC-BUTTONS">
          <button class="solver-btn" id="solver-reset" title="Reset" data-component-id="SC-RESET">
            <span class="icon">⏮</span> Reset
          </button>
          <button class="solver-btn" id="solver-step-back" title="Step Back" data-component-id="SC-STEP-BACK">
            <span class="icon">⏪</span> Back
          </button>
          <button class="solver-btn primary" id="solver-play-pause" title="Play/Pause" data-component-id="SC-PLAY-PAUSE">
            <span class="icon" id="play-icon">▶</span> Play
          </button>
          <button class="solver-btn" id="solver-step-forward" title="Step Forward" data-component-id="SC-STEP-FORWARD">
            <span class="icon">⏩</span> Fwd
          </button>
          <button class="solver-btn" id="solver-skip-end" title="Skip to End" data-component-id="SC-SKIP-END">
            <span class="icon">⏭</span> End
          </button>
        </div>

        <div class="solver-speed-container" data-component-id="SC-SPEED">
          <span class="solver-speed-label">Speed:</span>
          <input type="range" class="solver-speed-slider" id="solver-speed"
                 min="1" max="100" value="1" step="1">
          <span class="solver-speed-value" id="solver-speed-value">1x</span>
        </div>

        <div class="solver-speed-presets" data-component-id="SC-PRESETS">
          <span class="solver-speed-preset active" data-speed="1">1x</span>
          <span class="solver-speed-preset" data-speed="10">10x</span>
          <span class="solver-speed-preset" data-speed="50">50x</span>
          <span class="solver-speed-preset" data-speed="100">Max</span>
        </div>

        <div class="solver-progress" data-component-id="SC-PROGRESS">
          <div class="solver-progress-bar" id="solver-progress"></div>
        </div>

        <div class="solver-info" id="solver-info" data-component-id="SC-INFO">
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
    this.playPauseBtn?.addEventListener('click', () => this.controller.toggle());
    this.stepBackBtn?.addEventListener('click', () => this.controller.stepBack());
    this.stepForwardBtn?.addEventListener('click', () => this.controller.stepForward());
    this.resetBtn?.addEventListener('click', () => this.controller.reset());
    this.skipToEndBtn?.addEventListener('click', () => this.controller.skipToEnd());
    this.speedSlider?.addEventListener('input', (e) => {
      const speed = parseInt((e.target as HTMLInputElement).value, 10);
      this.setSpeed(speed);
    });

    // Speed presets
    this.container.querySelectorAll('.solver-speed-preset').forEach(preset => {
      preset.addEventListener('click', () => {
        const speed = parseInt((preset as HTMLElement).dataset.speed || '1', 10);
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

  /**
   * Set speed and update UI
   */
  setSpeed(speed: number): void {
    this.currentSpeed = speed;
    this.controller.setSpeed(speed);

    // Update slider
    if (this.speedSlider) {
      this.speedSlider.value = String(speed);
    }
    if (this.speedValue) {
      this.speedValue.textContent = speed === 100 ? 'Max' : `${speed}x`;
    }

    // Update slider visual progress
    const progress = (speed - 1) / 99 * 100;
    this.speedSlider?.style.setProperty('--progress', `${progress}%`);

    // Update preset highlights
    this.container.querySelectorAll('.solver-speed-preset').forEach(preset => {
      const presetSpeed = parseInt((preset as HTMLElement).dataset.speed || '1', 10);
      preset.classList.toggle('active', presetSpeed === speed);
    });
  }

  /**
   * Update play/pause button state
   */
  updatePlayState(playing: boolean): void {
    this.isPlaying = playing;
    const icon = this.playPauseBtn?.querySelector('.icon');
    if (icon) {
      icon.textContent = playing ? '⏸' : '▶';
    }
    if (this.playPauseBtn) {
      this.playPauseBtn.title = playing ? 'Pause' : 'Play';
      // Update button text as well
      const textNode = Array.from(this.playPauseBtn.childNodes).find(
        node => node.nodeType === Node.TEXT_NODE
      );
      if (textNode) {
        textNode.textContent = playing ? ' Pause' : ' Play';
      }
    }
  }

  /**
   * Update info display
   */
  updateInfo(state: SolverState, stats: HistoryStats): void {
    if (!state) return;

    const iteration = this.container.querySelector('#info-iteration');
    const cell = this.container.querySelector('#info-cell');
    const value = this.container.querySelector('#info-value');
    const depth = this.container.querySelector('#info-depth');

    if (iteration) iteration.textContent = state.iteration.toLocaleString();
    if (cell) cell.textContent = state.row !== undefined ? `(${state.row}, ${state.col})` : '-';
    if (value) value.textContent = String(state.value || '-');
    if (depth) {
      depth.textContent = String(state.depth || '0');

      // Color depth based on backtrack intensity
      if (state.depth > 5) {
        (depth as HTMLElement).style.color = '#ff0064';
      } else if (state.depth > 2) {
        (depth as HTMLElement).style.color = '#ff9d00';
      } else {
        (depth as HTMLElement).style.color = '#00ff9d';
      }
    }
  }

  /**
   * Update progress bar
   */
  updateProgress(stats: HistoryStats): void {
    if (!stats || stats.totalStates === 0) return;

    const progress = (stats.currentPosition / (stats.totalStates - 1)) * 100;
    if (this.progressBar) {
      this.progressBar.style.width = `${progress}%`;
    }
  }

  /**
   * Enable/disable controls
   */
  setEnabled(enabled: boolean): void {
    const buttons = [
      this.playPauseBtn,
      this.stepBackBtn,
      this.stepForwardBtn,
      this.resetBtn,
      this.skipToEndBtn
    ];

    buttons.forEach(btn => {
      if (btn) (btn as HTMLButtonElement).disabled = !enabled;
    });

    if (this.speedSlider) {
      this.speedSlider.disabled = !enabled;
    }
  }

  /**
   * Cleanup
   */
  cleanup(): void {
    this.container.innerHTML = '';
  }
}
