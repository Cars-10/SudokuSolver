/**
 * Screensaver Component
 * Wraps the Matrix rain screensaver with Red/Blue pill modes
 *
 * Re-exports from Metrics/modules/screensaver.js for Vite integration
 */

import { eventBus, Events } from '../core/EventBus';
import { componentRegistry } from '../core/ComponentRegistry';

// Import the existing screensaver module
// @ts-ignore - JavaScript module
import {
  initMatrixScreensaver,
  startScreensaver,
  stopScreensaver,
  AlienStatusSystem,
  RiddleSystem
} from '../../Metrics/modules/screensaver.js';

class ScreensaverComponent {
  private initialized = false;
  private alienSystem: any;
  private riddleSystem: any;

  constructor() {
    componentRegistry.register({
      id: 'SCREENSAVER',
      fullName: 'Matrix Screensaver',
      type: 'feature',
      path: '/src/components/Screensaver.ts'
    });
  }

  init(puzzles: string[] = []): void {
    if (this.initialized) return;

    // Create canvas if not exists
    let canvas = document.getElementById('matrix-canvas') as HTMLCanvasElement;
    if (!canvas) {
      canvas = document.createElement('canvas');
      canvas.id = 'matrix-canvas';
      canvas.style.display = 'none';
      document.body.appendChild(canvas);
    }

    // Create fullscreen header if not exists
    if (!document.getElementById('fullscreen-header')) {
      const header = document.createElement('div');
      header.id = 'fullscreen-header';
      header.className = 'fullscreen-header';
      header.innerHTML = `
        <div class="fullscreen-header-title">
          MATRIX MODE
        </div>
        <div class="fullscreen-header-hint">
          Press ESC or ALT to exit • SHIFT to change colors
        </div>
      `;
      document.body.appendChild(header);
    }

    initMatrixScreensaver(puzzles);

    // Initialize alien and riddle systems
    this.alienSystem = new AlienStatusSystem();
    this.riddleSystem = new RiddleSystem();

    this.initialized = true;
    console.debug('[Screensaver] Initialized');
  }

  start(mode: 'red' | 'blue' = 'red'): void {
    if (!this.initialized) {
      this.init();
    }

    const canvas = document.getElementById('matrix-canvas');
    if (canvas) {
      canvas.style.display = 'block';
    }

    startScreensaver(mode);

    if (mode === 'red') {
      this.alienSystem?.start();
      this.riddleSystem?.start();
    }

    eventBus.emit(Events.SCREENSAVER_STARTED, mode);
    console.debug('[Screensaver] Started in', mode, 'mode');
  }

  stop(): void {
    stopScreensaver();

    const canvas = document.getElementById('matrix-canvas');
    if (canvas) {
      canvas.style.display = 'none';
    }

    this.riddleSystem?.stop();

    eventBus.emit(Events.SCREENSAVER_STOPPED);
    console.debug('[Screensaver] Stopped');
  }

  isActive(): boolean {
    return document.body.classList.contains('fullscreen-active');
  }
}

export const screensaver = new ScreensaverComponent();

// Expose globally for legacy compatibility
if (typeof window !== 'undefined') {
  (window as any).screensaver = screensaver;
}
