/**
 * Screensaver Component
 * Wraps the Matrix rain screensaver with Red/Blue pill modes
 *
 * Re-exports from Metrics/modules/screensaver.js for Vite integration
 */

import { eventBus, Events } from '../core/EventBus';
import { componentRegistry } from '../core/ComponentRegistry';
import { metricsService } from '../services/MetricsService';
import { chartContainer } from './ChartContainer';

// Import the existing screensaver module (typed in src/types/screensaver.d.ts)
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

      // Get metrics stats
      const metrics = metricsService.getAll();
      const totalCount = metrics.length;
      const mismatchCount = metrics.filter(m => m.hasMismatch).length;
      const failedCount = metrics.filter(m => m.failed).length;
      const timeoutCount = metrics.filter(m =>
        m.results?.some(r => r.status === 'timeout')
      ).length;

      header.innerHTML = `
        <div class="screensaver-info-panel">
          <div id="solver-text" class="screensaver-title-text">${totalCount} WORKING<br>IMPLEMENTATIONS</div>
          <div class="screensaver-mismatch">MISMATCHES: ${mismatchCount}</div>
          <div id="diagnostics-status" class="screensaver-diagnostics">
            <span class="diag-env">ENV: 0</span>
            <span class="diag-timeout">TIMEOUT: ${timeoutCount}</span>
            <span class="diag-error">ERROR: ${failedCount}</span>
            <span class="diag-missing">MISSING: 0</span>
          </div>
          <div id="riddle-container" class="screensaver-riddle"></div>
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

    const canvas = document.getElementById('matrix-canvas') as HTMLCanvasElement;
    if (!canvas) return;

    if (mode === 'red') {
      // Red pill: fullscreen mode
      document.body.classList.add('fullscreen-active');
      document.body.classList.remove('bluepill-active');
      canvas.style.display = 'block';
      canvas.style.position = 'fixed';
      canvas.style.top = '0';
      canvas.style.left = '0';
      canvas.style.width = '100vw';
      canvas.style.height = '100vh';
      canvas.style.zIndex = '9999';
      canvas.style.background = '#000';
    } else {
      // Blue pill: chart area only
      document.body.classList.remove('fullscreen-active');
      document.body.classList.add('bluepill-active');
      const chartEl = document.getElementById('chart-container');
      if (chartEl) {
        // Clear any existing chart content so canvas is visible
        chartEl.innerHTML = '';
        // Move canvas into chart container
        chartEl.appendChild(canvas);
        canvas.style.display = 'block';
        canvas.style.position = 'absolute';
        canvas.style.top = '0';
        canvas.style.left = '0';
        canvas.style.width = '100%';
        canvas.style.height = '100%';
        canvas.style.zIndex = '10';
        canvas.style.background = '#000';

        // Wait for DOM to update before starting animation
        requestAnimationFrame(() => {
          startScreensaver(mode);
        });

        eventBus.emit(Events.SCREENSAVER_STARTED, mode);
        console.debug('[Screensaver] Started in', mode, 'mode');
        return; // Early return since we're using RAF
      }
    }

    startScreensaver(mode);

    if (mode === 'red') {
      // Delay alien/riddle systems slightly to let rain establish
      setTimeout(() => {
        this.alienSystem?.start();
        this.riddleSystem?.start();
      }, 500);
    }

    eventBus.emit(Events.SCREENSAVER_STARTED, mode);
    console.debug('[Screensaver] Started in', mode, 'mode');
  }

  stop(): void {
    const wasInBluePillMode = document.body.classList.contains('bluepill-active');

    stopScreensaver();

    const canvas = document.getElementById('matrix-canvas');
    if (canvas) {
      canvas.style.display = 'none';
      // Move canvas back to body if it was in chart container
      if (canvas.parentElement?.id === 'chart-container') {
        document.body.appendChild(canvas);
      }
    }

    document.body.classList.remove('fullscreen-active');
    document.body.classList.remove('bluepill-active');
    this.alienSystem?.stop();
    this.riddleSystem?.stop();

    // Restore chart if we were in blue pill mode
    if (wasInBluePillMode) {
      chartContainer.resize();
    }

    eventBus.emit(Events.SCREENSAVER_STOPPED);
    console.debug('[Screensaver] Stopped');
  }

  isActive(): boolean {
    return document.body.classList.contains('fullscreen-active') ||
           document.body.classList.contains('bluepill-active');
  }
}

export const screensaver = new ScreensaverComponent();

// Expose globally for legacy compatibility
if (typeof window !== 'undefined') {
  (window as any).screensaver = screensaver;
}
