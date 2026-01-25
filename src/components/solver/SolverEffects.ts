/**
 * Glitch Effects for Interactive Solver
 *
 * Provides visual effects including screen shake, alien character scramble,
 * chromatic aberration, and color inversion.
 *
 * Migrated from Metrics/modules/solver-effects.js to TypeScript
 */

import { componentRegistry } from '../../core/ComponentRegistry';
import type { SolverState } from '../../types/solver';

export class SolverEffects {
  private container: HTMLElement;
  private aliens: string = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
  private activeEffects: Set<string> = new Set();
  private enabled: boolean = true;

  constructor(gridContainer: HTMLElement) {
    this.container = gridContainer;

    // Register with ComponentRegistry
    componentRegistry.register({
      id: 'SEF',
      fullName: 'Solver Effects',
      type: 'solver',
      path: '/src/components/solver/SolverEffects.ts',
      dependencies: []
    });
  }

  /**
   * Enable/disable all effects
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled;
  }

  /**
   * Screen shake effect
   * Trigger on deep backtracks (depth > threshold)
   */
  screenShake(duration: number = 200): void {
    if (!this.enabled || this.activeEffects.has('shake')) return;

    this.activeEffects.add('shake');
    this.container.classList.add('glitch-screen-shake');

    setTimeout(() => {
      this.container.classList.remove('glitch-screen-shake');
      this.activeEffects.delete('shake');
    }, duration);
  }

  /**
   * Alien character scramble on a specific cell
   * Shows random alien characters briefly before settling
   */
  alienScramble(cellElement: HTMLElement, finalValue: string, duration: number = 300): void {
    if (!this.enabled) return;

    const valueSpan = cellElement.querySelector('.cell-value');
    if (!valueSpan) return;

    const startTime = Date.now();
    const originalValue = finalValue || valueSpan.textContent || '';

    const scrambleInterval = setInterval(() => {
      const elapsed = Date.now() - startTime;
      if (elapsed >= duration) {
        clearInterval(scrambleInterval);
        valueSpan.textContent = originalValue;
        valueSpan.setAttribute('data-value', originalValue);
        return;
      }

      // Random alien character
      const randomChar = this.aliens[Math.floor(Math.random() * this.aliens.length)];
      valueSpan.textContent = randomChar;
    }, 50);
  }

  /**
   * Chromatic aberration effect on cell
   */
  chromaticAberration(cellElement: HTMLElement, duration: number = 500): void {
    if (!this.enabled) return;

    cellElement.classList.add('chromatic');

    setTimeout(() => {
      cellElement.classList.remove('chromatic');
    }, duration);
  }

  /**
   * Color inversion flash (entire grid)
   */
  colorInvert(duration: number = 100): void {
    if (!this.enabled) return;

    this.container.style.filter = 'invert(1)';

    setTimeout(() => {
      this.container.style.filter = '';
    }, duration);
  }

  /**
   * Trigger combined glitch based on intensity level
   * intensity: 0-1 (0 = subtle, 1 = maximum chaos)
   */
  triggerGlitch(intensity: number, cellElement: HTMLElement | null = null): void {
    if (!this.enabled) return;

    if (intensity > 0.7) {
      this.screenShake(200 + intensity * 100);
    }

    if (intensity > 0.5 && cellElement) {
      this.chromaticAberration(cellElement, 300 + intensity * 200);
    }

    if (intensity > 0.8) {
      this.colorInvert(50);
    }
  }

  /**
   * Smart trigger based on solver state
   * Call this from animation controller when state changes
   */
  onStateChange(state: SolverState, prevState: SolverState): void {
    if (!this.enabled) return;

    const { isBacktrack, depth, row, col } = state;

    // Screen shake on deep backtrack
    if (isBacktrack && depth > 5) {
      const intensity = Math.min(1, depth / 15);
      this.screenShake(150 + intensity * 150);
    }

    // Alien scramble on backtrack
    if (isBacktrack && row !== undefined && col !== undefined && row >= 0 && col >= 0) {
      const cellIdx = row * 9 + col;
      const cells = this.container.querySelectorAll('.solver-cell');
      if (cells[cellIdx]) {
        // Only scramble occasionally to avoid chaos
        if (Math.random() > 0.7) {
          this.alienScramble(cells[cellIdx] as HTMLElement, '', 150);
        }
      }
    }
  }

  /**
   * Clear all active effects
   */
  clear(): void {
    this.container.classList.remove('glitch-screen-shake');
    this.container.style.filter = '';
    this.container.querySelectorAll('.chromatic').forEach(el => {
      el.classList.remove('chromatic');
    });
    this.activeEffects.clear();
  }
}

/**
 * Factory function
 */
export function createGlitchEffects(gridContainer: HTMLElement): SolverEffects {
  return new SolverEffects(gridContainer);
}
