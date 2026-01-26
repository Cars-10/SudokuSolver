/**
 * Sudoku Solver State History Management
 *
 * Provides immutable state history with memory limits for step-by-step
 * animation playback. Uses circular buffer pattern to cap memory usage.
 *
 * Migrated from Metrics/modules/solver-state.js to TypeScript
 */

import { componentRegistry } from '../../core/ComponentRegistry';
import type { SolverState, HistoryStats } from '../../types/solver';

export class SolverHistory {
  private states: SolverState[] = [];
  private position: number = -1;
  private maxStates: number;
  private totalPushed: number = 0;

  constructor(maxStates: number = 10000) {
    this.maxStates = maxStates;

    // Register with ComponentRegistry
    componentRegistry.register({
      id: 'SH',
      fullName: 'Solver History',
      type: 'solver',
      path: '/src/components/solver/SolverHistory.ts',
      dependencies: []
    });
  }

  /**
   * Add new state to history (freezes for immutability)
   * Truncates any future states beyond current position
   * Enforces memory limit by removing oldest states
   */
  push(state: SolverState): void {
    // When pushing, truncate any future states beyond current position
    this.states.length = this.position + 1;

    // Create frozen copy for immutability
    // Deep freeze the grid (both outer array and inner row arrays)
    const frozenState: SolverState = Object.freeze({
      grid: Object.freeze(state.grid.map(row => Object.freeze([...row]))) as number[][],
      row: state.row,
      col: state.col,
      value: state.value,
      iteration: state.iteration,
      depth: state.depth,
      isBacktrack: state.isBacktrack,
      isSolved: state.isSolved || false
    });

    this.states.push(frozenState);
    this.position++;
    this.totalPushed++;

    // Enforce memory limit: remove oldest states if exceeded
    if (this.states.length > this.maxStates) {
      const excess = this.states.length - this.maxStates;
      this.states.splice(0, excess);
      this.position -= excess;

      // Safety: ensure position doesn't go negative
      if (this.position < 0) {
        this.position = 0;
      }
    }
  }

  /**
   * Check if backward navigation is possible
   */
  canStepBack(): boolean {
    return this.position > 0;
  }

  /**
   * Check if forward navigation is possible
   */
  canStepForward(): boolean {
    return this.position < this.states.length - 1;
  }

  /**
   * Step backward to previous state
   */
  stepBack(): SolverState | null {
    if (this.canStepBack()) {
      this.position--;
    }
    return this.getCurrentState();
  }

  /**
   * Step forward to next state
   */
  stepForward(): SolverState | null {
    if (this.canStepForward()) {
      this.position++;
    }
    return this.getCurrentState();
  }

  /**
   * Get state at current position
   */
  getCurrentState(): SolverState | null {
    if (this.position >= 0 && this.position < this.states.length) {
      return this.states[this.position];
    }
    return null;
  }

  /**
   * Jump to specific position in history
   */
  goTo(index: number): SolverState | null {
    if (index >= 0 && index < this.states.length) {
      this.position = index;
    }
    return this.getCurrentState();
  }

  /**
   * Jump to first state in history
   */
  goToStart(): SolverState | null {
    if (this.states.length > 0) {
      this.position = 0;
    }
    return this.getCurrentState();
  }

  /**
   * Jump to last state in history
   */
  goToEnd(): SolverState | null {
    if (this.states.length > 0) {
      this.position = this.states.length - 1;
    }
    return this.getCurrentState();
  }

  /**
   * Clear all history and reset to initial state
   */
  clear(): void {
    this.states = [];
    this.position = -1;
    this.totalPushed = 0;
  }

  /**
   * Get statistics for UI display
   */
  getStats(): HistoryStats {
    return {
      totalStates: this.states.length,
      currentPosition: this.position,
      totalPushed: this.totalPushed,
      memoryLimitReached: this.totalPushed > this.maxStates,
      percentage: this.states.length > 0
        ? Math.round((this.position / (this.states.length - 1)) * 100)
        : 0
    };
  }

  /**
   * Get all states (for debugging or export)
   * Note: Returns reference to internal array - do not mutate!
   */
  getAllStates(): readonly SolverState[] {
    return this.states;
  }

  /**
   * Get total number of states in history
   */
  getLength(): number {
    return this.states.length;
  }
}

// ============================================================================
// Shared Instance Management
// ============================================================================

let sharedInstance: SolverHistory | null = null;

/**
 * Get or create shared history instance
 * Useful for components that need to share the same history
 */
export function getSharedHistory(maxStates: number = 10000): SolverHistory {
  if (!sharedInstance) {
    sharedInstance = new SolverHistory(maxStates);
  }
  return sharedInstance;
}

/**
 * Reset shared history instance
 * Call this when starting a new solve to clear previous history
 */
export function resetSharedHistory(): void {
  if (sharedInstance) {
    sharedInstance.clear();
  }
  sharedInstance = null;
}
