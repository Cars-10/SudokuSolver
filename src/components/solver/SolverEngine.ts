/**
 * Sudoku Solver - TypeScript Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * ⚠️ CRITICAL: DO NOT MODIFY ALGORITHM LOGIC!
 * The iteration count serves as the algorithm's fingerprint.
 * Expected iterations for Matrix 1: 656
 *
 * Algorithm Requirements (matches C reference):
 * - Row-major search order: top-to-bottom, left-to-right
 * - Candidate order: 1-9 ascending
 * - Iteration counting: Increment BEFORE validity check
 *
 * Adapted from Metrics/modules/solver-engine.js for TypeScript + ComponentRegistry
 */

import { componentRegistry } from '../../core/ComponentRegistry';
import type { SolverState, StateChangeCallback } from '../../types/solver';

export class BruteForceSolver {
  private puzzle: number[][] = [];
  private iteration: number = 0;
  private depth: number = 0;
  private solved: boolean = false;
  public onStateChange: StateChangeCallback | null = null;

  constructor() {
    // Register with ComponentRegistry
    componentRegistry.register({
      id: 'SE',
      fullName: 'Solver Engine',
      type: 'solver',
      path: '/src/components/solver/SolverEngine.ts',
      dependencies: []
    });

    this.reset();
  }

  /**
   * Load puzzle from 2D array or matrix string
   */
  loadPuzzle(input: number[][] | string): void {
    if (typeof input === 'string') {
      this.puzzle = BruteForceSolver.parseMatrixString(input);
    } else if (Array.isArray(input)) {
      // Deep copy to prevent external mutation
      this.puzzle = input.map(row => [...row]);
    } else {
      throw new Error('Invalid input: expected 2D array or matrix string');
    }

    // Validate puzzle dimensions
    if (this.puzzle.length !== 9 || !this.puzzle.every(row => row.length === 9)) {
      throw new Error('Invalid puzzle: must be 9x9 grid');
    }

    this.iteration = 0;
    this.solved = false;
    this.depth = 0;
  }

  /**
   * Parse matrix file format string into 9x9 grid
   * Format: 9 lines of 9 space-separated digits (0 = empty)
   */
  static parseMatrixString(matrixString: string): number[][] {
    const lines = matrixString.split('\n')
      .map(line => line.trim())
      .filter(line => line && !line.startsWith('#')); // Skip empty/comment lines

    if (lines.length < 9) {
      throw new Error(`Invalid matrix: expected 9 lines, got ${lines.length}`);
    }

    const grid: number[][] = [];
    for (let i = 0; i < 9; i++) {
      const values = lines[i].split(/\s+/).map(Number);
      if (values.length !== 9) {
        throw new Error(`Invalid matrix line ${i + 1}: expected 9 values, got ${values.length}`);
      }
      grid.push(values);
    }

    return grid;
  }

  /**
   * ⚠️ ALGORITHM CORE - DO NOT MODIFY
   * Check if value is valid at position (row, col)
   * Validates against row, column, and 3x3 box constraints
   */
  private isValid(row: number, col: number, val: number): boolean {
    // Check row
    for (let i = 0; i < 9; i++) {
      if (this.puzzle[row][i] === val) return false;
    }

    // Check column
    for (let i = 0; i < 9; i++) {
      if (this.puzzle[i][col] === val) return false;
    }

    // Check 3x3 box
    const boxRow = Math.floor(row / 3) * 3;
    const boxCol = Math.floor(col / 3) * 3;
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 3; j++) {
        if (this.puzzle[boxRow + i][boxCol + j] === val) return false;
      }
    }

    return true;
  }

  /**
   * Get deep copy of current puzzle grid
   * Used for immutable state history
   */
  getGridSnapshot(): number[][] {
    return this.puzzle.map(row => [...row]);
  }

  /**
   * ⚠️ ALGORITHM CORE - DO NOT MODIFY
   * Main solving algorithm with state emission
   * Returns true if solved, false if unsolvable
   */
  solve(): boolean {
    // Find first empty cell (row-major order)
    let row = -1, col = -1;
    outer: for (let r = 0; r < 9; r++) {
      for (let c = 0; c < 9; c++) {
        if (this.puzzle[r][c] === 0) {
          row = r;
          col = c;
          break outer;
        }
      }
    }

    // If no empty cell found, puzzle is solved
    if (row === -1) {
      this.solved = true;

      // Emit final solved state
      if (this.onStateChange) {
        this.onStateChange({
          grid: this.getGridSnapshot(),
          row: -1,
          col: -1,
          value: 0,
          iteration: this.iteration,
          depth: this.depth,
          isBacktrack: false,
          isSolved: true
        });
      }

      return true;
    }

    // Try values 1-9 in ascending order
    for (let val = 1; val <= 9; val++) {
      // ⚠️ CRITICAL: COUNT EVERY ATTEMPT - this is the algorithm fingerprint
      this.iteration++;

      // Emit state BEFORE validity check (matches C reference timing)
      if (this.onStateChange) {
        this.onStateChange({
          grid: this.getGridSnapshot(),
          row: row,
          col: col,
          value: val,
          iteration: this.iteration,
          depth: this.depth,
          isBacktrack: false,
          isSolved: false
        });
      }

      if (this.isValid(row, col, val)) {
        // Place value and recurse
        this.puzzle[row][col] = val;
        this.depth++;

        if (this.solve()) {
          return true;
        }

        // Backtrack - restore cell to empty
        this.puzzle[row][col] = 0;
        this.depth--;

        // Emit backtrack state
        if (this.onStateChange) {
          this.onStateChange({
            grid: this.getGridSnapshot(),
            row: row,
            col: col,
            value: val,
            iteration: this.iteration,
            depth: this.depth,
            isBacktrack: true,
            isSolved: false
          });
        }
      }
    }

    return false;
  }

  /**
   * Reset solver to initial state
   */
  reset(): void {
    this.puzzle = Array(9).fill(null).map(() => Array(9).fill(0));
    this.iteration = 0;
    this.depth = 0;
    this.solved = false;
  }

  /**
   * Get current puzzle state as string (for debugging)
   */
  toString(): string {
    return this.puzzle.map(row => row.join(' ')).join('\n');
  }

  /**
   * Get current iteration count
   */
  getIteration(): number {
    return this.iteration;
  }

  /**
   * Check if puzzle is solved
   */
  isSolved(): boolean {
    return this.solved;
  }

  /**
   * Get current depth
   */
  getDepth(): number {
    return this.depth;
  }

  /**
   * Get current grid (reference, not copy)
   */
  getGrid(): number[][] {
    return this.puzzle;
  }
}
