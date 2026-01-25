// Solver type definitions

/**
 * Represents the state of the solver at a specific iteration
 */
export interface SolverState {
  grid: number[][];
  row: number;
  col: number;
  value: number;
  iteration: number;
  depth: number;
  isBacktrack: boolean;
  isSolved: boolean;
}

/**
 * Callback function for state changes
 */
export type StateChangeCallback = (state: SolverState) => void;

/**
 * Statistics about solver history
 */
export interface HistoryStats {
  totalStates: number;
  currentPosition: number;
  totalPushed: number;
  memoryLimitReached: boolean;
  percentage: number;
}

/**
 * Options for rendering solver grid
 */
export interface RenderOptions {
  skipAnimation?: boolean;
}

/**
 * Playback state change callback
 */
export type PlayStateChangeCallback = (playing: boolean) => void;

/**
 * Completion callback
 */
export type CompletionCallback = () => void;

/**
 * Info update callback
 */
export type InfoUpdateCallback = (state: SolverState, stats: HistoryStats) => void;
