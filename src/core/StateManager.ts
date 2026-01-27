// Centralized reactive state manager (Zustand-inspired)
import type { AppState } from '../types/state';

type Listener<T> = (state: T) => void;

export class StateManager<T> {
  private state: T;
  private listeners: Set<Listener<T>> = new Set();

  constructor(initialState: T) {
    this.state = initialState;
  }

  get(): T {
    return this.state;
  }

  set(updater: Partial<T> | ((prev: T) => T)): void {
    this.state = typeof updater === 'function'
      ? updater(this.state)
      : { ...this.state, ...updater };

    this.listeners.forEach(listener => listener(this.state));
  }

  subscribe(listener: Listener<T>): () => void {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  // Get a specific slice of state
  getSlice<K extends keyof T>(key: K): T[K] {
    return this.state[key];
  }

  // Update a specific slice of state
  setSlice<K extends keyof T>(key: K, value: Partial<T[K]> | T[K]): void {
    this.state = {
      ...this.state,
      [key]: typeof value === 'object' && value !== null
        ? { ...this.state[key], ...value }
        : value
    };

    this.listeners.forEach(listener => listener(this.state));
  }
}

// Global state instance
export const appState = new StateManager<AppState>({
  ui: {
    currentPersona: 'Standard',
    currentSort: { column: 'score', direction: 'asc' },
    showLogos: true,
    showMismatches: true,
    showFailed: true,
    dockerMode: window.location.port === '9001',
    scrollY: 0
  },
  solver: {
    isPlaying: false,
    speed: 1,
    currentMatrix: 1,
    iteration: 0,
    grid: Array(9).fill(null).map(() => Array(9).fill(0)),
    solution: null
  },
  charts: {
    mode: 'scatter',
    fullscreen: false,
    logScale: false,
    xAxis: 'totalTime',
    yAxis: 'avgMemory'
  },
  filters: {
    searchTerm: '',
    algorithm: 'All',
    language: ''
  }
});

// Debug helper
if (import.meta.env.DEV) {
  (window as any).appState = appState;
}
