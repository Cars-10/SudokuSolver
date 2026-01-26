// Migrated from Metrics/modules/ui-persistence.js
import { appState } from '../core/StateManager';
import type { AppState } from '../types/state';

export class PersistenceService {
  private readonly STORAGE_KEY = 'sudokuBenchmark_state_v2';
  private saveTimeout: number | null = null;

  save(): void {
    const state = appState.get();
    try {
      localStorage.setItem(this.STORAGE_KEY, JSON.stringify(state));
      if (import.meta.env.DEV) {
        console.debug('[PersistenceService] State saved');
      }
    } catch (err) {
      console.error('[PersistenceService] Save failed:', err);
    }
  }

  restore(): void {
    try {
      const saved = localStorage.getItem(this.STORAGE_KEY);
      if (saved) {
        const restoredState = JSON.parse(saved) as AppState;
        appState.set(restoredState);
        if (import.meta.env.DEV) {
          console.debug('[PersistenceService] State restored');
        }
      }
    } catch (err) {
      console.warn('[PersistenceService] Failed to restore state, clearing:', err);
      // Clear corrupted state
      localStorage.removeItem(this.STORAGE_KEY);
    }
  }

  // Debounced save (2 seconds)
  scheduleSave(): void {
    if (this.saveTimeout) clearTimeout(this.saveTimeout);
    this.saveTimeout = window.setTimeout(() => this.save(), 2000);
  }

  clear(): void {
    localStorage.removeItem(this.STORAGE_KEY);
    console.log('[PersistenceService] State cleared');
  }

  init(): void {
    this.restore();

    // Auto-save on state change
    appState.subscribe(() => this.scheduleSave());

    // Save before unload
    window.addEventListener('beforeunload', () => this.save());
  }
}

export const persistenceService = new PersistenceService();
