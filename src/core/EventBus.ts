// Pub/sub event system for decoupled component communication

type EventHandler = (...args: any[]) => void;

export class EventBus {
  private events: Map<string, Set<EventHandler>> = new Map();

  on(event: string, handler: EventHandler): () => void {
    if (!this.events.has(event)) {
      this.events.set(event, new Set());
    }
    this.events.get(event)!.add(handler);

    // Return unsubscribe function
    return () => this.off(event, handler);
  }

  off(event: string, handler: EventHandler): void {
    this.events.get(event)?.delete(handler);
  }

  emit(event: string, ...args: any[]): void {
    const handlers = this.events.get(event);
    if (handlers) {
      handlers.forEach(handler => {
        try {
          handler(...args);
        } catch (err) {
          console.error(`[EventBus] Error in handler for "${event}":`, err);
        }
      });
    }

    // Log event in dev mode
    if (import.meta.env.DEV) {
      console.debug(`[EventBus] ${event}`, ...args);
    }
  }

  once(event: string, handler: EventHandler): void {
    const wrapper = (...args: any[]) => {
      handler(...args);
      this.off(event, wrapper);
    };
    this.on(event, wrapper);
  }

  clear(): void {
    this.events.clear();
  }

  // Get active listeners count
  listenerCount(event?: string): number {
    if (event) {
      return this.events.get(event)?.size || 0;
    }
    let total = 0;
    this.events.forEach(handlers => {
      total += handlers.size;
    });
    return total;
  }
}

export const eventBus = new EventBus();

// Event names as constants (prevents typos)
export const Events = {
  // Persona events
  PERSONA_CHANGED: 'persona:changed',

  // Modal events
  MODAL_OPENED: 'modal:opened',
  MODAL_CLOSED: 'modal:closed',

  // Chart events
  CHART_RENDERED: 'chart:rendered',
  CHART_MODE_CHANGED: 'chart:mode-changed',

  // Solver events
  SOLVER_STEP: 'solver:step',
  SOLVER_STARTED: 'solver:started',
  SOLVER_PAUSED: 'solver:paused',
  SOLVER_RESET: 'solver:reset',
  SOLVER_COMPLETED: 'solver:completed',

  // State events
  STATE_UPDATED: 'state:updated',

  // Filter events
  FILTER_CHANGED: 'filter:changed',

  // UI events
  THEME_CHANGED: 'theme:changed',
  SCREENSAVER_STARTED: 'screensaver:started',
  SCREENSAVER_STOPPED: 'screensaver:stopped'
} as const;

// Debug helper
if (import.meta.env.DEV) {
  (window as any).eventBus = eventBus;
}
