// Type declarations for the legacy screensaver module
declare module '../../Metrics/modules/screensaver.js' {
  export function initMatrixScreensaver(puzzles: string[]): void;
  export function startScreensaver(mode: 'red' | 'blue'): void;
  export function stopScreensaver(): void;
  export class AlienStatusSystem {
    constructor();
    show(): void;
    hide(): void;
  }
  export class RiddleSystem {
    constructor();
    show(): void;
    hide(): void;
  }
}
