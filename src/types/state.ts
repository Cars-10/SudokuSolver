// Application state interfaces

export interface AppState {
  ui: UIState;
  solver: SolverState;
  charts: ChartState;
  filters: FilterState;
}

export interface UIState {
  currentPersona: string;
  currentSort: {
    column: string;
    direction: 'asc' | 'desc';
  };
  showLogos: boolean;
  showMismatches: boolean;
  dockerMode: boolean;
}

export interface SolverState {
  isPlaying: boolean;
  speed: number;
  currentMatrix: number;
  iteration: number;
  grid: number[][];
  solution: number[][] | null;
}

export interface ChartState {
  mode: 'scatter' | 'line' | 'bar' | 'heatmap' | 'treemap';
  fullscreen: boolean;
  logScale: boolean;
  xAxis: string;
  yAxis: string;
}

export interface FilterState {
  searchTerm: string;
  algorithm: string;
  language: string;
}
