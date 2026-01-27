// Chart type definitions

import type { SolverMetrics } from './metrics';

/**
 * Chart mode types
 */
export type ChartMode = 'scatter' | 'bar' | 'line' | 'heatmap' | 'treemap';

/**
 * Chart data point for visualizations
 */
export interface ChartDataPoint {
  solver: string;
  language?: string;
  algorithm?: string;
  totalTime: number;
  maxMem: number;
  avgIterations: number;
  score?: number;
  tier?: string;
  logo?: string;
  color?: string;
  results?: Array<{
    matrix: string;
    time: number;
    memory: number;
    iterations: number;
  }>;
}

/**
 * Chart configuration options
 */
export interface ChartConfig {
  width?: number;
  height?: number;
  margin?: {
    top: number;
    right: number;
    bottom: number;
    left: number;
  };
  showLogos?: boolean;
  showLabels?: boolean;
  interactive?: boolean;
}

/**
 * Chart dimensions
 */
export interface ChartDimensions {
  width: number;
  height: number;
  margin: {
    top: number;
    right: number;
    bottom: number;
    left: number;
  };
  innerWidth: number;
  innerHeight: number;
}
