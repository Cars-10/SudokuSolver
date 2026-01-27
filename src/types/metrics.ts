// Migrated from Metrics/types.ts

export interface MetricResult {
  matrix: string;
  time: number;
  iterations: number;
  memory: number;
  cpu_user: number;
  cpu_sys: number;
  status: string;
  output?: string;
}

export interface IterationMismatch {
  matrix: string;
  actual: number;
  expected: number;
}

export interface ScoreBreakdown {
  time: number;
  memory: number;
  cpu: number;
}

export interface SolverMetrics {
  solver: string;
  algorithmType?: 'BruteForce' | 'DLX' | 'CP';
  runType?: string; // 'Local' | 'Docker' | 'AI'
  timestamp: string;
  score?: number;
  tier?: string; // 'S' | 'A' | 'B' | 'C' | 'D' | 'F'
  scoreBreakdown?: ScoreBreakdown;
  failed?: boolean;
  failureReason?: string;
  results: MetricResult[];

  // Computed properties
  language?: string;
  algorithm?: string;
  logo?: string;
  totalTime?: number;
  totalIterations?: number;
  avgIterations?: number;
  avgMemory?: number;
  maxMemory?: number;

  // Iteration mismatch tracking (for algorithm correctness)
  iterationMismatches?: IterationMismatch[];
  hasMismatch?: boolean;
}

// Sensitivity Analysis Types
export interface WeightScenario {
  name: string;
  weights: { time: number; memory: number };
}

export interface ScenarioResult {
  scenario: string;
  score: number;
  rank: number;
}

export interface SensitivityResult {
  language: string;
  scenarios: ScenarioResult[];
}

export interface RankStabilityResult {
  language: string;
  maxSwing: number;
  bestRank: number;
  worstRank: number;
  currentRank: number;
}

// Correlation Analysis Types
export interface CorrelationResult {
  rValue: number;
  rSquared: number;
  interpretation: string;
}

// Outlier Analysis Types
export interface OutlierAnalysis {
  language: string;
  metric: 'time' | 'memory' | 'score';
  value: number;
  threshold: number;
  direction: 'high' | 'low';
  explanation: string;
}

// Percentile Analysis Types
export interface PercentileResult {
  p25: number;
  p50: number;  // Median
  p75: number;
  p90: number;
  p99: number;
}
