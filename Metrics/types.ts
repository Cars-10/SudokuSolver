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

export interface SolverMetrics {
    solver: string;
    algorithmType?: 'BruteForce' | 'DLX' | 'CP'; // Algorithm type from directory structure
    runType?: string; // 'Local' | 'Docker' | 'AI'
    timestamp: string;
    score?: number; // Calculated composite score
    failed?: boolean; // If true, indicates a recent benchmark run failed
    failureReason?: string; // Reason for failure (from benchmark_issues.json)
    results: MetricResult[];
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
    currentRank: number;  // Rank under current 80/20 weighting
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
