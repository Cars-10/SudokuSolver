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
    results: MetricResult[];
}
