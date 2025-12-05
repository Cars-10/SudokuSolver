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
    runType?: string; // 'Local' | 'Docker' | 'AI' | 'Manual'
    timestamp: string;
    results: MetricResult[];
}
