// Data loading and access service
import type { SolverMetrics } from '../types/metrics';

export class MetricsService {
  private data: SolverMetrics[] = [];

  async load(): Promise<void> {
    // Data is injected via window.metricsData by HTMLGenerator
    this.data = (window as any).metricsData || [];

    // Compute derived properties
    this.data.forEach(metric => {
      // Extract language and algorithm from solver name
      const parts = metric.solver.split('/');
      if (parts.length >= 3) {
        metric.algorithm = parts[1]; // e.g., "BruteForce"
        metric.language = parts[2];  // e.g., "Python"
      }

      // Compute totals
      if (metric.results && metric.results.length > 0) {
        metric.totalTime = metric.results.reduce((sum, r) => sum + r.time, 0);
        metric.avgIterations = metric.results.reduce((sum, r) => sum + r.iterations, 0) / metric.results.length;
        metric.avgMemory = metric.results.reduce((sum, r) => sum + r.memory, 0) / metric.results.length;
      }
    });

    console.debug('[MetricsService] Loaded', this.data.length, 'metrics');
  }

  getAll(): SolverMetrics[] {
    return this.data;
  }

  getByLanguage(language: string): SolverMetrics | undefined {
    return this.data.find(m => m.language === language);
  }

  getByAlgorithm(algorithm: string): SolverMetrics[] {
    return this.data.filter(m => m.algorithm === algorithm);
  }

  search(term: string): SolverMetrics[] {
    const lower = term.toLowerCase();
    return this.data.filter(m =>
      m.language?.toLowerCase().includes(lower) ||
      m.algorithm?.toLowerCase().includes(lower) ||
      m.solver.toLowerCase().includes(lower)
    );
  }

  // Get top N performers by a metric
  getTopN(n: number, metric: 'totalTime' | 'avgMemory' | 'score' = 'totalTime'): SolverMetrics[] {
    return [...this.data]
      .filter(m => m[metric] !== undefined)
      .sort((a, b) => (a[metric] || 0) - (b[metric] || 0))
      .slice(0, n);
  }

  // Get languages that failed
  getFailed(): SolverMetrics[] {
    return this.data.filter(m => m.failed);
  }
}

export const metricsService = new MetricsService();
