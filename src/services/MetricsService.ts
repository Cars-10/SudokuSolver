// Data loading and access service
import type { SolverMetrics, MetricResult } from '../types/metrics';

// Tier thresholds based on score relative to baseline (C)
const TIER_THRESHOLDS = {
  S: 0.95,  // Better than baseline
  A: 1.05,  // Equal to baseline
  B: 1.50,  // Up to 50% worse
  C: 3.00,  // Up to 3x worse
  D: 10.00, // Up to 10x worse
  F: Infinity
};

// Reference iteration counts for BruteForce algorithm
const REFERENCE_ITERATIONS: Record<string, number> = {
  '1': 656,
  '2': 439269,
  '3': 98847,
  '4': 9085,
  '5': 445778,
  '6': 622577597
};

export class MetricsService {
  private data: SolverMetrics[] = [];
  private baseline: SolverMetrics | null = null;
  private loaded = false;

  async load(): Promise<void> {
    if (this.loaded) return;

    // Try fetching from the API first (preferred - always fresh data)
    try {
      const response = await fetch('/api/metrics');
      if (response.ok) {
        this.data = await response.json();
        console.debug('[MetricsService] Loaded from API:', this.data.length, 'metrics');
        this.processMetrics();
        this.loaded = true;
        return;
      }
    } catch (err) {
      console.debug('[MetricsService] API not available, trying fallbacks');
    }

    // Fallback: Try window.metricsData (for legacy builds)
    if ((window as any).metricsData) {
      this.data = (window as any).metricsData;
      console.debug('[MetricsService] Loaded from window.metricsData:', this.data.length, 'metrics');
      this.processMetrics();
      this.loaded = true;
      return;
    }

    console.warn('[MetricsService] No data source available. Make sure the server is running.');
    this.data = [];
    this.loaded = true;
  }

  /**
   * Process raw metrics to compute derived properties
   */
  private processMetrics(): void {
    // First pass: extract basic info and compute totals
    this.data.forEach(metric => {
      // Extract language and algorithm from solver name
      const parts = metric.solver.split('/');
      if (parts.length >= 3) {
        metric.algorithm = parts[1]; // e.g., "BruteForce"
        metric.language = parts[2];  // e.g., "Python"
      } else if (metric.solver) {
        // Fallback: solver might just be the language name
        metric.language = metric.solver;
        metric.algorithm = metric.algorithmType || 'BruteForce';
      }

      // Compute totals from results
      if (metric.results && metric.results.length > 0) {
        metric.totalTime = metric.results.reduce((sum, r) => sum + r.time, 0);
        metric.totalIterations = metric.results.reduce((sum, r) => sum + r.iterations, 0);
        metric.avgIterations = metric.totalIterations / metric.results.length;
        metric.avgMemory = metric.results.reduce((sum, r) => sum + r.memory, 0) / metric.results.length;
        metric.maxMemory = Math.max(...metric.results.map(r => r.memory));
      }

      // Generate logo path
      metric.logo = this.getLogoPath(metric.language || metric.solver);

      // Check for iteration mismatches (algorithm correctness)
      if (metric.algorithm === 'BruteForce') {
        metric.iterationMismatches = this.checkIterationMismatches(metric.results || []);
        metric.hasMismatch = metric.iterationMismatches.length > 0;
      }

      // Detect failed entries (no results, all errors, or explicit failed flag)
      if (!metric.failed) {
        const hasNoResults = !metric.results || metric.results.length === 0;
        const allErrors = metric.results?.every(r =>
          r.status === 'error' || r.status === 'timeout' || r.status === 'failed'
        );
        metric.failed = hasNoResults || allErrors || false;
      }
    });

    // Find C baseline (BruteForce algorithm)
    this.baseline = this.data.find(m =>
      m.language === 'C' && m.algorithm === 'BruteForce'
    ) || null;

    // Second pass: compute scores relative to baseline
    if (this.baseline) {
      this.data.forEach(metric => {
        const { score, tier, breakdown } = this.calculateScore(metric, this.baseline!);
        metric.score = score;
        metric.tier = tier;
        metric.scoreBreakdown = breakdown;
      });
    }

    // Sort by score (best first)
    this.data.sort((a, b) => (a.score || Infinity) - (b.score || Infinity));
  }

  /**
   * Get logo path for a language
   */
  private getLogoPath(language: string): string {
    const normalizedLang = language.replace(/\s+/g, '');
    return `Algorithms/BruteForce/${normalizedLang}/Media/${normalizedLang}_logo.png`;
  }

  /**
   * Check for iteration count mismatches
   */
  private checkIterationMismatches(results: MetricResult[]): { matrix: string; actual: number; expected: number }[] {
    const mismatches: { matrix: string; actual: number; expected: number }[] = [];

    results.forEach(result => {
      const expected = REFERENCE_ITERATIONS[result.matrix];
      if (expected !== undefined && result.iterations !== expected) {
        mismatches.push({
          matrix: result.matrix,
          actual: result.iterations,
          expected
        });
      }
    });

    return mismatches;
  }

  /**
   * Calculate score relative to baseline (C)
   */
  private calculateScore(metric: SolverMetrics, baseline: SolverMetrics): {
    score: number;
    tier: string;
    breakdown: { time: number; memory: number; cpu: number };
  } {
    if (!metric.results || !baseline.results) {
      return { score: Infinity, tier: 'F', breakdown: { time: 0, memory: 0, cpu: 0 } };
    }

    const timeRatios: number[] = [];
    const memRatios: number[] = [];
    const cpuRatios: number[] = [];

    // Calculate ratios for each matrix result
    metric.results.forEach(result => {
      const baseResult = baseline.results?.find(r => r.matrix === result.matrix);
      if (!baseResult) return;

      // Time ratio (higher = worse)
      if (baseResult.time > 0 && result.time > 0) {
        timeRatios.push(result.time / baseResult.time);
      }

      // Memory ratio (higher = worse)
      if (baseResult.memory > 0 && result.memory > 0) {
        memRatios.push(result.memory / baseResult.memory);
      }

      // CPU ratio using user+sys time if available
      const baseCpu = (baseResult.cpu_user || 0) + (baseResult.cpu_sys || 0);
      const metricCpu = (result.cpu_user || 0) + (result.cpu_sys || 0);
      if (baseCpu > 0 && metricCpu > 0) {
        cpuRatios.push(metricCpu / baseCpu);
      }
    });

    // Calculate averages
    const avgTime = timeRatios.length > 0
      ? timeRatios.reduce((a, b) => a + b, 0) / timeRatios.length
      : 1;
    const avgMem = memRatios.length > 0
      ? memRatios.reduce((a, b) => a + b, 0) / memRatios.length
      : 1;
    const avgCpu = cpuRatios.length > 0
      ? cpuRatios.reduce((a, b) => a + b, 0) / cpuRatios.length
      : 1;

    // Composite score: geometric mean of 3 ratios
    const score = Math.pow(
      Math.max(0.001, avgTime) * Math.max(0.001, avgMem) * Math.max(0.001, avgCpu),
      1 / 3
    );

    // Determine tier
    let tier = 'F';
    if (score < TIER_THRESHOLDS.S) tier = 'S';
    else if (score < TIER_THRESHOLDS.A) tier = 'A';
    else if (score < TIER_THRESHOLDS.B) tier = 'B';
    else if (score < TIER_THRESHOLDS.C) tier = 'C';
    else if (score < TIER_THRESHOLDS.D) tier = 'D';

    return {
      score,
      tier,
      breakdown: { time: avgTime, memory: avgMem, cpu: avgCpu }
    };
  }

  /**
   * Force reload metrics from the API
   */
  async reload(): Promise<void> {
    this.loaded = false;
    this.data = [];
    await this.load();
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
