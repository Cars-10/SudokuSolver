import { sampleCorrelation, linearRegression, linearRegressionLine, rSquared, quantile, interquartileRange } from 'simple-statistics';
import { calculateOverallScore } from './scoring.ts';
import type { SolverMetrics, MetricResult, WeightScenario, ScenarioResult, SensitivityResult, RankStabilityResult, CorrelationResult, OutlierAnalysis, PercentileResult } from './types.ts';

// Weight scenarios for sensitivity analysis
export const WEIGHT_SCENARIOS: WeightScenario[] = [
    { name: 'Time Only', weights: { time: 1.0, memory: 0.0 } },
    { name: 'Current (80/20)', weights: { time: 0.8, memory: 0.2 } },
    { name: 'Balanced (50/50)', weights: { time: 0.5, memory: 0.5 } },
    { name: 'Memory Only', weights: { time: 0.0, memory: 1.0 } }
];

/**
 * Calculate scores for all languages across all weight scenarios.
 * Returns a map of language -> array of scenario results.
 */
export function calculateSensitivityScores(
    metrics: SolverMetrics[],
    cResults: MetricResult[]
): Map<string, ScenarioResult[]> {
    const resultsByLanguage = new Map<string, ScenarioResult[]>();

    WEIGHT_SCENARIOS.forEach(scenario => {
        // Calculate scores for all languages under this scenario
        const languageScores = metrics.map(m => ({
            language: m.solver,
            score: calculateOverallScore(m.results, cResults, scenario.weights)
        }));

        // Sort by score (lower is better) and assign ranks
        languageScores.sort((a, b) => a.score - b.score);

        languageScores.forEach((lang, index) => {
            const rank = index + 1;
            if (!resultsByLanguage.has(lang.language)) {
                resultsByLanguage.set(lang.language, []);
            }
            resultsByLanguage.get(lang.language)!.push({
                scenario: scenario.name,
                score: lang.score,
                rank
            });
        });
    });

    return resultsByLanguage;
}

/**
 * Calculate rank stability metrics for all languages.
 * Max swing = worst rank - best rank across all scenarios.
 */
export function calculateRankStability(
    sensitivityResults: Map<string, ScenarioResult[]>
): RankStabilityResult[] {
    const stability: RankStabilityResult[] = [];

    Array.from(sensitivityResults.entries()).forEach(([language, scenarios]) => {
        const ranks = scenarios.map(s => s.rank);
        const bestRank = Math.min(...ranks);
        const worstRank = Math.max(...ranks);
        const maxSwing = worstRank - bestRank;

        // Find current rank (80/20 scenario)
        const currentScenario = scenarios.find(s => s.scenario === 'Current (80/20)');
        const currentRank = currentScenario?.rank ?? 0;

        stability.push({ language, maxSwing, bestRank, worstRank, currentRank });
    });

    // Sort by max swing (descending) to find most unstable
    stability.sort((a, b) => b.maxSwing - a.maxSwing);

    return stability;
}

/**
 * Convert sensitivity map to array format for easier consumption.
 */
export function sensitivityMapToArray(
    sensitivityMap: Map<string, ScenarioResult[]>
): SensitivityResult[] {
    const results: SensitivityResult[] = [];
    Array.from(sensitivityMap.entries()).forEach(([language, scenarios]) => {
        results.push({ language, scenarios });
    });
    return results;
}

/**
 * Compute Pearson correlation coefficient and R^2 between time and memory.
 * R^2 indicates how much variance in memory is explained by time.
 */
export function computeCorrelation(metrics: SolverMetrics[]): CorrelationResult {
    // Extract total time and memory for each language
    const timeData: number[] = [];
    const memoryData: number[] = [];

    metrics.forEach(m => {
        const totalTime = m.results.reduce((acc, r) => acc + r.time, 0);
        const maxMemory = Math.max(...m.results.map(r => r.memory));
        if (totalTime > 0 && maxMemory > 0) {
            timeData.push(totalTime);
            memoryData.push(maxMemory);
        }
    });

    // Need at least 3 data points for meaningful correlation
    if (timeData.length < 3) {
        return {
            rValue: 0,
            rSquared: 0,
            interpretation: 'Insufficient data for correlation analysis (need at least 3 languages with valid results).'
        };
    }

    // Calculate Pearson correlation coefficient
    const rValue = sampleCorrelation(timeData, memoryData);

    // Calculate R^2 using linear regression
    const samples = timeData.map((t, i) => [t, memoryData[i]] as [number, number]);
    const regression = linearRegression(samples);
    const regressionLine = linearRegressionLine(regression);
    const r2 = rSquared(samples, regressionLine);

    // Generate interpretation
    const interpretation = interpretCorrelation(r2);

    return { rValue, rSquared: r2, interpretation };
}

/**
 * Provide plain-English interpretation of R^2 correlation value.
 */
export function interpretCorrelation(r2: number): string {
    if (isNaN(r2)) {
        return 'Correlation could not be computed (data may have zero variance).';
    }

    if (r2 >= 0.8) {
        return `R² = ${r2.toFixed(2)} indicates a very strong correlation between time and memory performance. Languages that are fast tend to also be memory-efficient.`;
    } else if (r2 >= 0.5) {
        return `R² = ${r2.toFixed(2)} indicates a moderate correlation between time and memory performance. There's a noticeable relationship, but other factors also influence memory usage.`;
    } else if (r2 >= 0.3) {
        return `R² = ${r2.toFixed(2)} indicates a weak correlation between time and memory performance. Time and memory efficiency are somewhat independent characteristics.`;
    } else {
        return `R² = ${r2.toFixed(2)} indicates little to no correlation between time and memory performance. Being fast doesn't predict memory efficiency in this benchmark suite.`;
    }
}

/**
 * Identify statistical outliers using IQR method.
 * IQR is more robust than Z-score for non-normal (skewed) distributions.
 */
export function identifyOutliers(metrics: SolverMetrics[]): OutlierAnalysis[] {
    const outliers: OutlierAnalysis[] = [];

    // Extract total time and max memory for each language
    const timeValues = metrics.map(m => ({
        language: m.solver,
        value: m.results.reduce((acc, r) => acc + r.time, 0)
    })).filter(t => t.value > 0);

    const memoryValues = metrics.map(m => ({
        language: m.solver,
        value: Math.max(...m.results.map(r => r.memory))
    })).filter(m => m.value > 0);

    // Detect time outliers
    const timeOutliers = detectOutliersIQR(timeValues, 'time');
    outliers.push(...timeOutliers);

    // Detect memory outliers
    const memoryOutliers = detectOutliersIQR(memoryValues, 'memory');
    outliers.push(...memoryOutliers);

    return outliers;
}

/**
 * Internal helper for IQR-based outlier detection.
 * Outliers are values below Q1 - 1.5*IQR or above Q3 + 1.5*IQR.
 */
function detectOutliersIQR(
    data: Array<{ language: string; value: number }>,
    metricType: 'time' | 'memory' | 'score'
): OutlierAnalysis[] {
    const outliers: OutlierAnalysis[] = [];

    if (data.length < 4) return outliers; // Need enough data for quartiles

    const values = data.map(d => d.value);

    // Calculate quartiles
    const q1 = quantile(values, 0.25);
    const q3 = quantile(values, 0.75);
    const iqr = interquartileRange(values);

    // IQR outlier thresholds
    const lowerBound = q1 - 1.5 * iqr;
    const upperBound = q3 + 1.5 * iqr;

    // Check each value
    data.forEach(({ language, value }) => {
        if (value < lowerBound) {
            outliers.push({
                language,
                metric: metricType,
                value,
                threshold: lowerBound,
                direction: 'low',
                explanation: `Exceptionally fast ${metricType} performance (below Q1 - 1.5×IQR threshold of ${lowerBound.toFixed(2)})`
            });
        } else if (value > upperBound) {
            outliers.push({
                language,
                metric: metricType,
                value,
                threshold: upperBound,
                direction: 'high',
                explanation: `Unusually slow ${metricType} performance (above Q3 + 1.5×IQR threshold of ${upperBound.toFixed(2)})`
            });
        }
    });

    return outliers;
}

/**
 * Calculate percentiles for a dataset.
 * Useful for understanding score distribution.
 */
export function calculatePercentiles(data: number[]): PercentileResult {
    if (data.length === 0) {
        return { p25: 0, p50: 0, p75: 0, p90: 0, p99: 0 };
    }

    return {
        p25: quantile(data, 0.25),
        p50: quantile(data, 0.50),  // Median
        p75: quantile(data, 0.75),
        p90: quantile(data, 0.90),
        p99: quantile(data, 0.99)
    };
}

/**
 * Get score percentiles for all languages.
 */
export function getScorePercentiles(metrics: SolverMetrics[]): PercentileResult {
    const scores = metrics
        .map(m => m.score)
        .filter((s): s is number => s !== undefined && s > 0);

    return calculatePercentiles(scores);
}
