import { sampleCorrelation, linearRegression, linearRegressionLine, rSquared, quantile, interquartileRange } from 'simple-statistics';
import { calculateOverallScore } from './scoring';
import type { SolverMetrics, MetricResult, WeightScenario, ScenarioResult, SensitivityResult, RankStabilityResult, CorrelationResult, OutlierAnalysis, PercentileResult } from './types';

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
