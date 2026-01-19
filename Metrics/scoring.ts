import type { MetricResult } from './types.ts';

interface Weights {
    time: number;
    memory: number;
}

const TIMEOUT_SECONDS = 300; // Hardcoded fallback

export function calculateMatrixScore(
    result: MetricResult | undefined,
    cResult: MetricResult | undefined,
    weights: Weights,
    timeout: number = TIMEOUT_SECONDS
): number {
    // 1. Handle Missing C Baseline
    // If we can't compare to C, we can't score. Return 0 or undefined? 
    // For now, return 1.0 (parity) to avoid breaking things, but this shouldn't happen if C is run.
    if (!cResult || cResult.time === 0) return 1.0;

    // 2. Handle Solver Failure (Crash/Timeout)
    // If result is missing or status is not success, apply Penalty
    let time = 0;
    let memory = 0;
    let isFailure = false;

    if (!result || result.status !== 'success') {
        isFailure = true;
        time = timeout; // Penalty: Max Time
        memory = cResult.memory; // Penalty: Assume Parity (neutral) for memory, let Time dominate
    } else {
        time = result.time;
        memory = result.memory;
    }

    // 3. Calculate Ratios (Lower is Better -> Solver / C)
    // Ensure no division by zero (already checked cResult.time)
    // Floor ratios at 0.001 to prevent excessive rewards for unrealistic values
    const timeRatio = Math.max(0.001, time / cResult.time);
    
    // Memory might be 0 in some error cases or weird measurements
    const cMem = Math.max(1, cResult.memory); // Prevent div/0
    const memRatio = Math.max(0.001, memory / cMem);

    // 4. Weighted Geometric Mean
    // Score = (TimeRatio ^ w_time) * (MemRatio ^ w_mem)
    // Weights should sum to 1.0 normally.
    const score = Math.pow(timeRatio, weights.time) * Math.pow(memRatio, weights.memory);

    return score;
}

export function calculateOverallScore(
    results: MetricResult[],
    cResults: MetricResult[],
    weights: Weights,
    timeout: number = TIMEOUT_SECONDS
): number {
    const matrices = ['1', '2', '3', '4', '5']; // Standard suite 1-5
    let totalScore = 0;
    let count = 0;

    for (const m of matrices) {
        // Find result for this matrix (normalize names "1.matrix" -> "1")
        const res = results.find(r => r.matrix.replace('.matrix', '') === m);
        const cRes = cResults.find(r => r.matrix.replace('.matrix', '') === m);

        // Calculate score for this matrix
        // Note: We calculate even if 'res' is missing (it's handled as a failure inside)
        // But we MUST have a C baseline.
        if (cRes) {
            const s = calculateMatrixScore(res, cRes, weights, timeout);
            totalScore += s;
            count++;
        }
    }

    if (count === 0) return 0; // Should not happen

    // Arithmetic Mean of Geometric Means (as per user "Average those scores")
    return totalScore / count;
}
