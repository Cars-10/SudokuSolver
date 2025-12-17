#!/usr/bin/env node
/**
 * Format Validation Library
 * Validates solver output format against C reference
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ============================================================================
// OUTPUT FORMAT VALIDATION
// ============================================================================

/**
 * Normalize output for comparison
 * Handles minor whitespace variations while preserving structure
 * @param {string} output - Raw solver output
 * @returns {string} Normalized output
 */
function normalizeOutput(output) {
    return output
        .trim()
        .split('\n')
        .map(line => line.trimEnd())  // Remove trailing spaces from each line
        .join('\n');
}

/**
 * Load reference output for a matrix
 * @param {string|number} matrix - Matrix number
 * @returns {string} Reference output text
 */
export function loadReferenceOutput(matrix) {
    const matrixNum = String(matrix).replace('.matrix', '');
    const refPath = path.join(__dirname, '..', 'Matrices', 'reference_output', `${matrixNum}.txt`);

    if (!fs.existsSync(refPath)) {
        throw new Error(`Reference output not found for matrix ${matrixNum}: ${refPath}`);
    }

    return fs.readFileSync(refPath, 'utf8');
}

/**
 * Compare two outputs line by line
 * @param {string} actual - Actual solver output
 * @param {string} reference - Reference output
 * @returns {Object} Comparison result with valid flag and details
 */
export function compareOutputs(actual, reference) {
    const normalizedActual = normalizeOutput(actual);
    const normalizedRef = normalizeOutput(reference);

    // Quick check for exact match
    if (normalizedActual === normalizedRef) {
        return {
            valid: true,
            message: 'Output matches reference exactly'
        };
    }

    // Find first difference line-by-line
    const actualLines = normalizedActual.split('\n');
    const refLines = normalizedRef.split('\n');
    const maxLines = Math.max(actualLines.length, refLines.length);

    for (let i = 0; i < maxLines; i++) {
        const actualLine = actualLines[i] || '(end of output)';
        const refLine = refLines[i] || '(end of output)';

        if (actualLine !== refLine) {
            return {
                valid: false,
                error: `Output format mismatch at line ${i + 1}`,
                lineNumber: i + 1,
                expectedLine: refLine,
                actualLine: actualLine,
                context: {
                    before: refLines.slice(Math.max(0, i - 2), i),
                    after: refLines.slice(i + 1, Math.min(refLines.length, i + 3))
                }
            };
        }
    }

    // Should not reach here if outputs differ
    return {
        valid: false,
        error: 'Outputs differ but difference not found (internal error)'
    };
}

/**
 * Validate output format for a single matrix result
 * @param {string|number} matrix - Matrix number
 * @param {string} actualOutput - Actual solver output
 * @returns {Object} Validation result
 */
export function validateOutputFormat(matrix, actualOutput) {
    try {
        const reference = loadReferenceOutput(matrix);
        const result = compareOutputs(actualOutput, reference);

        return {
            matrix: String(matrix).replace('.matrix', ''),
            ...result
        };
    } catch (error) {
        return {
            matrix: String(matrix).replace('.matrix', ''),
            valid: false,
            error: error.message
        };
    }
}

/**
 * Validate all outputs from a metrics file
 * @param {string} metricsPath - Path to metrics.json
 * @returns {Array} Array of format validation results
 */
export function validateOutputFormats(metricsPath) {
    if (!fs.existsSync(metricsPath)) {
        throw new Error(`Metrics file not found: ${metricsPath}`);
    }

    const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));
    const results = [];

    for (const run of metrics) {
        const language = run.solver || 'Unknown';

        for (const result of run.results || []) {
            // Only validate successful runs with output
            if (result.status === 'success' && result.output) {
                const validation = validateOutputFormat(result.matrix, result.output);
                results.push({
                    language,
                    ...validation
                });
            } else {
                results.push({
                    language,
                    matrix: result.matrix,
                    valid: false,
                    error: `Cannot validate format: status=${result.status}, hasOutput=${!!result.output}`,
                    skipped: true
                });
            }
        }
    }

    return results;
}

// ============================================================================
// EXPORTS
// ============================================================================

export default {
    normalizeOutput,
    loadReferenceOutput,
    compareOutputs,
    validateOutputFormat,
    validateOutputFormats
};
