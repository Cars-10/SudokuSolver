#!/usr/bin/env node
/**
 * Validation Library
 * Validates iteration counts and output format against C reference
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ============================================================================
// ITERATION COUNT VALIDATION
// ============================================================================

/**
 * Load reference iteration counts from JSON file
 * @returns {Object} Map of matrix number to expected iterations
 */
export function loadReferenceIterations() {
    const refPath = path.join(__dirname, '..', 'Matrices', 'reference_iterations.json');

    if (!fs.existsSync(refPath)) {
        throw new Error(`Reference iterations file not found: ${refPath}`);
    }

    return JSON.parse(fs.readFileSync(refPath, 'utf8'));
}

/**
 * Validate iteration count for a single matrix
 * @param {string|number} matrix - Matrix number (e.g., "1" or 1)
 * @param {number} actualIterations - Actual iteration count from solver
 * @returns {Object} Validation result with valid flag and details
 */
export function validateIterations(matrix, actualIterations) {
    const reference = loadReferenceIterations();
    const matrixNum = String(matrix).replace('.matrix', '');
    const expected = reference[matrixNum];

    if (!expected) {
        return {
            valid: false,
            error: `No reference iteration count for matrix ${matrixNum}`,
            matrix: matrixNum
        };
    }

    if (actualIterations !== expected) {
        return {
            valid: false,
            error: `Iteration mismatch for matrix ${matrixNum}`,
            expected,
            actual: actualIterations,
            diff: actualIterations - expected,
            matrix: matrixNum
        };
    }

    return {
        valid: true,
        expected,
        actual: actualIterations,
        matrix: matrixNum
    };
}

/**
 * Validate all results from a metrics.json file
 * @param {string} metricsPath - Path to metrics.json file
 * @returns {Array} Array of validation results
 */
export function validateMetricsFile(metricsPath) {
    if (!fs.existsSync(metricsPath)) {
        throw new Error(`Metrics file not found: ${metricsPath}`);
    }

    const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));
    const results = [];

    // Metrics file is an array of run objects
    for (const run of metrics) {
        const language = run.solver || 'Unknown';

        // Each run has a results array
        for (const result of run.results || []) {
            // Only validate successful runs
            if (result.status === 'success') {
                const validation = validateIterations(result.matrix, result.iterations);
                results.push({
                    language,
                    ...validation
                });
            } else {
                // Record non-success status
                results.push({
                    language,
                    matrix: result.matrix,
                    valid: false,
                    error: `Run status: ${result.status}`,
                    skipped: true
                });
            }
        }
    }

    return results;
}

/**
 * Insert validation result into database
 * @param {Object} db - Better-sqlite3 database instance
 * @param {Object} validation - Validation result object
 */
export function recordValidation(db, validation) {
    const stmt = db.prepare(`
        INSERT INTO validations (
            language,
            matrix,
            expected_iterations,
            actual_iterations,
            valid,
            error_message,
            validated_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?)
    `);

    stmt.run(
        validation.language,
        validation.matrix,
        validation.expected || 0,
        validation.actual || 0,
        validation.valid ? 1 : 0,
        validation.error || null,
        new Date().toISOString()
    );
}

/**
 * Get validation summary for a language
 * @param {Object} db - Better-sqlite3 database instance
 * @param {string} language - Language name
 * @returns {Object} Summary with pass/fail counts
 */
export function getValidationSummary(db, language) {
    const summary = db.prepare(`
        SELECT
            COUNT(*) as total,
            SUM(CASE WHEN valid = 1 THEN 1 ELSE 0 END) as passed,
            SUM(CASE WHEN valid = 0 THEN 1 ELSE 0 END) as failed
        FROM validations
        WHERE language = ?
        ORDER BY validated_at DESC
        LIMIT 10
    `).get(language);

    return summary;
}

// ============================================================================
// EXPORTS
// ============================================================================

export default {
    loadReferenceIterations,
    validateIterations,
    validateMetricsFile,
    recordValidation,
    getValidationSummary
};
