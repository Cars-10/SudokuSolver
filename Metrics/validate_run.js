#!/usr/bin/env node
/**
 * Validate Run - CLI Tool
 * Validates metrics.json files against reference iteration counts and output format
 *
 * Usage:
 *   node validate_run.js <path-to-metrics.json>           # Iteration validation
 *   node validate_run.js --format <path-to-metrics.json>  # Format validation
 *   node validate_run.js --all <path-to-metrics.json>     # Both validations
 */

import { validateMetricsFile } from './validation.js';
import { validateOutputFormats } from './format_validation.js';
import Database from 'better-sqlite3';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Parse command line arguments
const args = process.argv.slice(2);
let validationType = 'iterations';  // default
let metricsPath = 'Languages/C/metrics.json';

if (args[0] === '--format') {
    validationType = 'format';
    metricsPath = args[1] || metricsPath;
} else if (args[0] === '--all') {
    validationType = 'all';
    metricsPath = args[1] || metricsPath;
} else {
    metricsPath = args[0] || metricsPath;
}

// Display header based on validation type
if (validationType === 'format') {
    console.log('\n=== Output Format Validation ===\n');
} else if (validationType === 'all') {
    console.log('\n=== Complete Validation (Iterations + Format) ===\n');
} else {
    console.log('\n=== Iteration Count Validation ===\n');
}
console.log(`Validating: ${metricsPath}\n`);

try {
    // Run appropriate validation(s)
    let results = [];

    if (validationType === 'format') {
        results = validateOutputFormats(metricsPath);
    } else if (validationType === 'all') {
        const iterResults = validateMetricsFile(metricsPath);
        const formatResults = validateOutputFormats(metricsPath);

        // Merge results by matrix
        results = iterResults.map((iterResult, idx) => ({
            ...iterResult,
            formatValid: formatResults[idx]?.valid,
            formatError: formatResults[idx]?.error
        }));
    } else {
        results = validateMetricsFile(metricsPath);
    }

    if (results.length === 0) {
        console.log('⚠️  No results found in metrics file\n');
        process.exit(1);
    }

    // Display results
    let allValid = true;
    const summary = { passed: 0, failed: 0, skipped: 0 };

    for (const result of results) {
        const status = result.valid ? '✓ PASS' : '✗ FAIL';
        const statusColor = result.valid ? '\x1b[32m' : '\x1b[31m'; // Green or red
        const resetColor = '\x1b[0m';

        if (result.skipped) {
            console.log(`⊝ SKIP - ${result.language} Matrix ${result.matrix}`);
            console.log(`  Reason: ${result.error}\n`);
            summary.skipped++;
            continue;
        }

        console.log(`${statusColor}${status}${resetColor} - ${result.language} Matrix ${result.matrix}`);

        if (!result.valid) {
            // Show iteration validation details
            if (validationType !== 'format') {
                console.log(`  Expected: ${result.expected?.toLocaleString() || 'N/A'} iterations`);
                console.log(`  Got:      ${result.actual?.toLocaleString() || 'N/A'} iterations`);
                if (result.diff !== undefined) {
                    const diffStr = result.diff > 0 ? `+${result.diff.toLocaleString()}` : result.diff.toLocaleString();
                    console.log(`  Diff:     ${diffStr}`);
                }
            }

            // Show format validation details
            if (validationType === 'format' || validationType === 'all') {
                if (result.lineNumber) {
                    console.log(`  Mismatch at line: ${result.lineNumber}`);
                    console.log(`  Expected: "${result.expectedLine}"`);
                    console.log(`  Got:      "${result.actualLine}"`);
                } else if (result.formatError) {
                    console.log(`  Format error: ${result.formatError}`);
                }
            }

            console.log(`  Error:    ${result.error}`);
            allValid = false;
            summary.failed++;
        } else {
            summary.passed++;
        }

        console.log('');
    }

    // Overall summary
    console.log('─'.repeat(50));
    console.log(`Total:   ${results.length} validations`);
    console.log(`✓ Passed: ${summary.passed}`);
    console.log(`✗ Failed: ${summary.failed}`);
    console.log(`⊝ Skipped: ${summary.skipped}`);
    console.log('─'.repeat(50));

    if (allValid && summary.failed === 0) {
        console.log('\n✅ ALL VALIDATIONS PASSED\n');
        process.exit(0);
    } else {
        console.log('\n❌ SOME VALIDATIONS FAILED\n');
        process.exit(1);
    }

} catch (error) {
    console.error(`\n❌ Error during validation:`);
    console.error(`   ${error.message}\n`);

    if (error.stack && process.env.DEBUG) {
        console.error(error.stack);
    }

    process.exit(1);
}
