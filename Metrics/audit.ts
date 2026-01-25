
import * as fs from 'fs';
import * as path from 'path';
import { glob } from 'glob';
import { C_Baselines } from './C_Baselines.js';

const ALGORITHMS_ROOT = path.join(process.cwd(), 'Algorithms');
const MATRIX_COUNT = 6;
const MATRICES = ['1', '2', '3', '4', '5', '6'];

interface ValidationResult {
    language: string;
    algorithm: string;
    status: 'PASS' | 'FAIL' | 'MISSING_METRICS' | 'MISSING_IMPL' | 'MISMATCH';
    details: string[];
}

const results: ValidationResult[] = [];

// Helper to normalize matrix names (remove .matrix extension)
const normalize = (m: string | number) => String(m).replace('.matrix', '');

async function runAudit() {
    console.log("Starting Sudoku Solver Audit...");
    console.log("================================");

    const algoTypes = ['BruteForce', 'DLX', 'CP'];
    const languages = new Set<string>();

    // 1. Discovery Phase
    for (const algo of algoTypes) {
        const algoDir = path.join(ALGORITHMS_ROOT, algo);
        if (!fs.existsSync(algoDir)) continue;

        const entries = fs.readdirSync(algoDir, { withFileTypes: true });
        for (const entry of entries) {
            if (entry.isDirectory() && !entry.name.startsWith('.')) {
                languages.add(entry.name);
            }
        }
    }

    console.log(`Found ${languages.size} unique languages across all algorithms.`);

    // 2. Audit Phase
    for (const lang of Array.from(languages).sort()) {
        for (const algo of algoTypes) {
            const langDir = path.join(ALGORITHMS_ROOT, algo, lang);

            // Check existence
            if (!fs.existsSync(langDir)) {
                // If BruteForce exists, we typically expect others, but maybe not strict requirement yet.
                // We'll mark as MISSING_IMPL if it's missing but exists in BruteForce
                if (algo !== 'BruteForce' && fs.existsSync(path.join(ALGORITHMS_ROOT, 'BruteForce', lang))) {
                    results.push({
                        language: lang,
                        algorithm: algo,
                        status: 'MISSING_IMPL',
                        details: ['Directory does not exist']
                    });
                }
                continue;
            }

            const metricsPath = path.join(langDir, 'metrics.json');
            if (!fs.existsSync(metricsPath)) {
                results.push({
                    language: lang,
                    algorithm: algo,
                    status: 'MISSING_METRICS',
                    details: ['metrics.json not found']
                });
                continue;
            }

            // Check Content
            try {
                const content = fs.readFileSync(metricsPath, 'utf8');
                const metrics = JSON.parse(content);
                // Handle array or single object format (usually array)
                const data = Array.isArray(metrics) ? metrics[0] : metrics;

                if (!data || !data.results) {
                    results.push({
                        language: lang,
                        algorithm: algo,
                        status: 'FAIL',
                        details: ['Invalid metrics.json structure']
                    });
                    continue;
                }

                const runResults = data.results;
                const issues: string[] = [];
                let status: ValidationResult['status'] = 'PASS';

                // Check for failures
                const failures = runResults.filter((r: any) => r.status !== 'success');
                if (failures.length > 0) {
                    issues.push(`Failed matrices: ${failures.map((r: any) => normalize(r.matrix)).join(', ')}`);
                    status = 'FAIL';
                }

                // Check for iterations mismatches (only for successful runs)
                const baseline = C_Baselines[algo as keyof typeof C_Baselines];
                if (baseline) {
                    runResults.forEach((r: any) => {
                        if (r.status === 'success') {
                            const mat = normalize(r.matrix);
                            // Only check if it's one of the standard 6 matrices
                            if (MATRICES.includes(mat)) {
                                const expected = baseline[mat as keyof typeof baseline];
                                if (expected !== undefined && r.iterations !== expected) {
                                    issues.push(`Matrix ${mat}: Expected ${expected}, got ${r.iterations}`);
                                    status = 'MISMATCH';
                                }
                            }
                        }
                    });
                }

                if (issues.length > 0) {
                    results.push({
                        language: lang,
                        algorithm: algo,
                        status: status,
                        details: issues
                    });
                } else if (status === 'PASS') {
                    // Clean Pass - usually we don't log these to keep noise down, or maybe we do?
                    // Let's only log non-passes for the "Audit of failures"
                }

            } catch (e: any) {
                results.push({
                    language: lang,
                    algorithm: algo,
                    status: 'FAIL',
                    details: [`JSON Parse Error: ${e.message}`]
                });
            }
        }
    }

    // 3. Reporting
    generateReport();
}

function generateReport() {
    const failures = results.filter(r => r.status === 'FAIL');
    const mismatches = results.filter(r => r.status === 'MISMATCH');
    const missingMetrics = results.filter(r => r.status === 'MISSING_METRICS');
    const missingImpl = results.filter(r => r.status === 'MISSING_IMPL');

    console.log("\nABC AUDIT REPORT");
    console.log("================\n");

    if (failures.length > 0) {
        console.log(`🔴 FAILURES (${failures.length})`);
        console.log("--------------------------------");
        failures.forEach(r => {
            console.log(`- [${r.algorithm}] ${r.language}: ${r.details.join('; ')}`);
        });
        console.log("");
    }

    if (mismatches.length > 0) {
        console.log(`⚠️  ITERATION MISMATCHES (${mismatches.length})`);
        console.log("--------------------------------");
        console.log("(Algorithm must strictly match C reference backtracking path)\n");
        mismatches.forEach(r => {
            console.log(`- [${r.algorithm}] ${r.language}`);
            r.details.forEach(d => console.log(`    ${d}`));
        });
        console.log("");
    }

    if (missingMetrics.length > 0) {
        console.log(`⚪ MISSING METRICS (${missingMetrics.length})`);
        console.log("--------------------------------");
        console.log("(Directory exists, but no metrics.json)\n");
        missingMetrics.forEach(r => {
            console.log(`- [${r.algorithm}] ${r.language}`);
        });
        console.log("");
    }

    // Group Missing Impl by Language
    if (missingImpl.length > 0) {
        console.log(`Empty Slots (Missing Implementations)`);
        console.log("-----------------------------------");
        const grouped = new Map<string, string[]>();
        missingImpl.forEach(r => {
            if (!grouped.has(r.language)) grouped.set(r.language, []);
            grouped.get(r.language)?.push(r.algorithm);
        });

        Array.from(grouped.entries()).forEach(([lang, algos]) => {
            console.log(`- ${lang}: Missing ${algos.join(', ')}`);
        });
    }
}

runAudit();
