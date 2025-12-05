import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';
import {
    findSolvers,
    runSolver,
    runDockerSolver,
    generateHtml,
    captureScreenshot,
    personalities,
    languageMetadata,
    methodologyTexts,
    readReferenceOutputs,
    HistoryManager,
    generateHistoryHtml
} from './gather_metrics.ts';
import type { SolverMetrics, MetricResult } from './gather_metrics.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function runSuite() {
    const args = process.argv.slice(2);
    let language = 'all';
    let matrices = ['3.matrix', '4.matrix', '5.matrix']; // Default as per gather_metrics.ts

    // Simple argument parsing
    // Usage: ts-node run_suite.ts [language] [matrix1,matrix2,...]

    if (args.length > 0) {
        language = args[0];
    }

    if (args.length > 1) {
        const matrixArg = args[1];
        if (matrixArg.toLowerCase() === 'all') {
            // Find all matrices in ../Matrices
            const matricesDir = path.resolve(__dirname, '../Matrices');
            // We can't easily list them here without importing glob or fs, but let's assume standard 1-6
            matrices = ['1.matrix', '2.matrix', '3.matrix', '4.matrix', '5.matrix', '6.matrix'];
        } else {
            matrices = matrixArg.split(',').map(m => {
                if (!m.endsWith('.matrix')) return `${m}.matrix`;
                return m;
            });
        }
    }

    console.log(`Configuration:`);
    console.log(`  Language: ${language}`);
    console.log(`  Matrices: ${matrices.join(', ')}`);

    const repoRoot = path.resolve(__dirname, '..');
    const outputDir = process.env.OUTPUT_DIR || repoRoot;
    const metricsFile = process.env.METRICS_FILE
        ? path.join(outputDir, process.env.METRICS_FILE)
        : path.join(repoRoot, 'CleanedUp', 'CleanedUp_Metrics.json');
    const htmlFile = path.join(outputDir, 'benchmark_report.html');

    let allMetrics: SolverMetrics[] = [];
    try {
        const data = await fs.readFile(metricsFile, 'utf-8');
        allMetrics = JSON.parse(data);
        console.log(`Loaded ${allMetrics.length} existing metrics.`);

        // Migration: Fix legacy " (Docker)" names
        let migratedCount = 0;
        for (const m of allMetrics) {
            if (m.solver.endsWith(' (Docker)')) {
                m.solver = m.solver.replace(' (Docker)', '');
                m.runType = 'Docker';
                migratedCount++;
            } else if (!m.runType) {
                // Assume Local for legacy entries without runType
                // But check if it was Manual or AI based on some other heuristic? 
                // For now, default to Local is safe as per plan.
                m.runType = 'Local';
            }
        }
        if (migratedCount > 0) {
            console.log(`Migrated ${migratedCount} legacy Docker entries.`);
        }
    } catch (e) {
        console.log("No existing metrics found, starting fresh.");
    }

    const allSolvers = await findSolvers(repoRoot);
    let solversToRun = allSolvers;

    if (language.toLowerCase() !== 'all') {
        const requestedLangs = language.toLowerCase().split(',').map(l => l.trim());
        solversToRun = allSolvers.filter(s => {
            const solverDir = path.dirname(s);
            const langName = path.basename(solverDir);
            return requestedLangs.includes(langName.toLowerCase());
        });
    }

    if (solversToRun.length === 0) {
        console.error(`No solvers found matching '${language}'`);
        process.exit(1);
    }

    console.log(`Found ${solversToRun.length} solvers to run.`);

    const useDocker = args.includes('--docker');

    for (const matrix of matrices) {
        console.log(`\n--- Benchmarking ${matrix} ---\n`);
        for (const script of solversToRun) {
            let metrics: SolverMetrics | null = null;
            if (useDocker) {
                metrics = await runDockerSolver(script, matrix);
            } else {
                metrics = await runSolver(script, matrix);
            }

            if (metrics) {
                // Find existing solver by name AND runType
                // For legacy metrics without runType, assume 'Local' if the new one is 'Local', or just match by name if runType is undefined
                let existingSolver = allMetrics.find(s =>
                    s.solver === metrics!.solver &&
                    (s.runType === metrics!.runType || (!s.runType && metrics!.runType === 'Local'))
                );

                if (!existingSolver) {
                    existingSolver = {
                        solver: metrics.solver,
                        runType: metrics.runType,
                        timestamp: metrics.timestamp,
                        results: []
                    };
                    allMetrics.push(existingSolver);
                }

                // Update results
                for (const res of metrics.results) {
                    existingSolver.results = existingSolver.results.filter((r: MetricResult) => r.matrix !== res.matrix);
                    existingSolver.results.push(res);
                }

                // Save & Generate
                await fs.writeFile(metricsFile, JSON.stringify(allMetrics, null, 2));

                const referenceOutputs = await readReferenceOutputs(repoRoot);
                console.log(`Generating HTML with ${allMetrics.length} metrics.`);
                const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts, referenceOutputs, []);
                await fs.writeFile(htmlFile, htmlContent);
                console.log(`Updated report for ${metrics.solver}`);

                // Save to History
                const historyManager = new HistoryManager(repoRoot);
                // Create a snapshot of this specific run
                const runSnapshot: SolverMetrics = {
                    solver: metrics.solver,
                    runType: metrics.runType,
                    timestamp: metrics.timestamp,
                    results: metrics.results
                };
                await historyManager.appendRecord(runSnapshot);

                // Generate History Report
                const fullHistory = await historyManager.getHistory();
                const historyHtml = await generateHistoryHtml(fullHistory);
                const historyHtmlFile = path.join(repoRoot, 'benchmark_history.html');
                await fs.writeFile(historyHtmlFile, historyHtml);
                console.log(`Updated history report at ${historyHtmlFile}`);

                // Only take screenshot if running a small batch (spot check) or if it's the last one?
                // The user said "I still want to take screenshots and stuff".
                // gather_metrics.ts takes it after EVERY run. We'll stick to that for now.
                await captureScreenshot(htmlFile);
            }
        }
    }
}

runSuite().catch(console.error);
