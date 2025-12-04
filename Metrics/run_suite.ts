import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';
import {
    findSolvers,
    runSolver,
    generateHtml,
    captureScreenshot,
    personalities,
    languageMetadata,
    methodologyTexts
} from './gather_metrics.ts';
import type { SolverMetrics } from './gather_metrics.ts';

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

    const rootDir = path.resolve(__dirname, '..');
    const metricsFile = path.join(rootDir, 'metrics.json');
    const htmlFile = path.join(rootDir, 'benchmark_report.html');

    let allMetrics: SolverMetrics[] = [];
    try {
        const data = await fs.readFile(metricsFile, 'utf-8');
        allMetrics = JSON.parse(data);
        console.log(`Loaded ${allMetrics.length} existing metrics.`);
    } catch (e) {
        console.log("No existing metrics found, starting fresh.");
    }

    const allSolvers = await findSolvers(rootDir);
    let solversToRun = allSolvers;

    if (language.toLowerCase() !== 'all') {
        solversToRun = allSolvers.filter(s => {
            const solverDir = path.dirname(s);
            const langName = path.basename(solverDir);
            return langName.toLowerCase() === language.toLowerCase();
        });
    }

    if (solversToRun.length === 0) {
        console.error(`No solvers found matching '${language}'`);
        process.exit(1);
    }

    console.log(`Found ${solversToRun.length} solvers to run.`);

    for (const matrix of matrices) {
        console.log(`\n--- Benchmarking ${matrix} ---\n`);
        for (const script of solversToRun) {
            const metrics = await runSolver(script, matrix);
            if (metrics) {
                let existingSolver = allMetrics.find(s => s.solver === metrics.solver);
                if (!existingSolver) {
                    existingSolver = { solver: metrics.solver, timestamp: metrics.timestamp, results: [] };
                    allMetrics.push(existingSolver);
                }

                // Update results
                for (const res of metrics.results) {
                    existingSolver.results = existingSolver.results.filter(r => r.matrix !== res.matrix);
                    existingSolver.results.push(res);
                }

                // Save & Generate
                await fs.writeFile(metricsFile, JSON.stringify(allMetrics, null, 2));

                const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts);
                await fs.writeFile(htmlFile, htmlContent);
                console.log(`Updated report for ${metrics.solver}`);

                // Only take screenshot if running a small batch (spot check) or if it's the last one?
                // The user said "I still want to take screenshots and stuff".
                // gather_metrics.ts takes it after EVERY run. We'll stick to that for now.
                await captureScreenshot(htmlFile);
            }
        }
    }
}

runSuite().catch(console.error);
