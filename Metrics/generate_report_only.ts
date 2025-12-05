import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { glob } from 'glob';
import {
    generateHtml,
    captureScreenshot,
    personalities,
    languageMetadata,
    methodologyTexts,
    mismatchLabels,
    narratorIntros
} from './gather_metrics.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const outputDir = process.env.OUTPUT_DIR || rootDir;
const metricsFile = path.join(outputDir, process.env.METRICS_FILE || 'metrics.json');
const htmlFile = path.join(rootDir, 'CleanedUp', 'benchmark_report.html');

async function run() {
    try {
        // Load main metrics (usually just C or recent run)
        let mainMetrics: any[] = [];
        try {
            const data = await fs.readFile(metricsFile, 'utf-8');
            mainMetrics = JSON.parse(data);
        } catch (e) {
            console.warn("Could not read main metrics.json", e);
        }

        // Aggregate All Metrics from CleanedUp
        const metricsPattern = path.join(rootDir, 'CleanedUp', 'Languages', '*', 'metrics.json');
        const metricFiles = await glob(metricsPattern);
        console.log(`Found ${metricFiles.length} additional metric files.`);

        let aggregatedMetrics: any[] = [];
        const knownSolvers = new Set();

        // 1. Load CleanedUp Metrics (Higher Priority)
        for (const file of metricFiles) {
            try {
                const content = await fs.readFile(file, 'utf-8');
                let parsedMetrics = JSON.parse(content);
                let metricsList: any[] = [];

                if (Array.isArray(parsedMetrics)) {
                    // Normalize flat array
                    const pathParts = file.split(path.sep);
                    const langIndex = pathParts.lastIndexOf('Languages');
                    let solverName = "Unknown";
                    if (langIndex !== -1 && pathParts[langIndex + 1]) {
                        solverName = pathParts[langIndex + 1];
                    }

                    const stats = await fs.stat(file);
                    metricsList.push({
                        solver: solverName,
                        timestamp: stats.mtimeMs, // Use file modification time
                        results: parsedMetrics
                    });
                } else {
                    metricsList = parsedMetrics;
                }

                for (const m of metricsList) {
                    if (!knownSolvers.has(m.solver)) {
                        aggregatedMetrics.push(m);
                        knownSolvers.add(m.solver);
                    }
                }
            } catch (e) {
                console.warn(`Error reading ${file}:`, e);
            }
        }

        // 2. Load Root Metrics (Lower Priority - Backfill)
        if (mainMetrics.length > 0) {
            console.log(`Backfilling with root metrics (${mainMetrics.length} found)...`);
            for (const m of mainMetrics) {
                if (!knownSolvers.has(m.solver)) {
                    console.log(`Restoring legacy metric for: ${m.solver}`);
                    aggregatedMetrics.push(m);
                    knownSolvers.add(m.solver);
                } else {
                    // console.log(`Skipping stale root metric for: ${m.solver}`);
                }
            }
        }

        // Filter out languages with no actual results
        const allMetrics = aggregatedMetrics.filter(m => m.results && m.results.length > 0);
        console.log(`Loaded ${allMetrics.length} total metrics (filtered from ${aggregatedMetrics.length}).`);

        // Read allowed matrices from TypeScript metrics
        const allowedMatrices = ['1.matrix', '2.matrix', '3.matrix', '4.matrix', '5.matrix', '6.matrix'];
        console.log(`Filtering report to standard suite: ${allowedMatrices.join(', ')}`);

        const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts, {}, allowedMatrices);
        await fs.writeFile(htmlFile, htmlContent);

        // Write timestamp file for smart refresh
        const timestampFile = path.join(rootDir, 'CleanedUp', 'timestamp.js');
        await fs.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()};`);

        console.log(`Successfully generated ${htmlFile}`);

        await captureScreenshot(htmlFile);
        console.log(`Screenshot captured for ${htmlFile}`);
    } catch (e) {
        console.error("Error:", e);
    }
}

run();
