import * as fs from 'fs';
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
    narratorIntros,
    HistoryManager,
    generateHistoryHtml,
    orderedLanguages
} from './gather_metrics.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const outputDir = process.env.OUTPUT_DIR || rootDir;
const metricsFile = path.join(outputDir, process.env.METRICS_FILE || 'metrics.json');
const htmlFile = path.join(rootDir, 'CleanedUp', 'benchmark_report.html');

async function run() {
    try {
        // Load History
        const historyMgr = new HistoryManager(rootDir);
        const history = await historyMgr.getHistory();
        console.log(`Loaded ${history.length} historical records.`);

        // Load main metrics (usually just C or recent run)
        let mainMetrics: any[] = [];
        try {
            const data = await fs.promises.readFile(metricsFile, 'utf-8');
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
                const content = await fs.promises.readFile(file, 'utf-8');
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

                    const stats = await fs.promises.stat(file);

                    // Check for run_type file
                    let runType = 'Local';
                    try {
                        const runTypePath = path.join(path.dirname(file), 'run_type');
                        const rt = await fs.promises.readFile(runTypePath, 'utf-8');
                        runType = rt.trim();
                    } catch (e) {
                        // ignore, default to Local
                    }

                    metricsList.push({
                        solver: solverName,
                        timestamp: stats.mtimeMs, // Use file modification time
                        runType: runType,
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

        // Check for metadata overrides
        let metadataOverrides = {};
        const metadataPath = path.join(__dirname, '../CleanedUp/Languages/metadata.json');
        // fs.existsSync is synchronous, but this is fine for a startup config read
        if (fs.existsSync(metadataPath)) {
            console.log("Loading metadata overrides from " + metadataPath);
            try {
                // fs.readFileSync is synchronous, but this is fine for a startup config read
                metadataOverrides = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
            } catch (e) {
                console.error("Failed to load metadata overrides: " + e);
            }
        }

        const allMetrics = aggregatedMetrics;
        console.log(`Loaded ${allMetrics.length} total metrics.`);

        // Read allowed matrices from TypeScript metrics
        const allowedMatrices = ['1.matrix', '2.matrix', '3.matrix', '4.matrix', '5.matrix', '6.matrix'];
        console.log(`Filtering report to standard suite: ${allowedMatrices.join(', ')}`);

        // Load benchmark config for expected matrices per language
        let benchmarkConfig: { languages?: Record<string, any> } = {};
        try {
            const configPath = path.join(rootDir, 'CleanedUp', 'benchmark_config.json');
            const configData = await fs.promises.readFile(configPath, 'utf-8');
            benchmarkConfig = JSON.parse(configData);
            console.log(`Loaded benchmark config with ${Object.keys(benchmarkConfig.languages || {}).length} language configurations.`);
        } catch (e) {
            console.warn("Could not read benchmark_config.json, using defaults");
        }



        // Merge with Ordered Master List
        const metricsMap = new Map(allMetrics.map(m => [m.solver, m]));
        const finalMetrics: any[] = [];

        for (const lang of orderedLanguages) {
            if (metricsMap.has(lang)) {
                finalMetrics.push(metricsMap.get(lang));
            } else {
                // Create Placeholder for Missing Language
                finalMetrics.push({
                    solver: lang,
                    runType: 'Local',
                    timestamp: Date.now(),
                    results: [] // No results -> HTMLGenerator will render as Init/Empty
                });
            }
        }

        // Add any discovered languages NOT in the master list at the end
        for (const m of allMetrics) {
            if (!orderedLanguages.includes(m.solver)) {
                finalMetrics.push(m);
            }
        }

        console.log(`Final Report contains ${finalMetrics.length} languages.`);

        const htmlContent = await generateHtml(finalMetrics, history, personalities, languageMetadata, methodologyTexts, {}, allowedMatrices, benchmarkConfig, metadataOverrides);
        await fs.promises.writeFile(htmlFile, htmlContent);

        // Generate History Report
        const historyHtml = await generateHistoryHtml(history);
        const historyHtmlFile = path.join(rootDir, 'CleanedUp', 'benchmark_history.html');
        await fs.promises.writeFile(historyHtmlFile, historyHtml);
        console.log(`Successfully generated ${historyHtmlFile}`);

        // Write timestamp file for smart refresh
        const timestampFile = path.join(rootDir, 'CleanedUp', 'timestamp.js');
        await fs.promises.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()};`);

        console.log(`Successfully generated ${htmlFile}`);

        const now = new Date();
        const formatter = new Intl.DateTimeFormat('en-SE', { // en-SE uses ISO 8601 format
            timeZone: 'CET',
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
            hour: '2-digit',
            minute: '2-digit',
            second: '2-digit',
            hour12: false
        });
        const parts = formatter.formatToParts(now);
        const getPart = (type: string) => parts.find(p => p.type === type)?.value;
        const timestamp = `${getPart('year')}-${getPart('month')}-${getPart('day')}_${getPart('hour')}-${getPart('minute')}-${getPart('second')}_CET`;

        const screenshotsDir = path.join(rootDir, 'screenshots');
        if (!fs.existsSync(screenshotsDir)) {
            await fs.promises.mkdir(screenshotsDir, { recursive: true });
        }
        const screenshotPath = path.join(screenshotsDir, `benchmark_report_${timestamp}.png`);
        await captureScreenshot(htmlFile, screenshotPath);
        console.log(`Screenshot captured for ${htmlFile} at ${screenshotPath}`);
    } catch (e) {
        console.error("Error:", e);
    }
}

run();
