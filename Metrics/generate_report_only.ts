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
const htmlFile = path.join(rootDir, '_report.html');

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

        // Aggregate All Metrics from Languages
        const metricsPattern = path.join(rootDir, 'Languages', '*', 'metrics.json');
        const metricFiles = await glob(metricsPattern);
        console.log(`Found ${metricFiles.length} additional metric files.`);

        let aggregatedMetrics: any[] = [];
        const knownSolvers = new Set();

        // 1. Load Metrics (Higher Priority)
        for (const file of metricFiles) {
            try {
                const content = await fs.promises.readFile(file, 'utf-8');
                let parsedMetrics = JSON.parse(content);
                let metricsList: any[] = [];

                if (Array.isArray(parsedMetrics)) {
                    // Check if array already contains wrapped metrics (new common.sh format)
                    // New format: [{solver: "C++", results: [...], ...}]
                    // Old format: [{matrix: "1", time: 0.01, ...}, ...]
                    if (parsedMetrics.length > 0 && parsedMetrics[0].results) {
                        // Already wrapped format - use directly
                        metricsList = parsedMetrics;
                    } else {
                        // Flat array of results - need to wrap
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
                    }
                } else {
                    metricsList = [parsedMetrics];
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
        const metadataPath = path.join(__dirname, '../Languages/metadata.json');
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
        let benchmarkConfig: { languages?: Record<string, any>, lockedLanguages?: string[] } = {};
        try {
            const configPath = path.join(rootDir, 'benchmark_config.json');
            const configData = await fs.promises.readFile(configPath, 'utf-8');
            benchmarkConfig = JSON.parse(configData);
            console.log(`Loaded benchmark config with ${Object.keys(benchmarkConfig.languages || {}).length} language configurations.`);
        } catch (e) {
            console.warn("Could not read benchmark_config.json, using defaults");
        }

        // Load session state to get locked languages
        try {
            const sessionStatePath = path.join(rootDir, 'session_state.json');
            const sessionData = await fs.promises.readFile(sessionStatePath, 'utf-8');
            const sessionState = JSON.parse(sessionData);
            // Extract language names from locked array (format: [[lang, timestamp], ...])
            const lockedLanguages = (sessionState.locked || [])
                .map((entry: any) => entry[0])
                .filter((lang: any) => lang !== null);
            benchmarkConfig.lockedLanguages = lockedLanguages;
            console.log(`Loaded ${lockedLanguages.length} locked languages: ${lockedLanguages.join(', ')}`);
        } catch (e) {
            console.warn("Could not read session_state.json, no languages will be locked");
            benchmarkConfig.lockedLanguages = [];
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

        // Generate screenshot-specific report with only selected languages (those with results)
        const selectedLangs = benchmarkConfig.lockedLanguages || [];
        const screenshotMetrics = finalMetrics.filter(m =>
            selectedLangs.includes(m.solver) && m.results && m.results.length > 0
        );

        const screenshotHtmlFile = path.join(rootDir, '_report_screenshot.html');
        if (screenshotMetrics.length > 0) {
            console.log(`Generating screenshot report with ${screenshotMetrics.length} languages: ${screenshotMetrics.map(m => m.solver).join(', ')}`);
            const screenshotHtmlContent = await generateHtml(screenshotMetrics, history, personalities, languageMetadata, methodologyTexts, {}, allowedMatrices, benchmarkConfig, metadataOverrides);
            await fs.promises.writeFile(screenshotHtmlFile, screenshotHtmlContent);
        }

        // Generate History Report
        const historyHtml = await generateHistoryHtml(history);
        const historyHtmlFile = path.join(rootDir, 'benchmark_history.html');
        await fs.promises.writeFile(historyHtmlFile, historyHtml);
        console.log(`Successfully generated ${historyHtmlFile}`);

        // Write timestamp file for smart refresh
        const timestampFile = path.join(rootDir, 'timestamp.js');
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
        // Use the screenshot-specific HTML if it was generated, otherwise fall back to main report
        const fileToCapture = fs.existsSync(screenshotHtmlFile) ? screenshotHtmlFile : htmlFile;
        await captureScreenshot(fileToCapture, screenshotPath);
        console.log(`Screenshot captured for ${fileToCapture} at ${screenshotPath}`);
    } catch (e) {
        console.error("Error:", e);
    }
}

run();
