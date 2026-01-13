import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { glob } from 'glob';
import {
    generateHtml,
    personalities,
    languageMetadata,
    methodologyTexts,
    mismatchLabels,
    narratorIntros,
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
        // Empty history array (history feature removed)
        const history: any[] = [];

        // Load main metrics (usually just C or recent run)
        let mainMetrics: any[] = [];
        try {
            const data = await fs.promises.readFile(metricsFile, 'utf-8');
            mainMetrics = JSON.parse(data);
        } catch (e) {
            console.warn("Could not read main metrics.json", e);
        }

        // Aggregate All Metrics from Languages (legacy) and Algorithms (new structure)
        const metricsPatterns = [
            path.join(rootDir, 'Languages', '*', 'metrics.json'),
            path.join(rootDir, 'Algorithms', 'BruteForce', '*', 'metrics.json'),
            path.join(rootDir, 'Algorithms', 'DLX', '*', 'metrics.json'),
            path.join(rootDir, 'Algorithms', 'CP', '*', 'metrics.json')
        ];

        let metricFiles: string[] = [];
        for (const pattern of metricsPatterns) {
            const files = await glob(pattern);
            metricFiles.push(...files);
        }
        console.log(`Found ${metricFiles.length} metric files across all algorithm types.`);

        let aggregatedMetrics: any[] = [];
        const knownSolverAlgoPairs = new Set(); // Track language+algorithm combinations

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
                        // Already wrapped format - use directly, but add algorithmType
                        const pathParts = file.split(path.sep);
                        const algoIndex = pathParts.lastIndexOf('Algorithms');
                        let algorithmType: 'BruteForce' | 'DLX' | 'CP' | undefined = undefined;

                        if (algoIndex !== -1 && pathParts[algoIndex + 1]) {
                            algorithmType = pathParts[algoIndex + 1] as 'BruteForce' | 'DLX' | 'CP';
                        } else {
                            algorithmType = 'BruteForce'; // Legacy defaults to BruteForce
                        }

                        metricsList = parsedMetrics.map((m: any) => ({
                            ...m,
                            algorithmType: algorithmType
                        }));
                    } else {
                        // Flat array of results - need to wrap
                        const pathParts = file.split(path.sep);

                        // Try new Algorithms structure first, then fallback to legacy Languages
                        const algoIndex = pathParts.lastIndexOf('Algorithms');
                        const langIndex = pathParts.lastIndexOf('Languages');
                        let solverName = "Unknown";
                        let algorithmType: 'BruteForce' | 'DLX' | 'CP' | undefined = undefined;

                        if (algoIndex !== -1 && pathParts[algoIndex + 1] && pathParts[algoIndex + 2]) {
                            // New structure: Algorithms/BruteForce/C/metrics.json
                            algorithmType = pathParts[algoIndex + 1] as 'BruteForce' | 'DLX' | 'CP';
                            solverName = pathParts[algoIndex + 2];
                        } else if (langIndex !== -1 && pathParts[langIndex + 1]) {
                            // Legacy structure: Languages/C/metrics.json
                            solverName = pathParts[langIndex + 1];
                            algorithmType = 'BruteForce'; // Legacy defaults to BruteForce
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
                            algorithmType: algorithmType,
                            timestamp: stats.mtimeMs, // Use file modification time
                            runType: runType,
                            results: parsedMetrics
                        });
                    }
                } else {
                    metricsList = [parsedMetrics];
                }

                for (const m of metricsList) {
                    const pairKey = `${m.solver}::${m.algorithmType || 'BruteForce'}`;
                    if (!knownSolverAlgoPairs.has(pairKey)) {
                        // Filter out N/A matrix entries - those are toolchain checks, not actual runs
                        if (m.results && Array.isArray(m.results)) {
                            m.results = m.results.filter((r: any) => r.matrix !== 'N/A');
                        }
                        aggregatedMetrics.push(m);
                        knownSolverAlgoPairs.add(pairKey);
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
                const pairKey = `${m.solver}::${m.algorithmType || 'BruteForce'}`;
                if (!knownSolverAlgoPairs.has(pairKey)) {
                    console.log(`Restoring legacy metric for: ${m.solver}`);
                    aggregatedMetrics.push(m);
                    knownSolverAlgoPairs.add(pairKey);
                } else {
                    // console.log(`Skipping stale root metric for: ${m.solver}`);
                }
            }
        }

        // Check for metadata overrides
        let metadataOverrides = {};
        const metadataPath = path.join(__dirname, '../Algorithms/metadata.json');
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

        // Write timestamp file for smart refresh
        const timestampFile = path.join(rootDir, 'timestamp.js');
        await fs.promises.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()};`);

        console.log(`Successfully generated ${htmlFile}`);
    } catch (e) {
        console.error("Error:", e);
    }
}

run();
