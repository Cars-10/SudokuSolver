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
import { calculateOverallScore } from './scoring.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const outputDir = process.env.OUTPUT_DIR || rootDir;
const metricsFile = path.join(outputDir, process.env.METRICS_FILE || 'metrics.json');
const htmlFile = path.join(rootDir, 'index.html');

async function run() {
    try {
        // Empty history array (history feature removed)
        const history: any[] = [];

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
                            // Legacy structure: Algorithms/BruteForce/C/metrics.json
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

        // Process Benchmark Issues (for Recent Failures)
        const issuesPath = path.join(rootDir, 'benchmark_issues.json');
        if (fs.existsSync(issuesPath)) {
            try {
                const issuesContent = await fs.promises.readFile(issuesPath, 'utf-8');
                const issues = JSON.parse(issuesContent);

                // Group issues by solver/algorithm
                const latestIssues = new Map<string, any>();
                for (const issue of issues) {
                    const key = `${issue.solver}::${issue.algorithm || 'BruteForce'}`;
                    const issueTs = new Date(issue.timestamp).getTime();

                    if (!latestIssues.has(key) || issueTs > latestIssues.get(key).timestamp) {
                        latestIssues.set(key, { ...issue, timestamp: issueTs });
                    }
                }

                // Check against successes
                for (const [key, issue] of latestIssues) {
                    const foundMetric = aggregatedMetrics.find(m => `${m.solver}::${m.algorithmType || 'BruteForce'}` === key);

                    if (foundMetric) {
                        // If issue is newer than success, mark as failed
                        const metricTs = new Date(foundMetric.timestamp).getTime();
                        if (issue.timestamp > metricTs) {
                            foundMetric.failed = true;
                            foundMetric.failureReason = `${issue.status}: ${issue.output ? issue.output.substring(0, 100) : 'Unknown error'}`;
                            console.log(`Marking ${key} as FAILED due to recent issue (Success: ${metricTs}, Issue: ${issue.timestamp})`);
                        }
                    } else {
                        // Metric doesn't exist? It might be a new language that failed completely.
                        // We should add it to aggregatedMetrics so it shows up in the report as failed.
                        aggregatedMetrics.push({
                            solver: issue.solver,
                            algorithmType: issue.algorithm || 'BruteForce',
                            runType: issue.runType || 'automated',
                            timestamp: issue.timestamp,
                            failed: true,
                            failureReason: `${issue.status}: ${issue.output ? issue.output.substring(0, 100) : 'Unknown error'}`,
                            results: []
                        });
                        knownSolverAlgoPairs.add(key);
                        console.log(`Adding completely failed solver ${key} to report.`);
                    }
                }
            } catch (e) {
                console.warn("Error processing benchmark_issues.json:", e);
            }
        }
        // Check for metadata overrides
        let metadataOverrides = {};
        const metadataPath = path.join(__dirname, '../Algorithms/metadata.json');
        if (fs.existsSync(metadataPath)) {
            console.log("Loading metadata overrides from " + metadataPath);
            try {
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
        let benchmarkConfig: { languages?: Record<string, any>, lockedLanguages?: string[], scoring_weights?: any } = {};
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
        // Use solver::algorithmType as key to preserve multiple algorithm variants per language
        const metricsMap = new Map(allMetrics.map(m => [`${m.solver}::${m.algorithmType || 'BruteForce'}`, m]));
        const finalMetrics: any[] = [];

        // For each language in ordered list, add BruteForce variant first
        for (const lang of orderedLanguages) {
            const bruteForceKey = `${lang}::BruteForce`;
            if (metricsMap.has(bruteForceKey)) {
                finalMetrics.push(metricsMap.get(bruteForceKey));
            } else {
                // Create Placeholder for Missing Language (BruteForce only)
                finalMetrics.push({
                    solver: lang,
                    algorithmType: 'BruteForce',
                    runType: 'Local',
                    timestamp: Date.now(),
                    results: [] // No results -> HTMLGenerator will render as Init/Empty
                });
            }
        }

        // Add any discovered languages NOT in the master list, or non-BruteForce variants
        for (const m of allMetrics) {
            const isInOrderedList = orderedLanguages.includes(m.solver);
            const isBruteForce = (m.algorithmType || 'BruteForce') === 'BruteForce';

            // Add if: not in ordered list, OR it's an algorithm variant (DLX/CP)
            if (!isInOrderedList || !isBruteForce) {
                finalMetrics.push(m);
            }
        }

        // --- SCORING CALCULATION START ---
        // 1. Get Weights from Config
        const weights = (benchmarkConfig as any).scoring_weights || { time: 0.8, memory: 0.2 };
        console.log(`Applying scoring weights: Time=${weights.time}, Memory=${weights.memory}`);

        // 2. Index C Baselines by Algorithm Type
        const cBaselines = new Map<string, any>();
        for (const m of finalMetrics) {
            if (m.solver === 'C') {
                const algo = m.algorithmType || 'BruteForce';
                cBaselines.set(algo, m);
            }
        }

        // 3. Calculate Scores
        for (const m of finalMetrics) {
            const algo = m.algorithmType || 'BruteForce';
            const cMetric = cBaselines.get(algo);

            // If we have a baseline (and it's not the baseline itself, though comparing C to C gives 1.0 which is correct)
            if (cMetric) {
                // Pass the results array (MetricsResult[])
                m.score = calculateOverallScore(m.results, cMetric.results, weights);
            } else {
                // No baseline found for this algorithm?
                // Fallback to BruteForce baseline if available?
                const fallbackC = cBaselines.get('BruteForce');
                if (fallbackC) {
                    m.score = calculateOverallScore(m.results, fallbackC.results, weights);
                } else {
                    m.score = 0; // Should not happen if C is present
                }
            }
        }
        // --- SCORING CALCULATION END ---

        console.log(`Final Report contains ${finalMetrics.length} languages.`);

        const result = await generateHtml(finalMetrics, history, personalities, languageMetadata, methodologyTexts, {}, allowedMatrices, benchmarkConfig, metadataOverrides);

        let htmlContent = "";
        if (typeof result === 'string') {
            htmlContent = result;
        } else if (result && typeof result === 'object' && result.html) {
            // Handle object return if it happens (forward compatibility)
            htmlContent = result.html;
            if (result.assets) {
                const assetsDir = path.join(rootDir, 'assets');
                if (!fs.existsSync(assetsDir)) {
                    await fs.promises.mkdir(assetsDir, { recursive: true });
                }
                for (const [assetPath, content] of Object.entries(result.assets)) {
                    const fullPath = path.join(rootDir, assetPath);
                    const dir = path.dirname(fullPath);
                    if (!fs.existsSync(dir)) {
                        await fs.promises.mkdir(dir, { recursive: true });
                    }
                    await fs.promises.writeFile(fullPath, content as string);
                    console.log(`Generated asset: ${assetPath}`);
                }
            }
        }

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