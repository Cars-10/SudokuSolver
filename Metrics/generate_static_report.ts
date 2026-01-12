/**
 * Generate a static, shareable HTML report
 * This report has all data embedded and no server-dependent features
 * Run: npx ts-node generate_static_report.ts
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { glob } from 'glob';
import {
    generateHtml,
    personalities,
    languageMetadata,
    methodologyTexts,
    HistoryManager,
    orderedLanguages
} from './gather_metrics.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const outputDir = process.env.OUTPUT_DIR || rootDir;
const metricsFile = path.join(outputDir, process.env.METRICS_FILE || 'metrics.json');
const staticHtmlFile = path.join(rootDir, '_report_static.html');

async function run() {
    try {
        console.log('=== Generating Static Report ===');
        console.log('This report can be shared without a server.\n');

        // Load History
        const historyMgr = new HistoryManager(rootDir);
        const history = await historyMgr.getHistory();
        console.log(`Loaded ${history.length} historical records.`);

        // Load main metrics
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
        console.log(`Found ${metricFiles.length} metric files.`);

        let aggregatedMetrics: any[] = [];
        const knownSolvers = new Set();

        for (const file of metricFiles) {
            try {
                const content = await fs.promises.readFile(file, 'utf-8');
                let parsedMetrics = JSON.parse(content);
                let metricsList: any[] = [];

                if (Array.isArray(parsedMetrics)) {
                    if (parsedMetrics.length > 0 && parsedMetrics[0].results) {
                        metricsList = parsedMetrics;
                    } else {
                        const pathParts = file.split(path.sep);
                        const langIndex = pathParts.lastIndexOf('Languages');
                        let solverName = "Unknown";
                        if (langIndex !== -1 && pathParts[langIndex + 1]) {
                            solverName = pathParts[langIndex + 1];
                        }

                        const stats = await fs.promises.stat(file);

                        let runType = 'Local';
                        try {
                            const runTypePath = path.join(path.dirname(file), 'run_type');
                            const rt = await fs.promises.readFile(runTypePath, 'utf-8');
                            runType = rt.trim();
                        } catch (e) {
                            // ignore
                        }

                        metricsList.push({
                            solver: solverName,
                            timestamp: stats.mtimeMs,
                            runType: runType,
                            results: parsedMetrics
                        });
                    }
                } else {
                    metricsList = [parsedMetrics];
                }

                for (const m of metricsList) {
                    if (!knownSolvers.has(m.solver)) {
                        if (m.results && Array.isArray(m.results)) {
                            m.results = m.results.filter((r: any) => r.matrix !== 'N/A');
                        }
                        aggregatedMetrics.push(m);
                        knownSolvers.add(m.solver);
                    }
                }
            } catch (e) {
                console.warn(`Error reading ${file}:`, e);
            }
        }

        // Backfill from root metrics
        if (mainMetrics.length > 0) {
            for (const m of mainMetrics) {
                if (!knownSolvers.has(m.solver)) {
                    aggregatedMetrics.push(m);
                    knownSolvers.add(m.solver);
                }
            }
        }

        // Load metadata overrides
        let metadataOverrides = {};
        const metadataPath = path.join(__dirname, '../Languages/metadata.json');
        if (fs.existsSync(metadataPath)) {
            try {
                metadataOverrides = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
            } catch (e) {
                console.error("Failed to load metadata overrides: " + e);
            }
        }

        const allMetrics = aggregatedMetrics;
        console.log(`Loaded ${allMetrics.length} total metrics.`);

        const allowedMatrices = ['1.matrix', '2.matrix', '3.matrix', '4.matrix', '5.matrix', '6.matrix'];

        // Load benchmark config
        let benchmarkConfig: { languages?: Record<string, any>, lockedLanguages?: string[] } = {};
        try {
            const configPath = path.join(rootDir, 'benchmark_config.json');
            const configData = await fs.promises.readFile(configPath, 'utf-8');
            benchmarkConfig = JSON.parse(configData);
        } catch (e) {
            console.warn("Could not read benchmark_config.json");
        }

        // Load session state for locked languages
        try {
            const sessionStatePath = path.join(rootDir, 'session_state.json');
            const sessionData = await fs.promises.readFile(sessionStatePath, 'utf-8');
            const sessionState = JSON.parse(sessionData);
            const lockedLanguages = (sessionState.locked || [])
                .map((entry: any) => entry[0])
                .filter((lang: any) => lang !== null);
            benchmarkConfig.lockedLanguages = lockedLanguages;
        } catch (e) {
            benchmarkConfig.lockedLanguages = [];
        }

        // Build final metrics list with ordered languages
        const metricsMap = new Map(allMetrics.map(m => [m.solver, m]));
        const finalMetrics: any[] = [];

        for (const lang of orderedLanguages) {
            if (metricsMap.has(lang)) {
                finalMetrics.push(metricsMap.get(lang));
            } else {
                finalMetrics.push({
                    solver: lang,
                    runType: 'Local',
                    timestamp: Date.now(),
                    results: []
                });
            }
        }

        // Add discovered languages not in master list
        for (const m of allMetrics) {
            if (!orderedLanguages.includes(m.solver)) {
                finalMetrics.push(m);
            }
        }

        console.log(`Static report will contain ${finalMetrics.length} languages.`);

        // Generate HTML with static mode enabled
        const htmlContent = await generateHtml(
            finalMetrics,
            history,
            personalities,
            languageMetadata,
            methodologyTexts,
            {},
            allowedMatrices,
            benchmarkConfig,
            metadataOverrides,
            { staticMode: true }  // Enable static mode
        );

        // Add a banner indicating this is a static report
        const staticBanner = `
        <div style="background: linear-gradient(90deg, #1a1b26, #24283b); padding: 10px 20px; text-align: center; border-bottom: 1px solid #414868; font-size: 0.9em; color: #787c99;">
            📊 Static Report - Generated ${new Date().toLocaleString('en-GB', { timeZone: 'Europe/Berlin' })} CET | <a href="https://github.com/Cars-10/SudokuSolver" style="color: #7aa2f7;">View Project</a>
        </div>`;

        // Insert banner after opening body tag
        const finalHtml = htmlContent.replace('<body>', '<body>' + staticBanner);

        await fs.promises.writeFile(staticHtmlFile, finalHtml);
        console.log(`\n✓ Static report generated: ${staticHtmlFile}`);
        console.log(`  File size: ${(fs.statSync(staticHtmlFile).size / 1024).toFixed(1)} KB`);
        console.log(`\nYou can share this file directly - no server required!`);

    } catch (error) {
        console.error('Error generating static report:', error);
        process.exit(1);
    }
}

run();
