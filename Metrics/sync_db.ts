import { glob } from 'glob';
import * as fs from 'fs/promises';
import { existsSync } from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { insertRuns, DB_PATH } from './db_utils.js';
import { initDatabase } from './init_database.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');

async function syncDatabase() {
    console.log('🔄 Starting Database Sync...');

    // 1. Reset Database (Delete and Re-init)
    // We want a fresh start to match the current state of JSON files exactly.
    try {
        if (existsSync(DB_PATH)) {
            await fs.unlink(DB_PATH);
            console.log('🗑️  Deleted existing database.');
        }
    } catch (e) {
        // Ignore if file doesn't exist
    }

    // Re-initialize (creates tables with new schema)
    initDatabase();

    // 2. Find all metrics.json files
    const metricsPattern = path.join(rootDir, 'Languages', '*', 'metrics.json');
    const metricFiles = await glob(metricsPattern);
    console.log(`📂 Found ${metricFiles.length} metrics.json files.`);

    let totalRuns = 0;

    // 3. Process each file
    for (const file of metricFiles) {
        try {
            const content = await fs.readFile(file, 'utf-8');
            let metrics = [];

            try {
                metrics = JSON.parse(content);
            } catch (parseError) {
                console.warn(`⚠️  Invalid JSON in ${file}: ${parseError.message}`);
                continue;
            }

            if (!Array.isArray(metrics)) {
                // Handle case where it might be a single object
                if (typeof metrics === 'object' && metrics !== null) {
                    metrics = [metrics];
                } else {
                    console.warn(`⚠️  Skipping ${file}: Content is not an array or object.`);
                    continue;
                }
            }

            // Normalizing data if needed (some legacy files might have different structure)
            const normalizedMetrics = metrics.map(m => {
                // Ensure runType is set if missing (default to Local)
                if (!m.runType) m.runType = 'Local';

                // Ensure results is an array (legacy flat format?)
                // Actually our insertRuns expects flat "Run" objects or objects with "results"?
                // Wait, db_utils.ts insertRun expects a SINGLE run object with { matrix, time, ... }
                // BUT metrics.json structure is:
                // [ { solver: "C", results: [ { matrix: "1", ... }, ... ] } ]

                // We need to flatten this structure for the DB insertion!
                // DB expects: timestamp, language, matrix, ...

                return m;
            });

            // FLATTEN LOGIC
            // The JSON is: Array of SolverMetrics.
            // SolverMetrics = { solver, runType, timestamp, results: [MetricResult] }
            // Run (DB) = { timestamp, language, matrix, runType, iterations, time_ms, ... }

            const flatRuns = [];
            for (const solverMetric of metrics) {
                if (!solverMetric.results || !Array.isArray(solverMetric.results)) continue;

                for (const res of solverMetric.results) {
                    flatRuns.push({
                        timestamp: solverMetric.timestamp,
                        language: solverMetric.solver, // solver name mapped to language
                        runType: solverMetric.runType || 'Local',
                        matrix: parseInt(res.matrix) || 0, // Ensure integer

                        // Metrics
                        iterations: res.iterations,
                        time_ms: res.time, // JSON has 'time' (which is now ms)
                        memory_bytes: res.memory, // JSON has 'memory' (bytes)

                        cpu_user: res.cpu_user,
                        cpu_sys: res.cpu_sys,

                        // OS Metrics
                        page_faults_major: res.page_faults_major,
                        page_faults_minor: res.page_faults_minor,
                        context_switches_voluntary: res.context_switches_voluntary,
                        context_switches_involuntary: res.context_switches_involuntary,
                        io_inputs: res.io_inputs,
                        io_outputs: res.io_outputs,

                        status: res.status,
                        output: res.output,
                        compiler_variant: 'default', // TODO: Add to JSON if available
                        toolchain_version: null
                    });
                }
            }

            if (flatRuns.length > 0) {
                const inserted = insertRuns(flatRuns);
                totalRuns += inserted;
            }

        } catch (e) {
            console.error(`❌ Error processing ${file}:`, e);
        }
    }

    console.log(`\n✅ Database Sync Complete!`);
    console.log(`📊 Total Runs Inserted: ${totalRuns}`);
    // console.log(`💾 Database Size: ${(await fs.stat(DB_PATH)).size / 1024} KB`);
}

syncDatabase().catch(console.error);
