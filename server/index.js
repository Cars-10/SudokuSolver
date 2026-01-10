const express = require('express');
const cors = require('cors');
const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');
const historyDb = require('./database');

const app = express();
const port = process.env.PORT || 9001;

app.use(cors());
app.use(express.json());

// Serve root directory for static assets (timestamp.js, images, etc.)
app.use(express.static(path.join(__dirname, '..')));

// Serve server directory for static assets
app.use(express.static(path.join(__dirname, '.')));

// Serve the Benchmark Report at Root
app.get('/', (req, res) => {
    const reportPath = path.join(__dirname, '../_report.html');
    if (fs.existsSync(reportPath)) {
        res.sendFile(reportPath);
    } else {
        res.send(`
            <html>
                <body style="font-family: sans-serif; display: flex; justify-content: center; align-items: center; height: 100vh; background: #0f172a; color: #fff;">
                    <div style="text-align: center;">
                        <h1>Report not found</h1>
                        <p>No benchmark report has been generated yet.</p>
                        <button onclick="generateReport()" style="padding: 10px 20px; cursor: pointer; background: #38bdf8; border: none; border-radius: 4px; font-weight: bold;">Generate Report</button>
                        <br><br>
                        <a href="/runner" style="color: #94a3b8;">Go to Matrix Runner</a>
                    </div>
                    <script>
                        async function generateReport() {
                            const btn = document.querySelector('button');
                            btn.disabled = true;
                            btn.textContent = 'Generating...';
                            try {
                                const res = await fetch('/api/generate-report', { method: 'POST' });
                                if (res.ok) window.location.reload();
                                else alert('Failed to generate report');
                            } catch (e) {
                                alert('Error: ' + e);
                            } finally {
                                btn.disabled = false;
                                btn.textContent = 'Generate Report';
                            }
                        }
                    </script>
                </body>
            </html>
        `);
    }
});

// Serve the Matrix Runner at /runner
app.use('/runner', express.static('public'));

// Serve Metrics directory for JS/CSS assets
app.use('/js', express.static(path.join(__dirname, '../Metrics')));
app.use('/css', express.static(path.join(__dirname, '../Metrics')));
app.use('/Metrics', express.static(path.join(__dirname, '../Metrics')));

// Serve logos directory
app.use('/logos', express.static(path.join(__dirname, '../logos')));

const LANGUAGES_DIR = path.join(__dirname, '../Languages');
const MATRICES_DIR = path.join(__dirname, '../Matrices');

// Get list of matrices
app.get('/api/matrices', (req, res) => {
    try {
        const files = fs.readdirSync(MATRICES_DIR).filter(file => file.endsWith('.matrix'));
        res.json(files);
    } catch (error) {
        console.error('Error reading matrices:', error);
        res.status(500).json({ error: 'Failed to list matrices' });
    }
});

// Get matrix content
app.get('/api/matrix/:filename', (req, res) => {
    try {
        const filePath = path.join(MATRICES_DIR, req.params.filename);
        if (!fs.existsSync(filePath)) {
            return res.status(404).json({ error: 'Matrix not found' });
        }
        const content = fs.readFileSync(filePath, 'utf8');
        res.json({ content });
    } catch (error) {
        console.error('Error reading matrix file:', error);
        res.status(500).json({ error: 'Failed to read matrix' });
    }
});

// Get list of languages
app.get('/api/languages', (req, res) => {
    try {
        const languages = fs.readdirSync(LANGUAGES_DIR)
            .filter(file => fs.statSync(path.join(LANGUAGES_DIR, file)).isDirectory());
        res.json(languages);
    } catch (error) {
        console.error('Error reading languages:', error);
        res.status(500).json({ error: 'Failed to list languages' });
    }
});

// Run benchmark
app.post('/api/run', (req, res) => {
    const { language, matrix } = req.body;

    if (!language) {
        return res.status(400).json({ error: 'Language is required' });
    }

    const langDir = path.join(LANGUAGES_DIR, language);
    // Only construct matrix path if matrix is provided
    const matrixPath = matrix ? path.join(MATRICES_DIR, matrix) : null;

    // Relative path to matrix from the language directory (where script runs)
    // server/../Languages/Lang -> server/../Matrices/1.matrix
    // We need to match how runBenchmarks.sh passes arguments
    // runBenchmarks.sh passes: ../../../Matrices/${m}.matrix
    // The CWD when running setupAndRunMe.sh is the language directory.
    // So `../../../Matrices` from `Languages/Lang` refers to `Matrices` at root.

    const metricsFile = path.join(langDir, 'metrics.json');
    let previousMetrics = [];

    // 1. Read existing metrics
    if (fs.existsSync(metricsFile)) {
        try {
            previousMetrics = JSON.parse(fs.readFileSync(metricsFile, 'utf8'));
        } catch (e) {
            console.warn(`Could not read existing metrics for ${language}:`, e.message);
        }
    }

    console.log(`Running ${language} with ${matrix || 'ALL matrices'}...`);

    // If matrix is missing, run all (default script behavior)
    // Try runMe.sh first (new pattern), fall back to setupAndRunMe.sh (legacy)
    const runMeScript = fs.existsSync(path.join(langDir, 'runMe.sh')) ? './runMe.sh' : './setupAndRunMe.sh';
    let command = runMeScript;
    if (matrix) {
        const matrixArg = `../../Matrices/${matrix}`;
        command += ` ${matrixArg}`;
    }

    exec(command, { cwd: langDir }, (error, stdout, stderr) => {
        if (error) {
            console.error(`Exec error: ${error}`);
        }

        const runTimestamp = new Date().toISOString();

        // 2. Read new metrics (generated by the script)
        let newMetrics = [];
        if (fs.existsSync(metricsFile)) {
            try {
                const content = fs.readFileSync(metricsFile, 'utf8');
                newMetrics = JSON.parse(content);
            } catch (e) {
                console.error("Error reading new metrics:", e);
            }
        }

        // 3. Merge Phase - properly handle the run/results structure
        // metrics.json structure: [{solver, runType, timestamp, results: [{matrix, time, ...}]}]
        let finalMetrics = newMetrics;

        if (matrix && Array.isArray(previousMetrics) && previousMetrics.length > 0) {
            // Single matrix run - merge into existing run
            // Get the latest run from previous metrics
            const latestRun = previousMetrics[previousMetrics.length - 1];

            // Get the new matrix result from the script output
            let newMatrixResult = null;
            if (Array.isArray(newMetrics) && newMetrics.length > 0) {
                const newRun = newMetrics[newMetrics.length - 1];
                if (newRun && Array.isArray(newRun.results)) {
                    newMatrixResult = newRun.results.find(r => r.matrix);
                }
            }

            if (latestRun && latestRun.results && newMatrixResult) {
                // Add run_timestamp to the new matrix result (US-009)
                newMatrixResult.run_timestamp = runTimestamp;

                // Create a map of existing results by matrix number
                const resultsMap = new Map();
                latestRun.results.forEach(r => {
                    resultsMap.set(String(r.matrix).replace('.matrix', ''), r);
                });

                // Update/Add the new matrix result
                const matrixKey = String(newMatrixResult.matrix).replace('.matrix', '');
                resultsMap.set(matrixKey, newMatrixResult);

                // Convert back to array, sorted by matrix number
                const mergedResults = Array.from(resultsMap.values())
                    .sort((a, b) => parseInt(a.matrix) - parseInt(b.matrix));

                // Update the run with merged results and new timestamp (US-008)
                latestRun.results = mergedResults;
                latestRun.timestamp = runTimestamp;
                latestRun.runType = 'Local';

                // Keep only the latest run (or could keep history)
                finalMetrics = [latestRun];

                // Write merged back
                try {
                    fs.writeFileSync(metricsFile, JSON.stringify(finalMetrics, null, 2));
                    console.log(`Merged matrix ${matrix} into ${language} metrics`);
                } catch (e) {
                    console.error("Error writing merged metrics:", e);
                }
            }
        } else if (!matrix && Array.isArray(newMetrics) && newMetrics.length > 0) {
            // Full run - add run_timestamp to all results (US-009)
            const latestRun = newMetrics[newMetrics.length - 1];
            if (latestRun && Array.isArray(latestRun.results)) {
                latestRun.results.forEach(r => {
                    r.run_timestamp = runTimestamp;
                });
                latestRun.timestamp = runTimestamp;
                latestRun.runType = 'Local';

                // Write back with timestamps
                try {
                    fs.writeFileSync(metricsFile, JSON.stringify(newMetrics, null, 2));
                } catch (e) {
                    console.error("Error writing metrics:", e);
                }
            }
        }

        // Get final results for history
        let historyResults = [];
        if (Array.isArray(finalMetrics) && finalMetrics.length > 0) {
            const latestRun = finalMetrics[finalMetrics.length - 1];
            historyResults = latestRun.results || [];
        }

        // Capture History (SQLite)
        try {
            historyDb.insertBenchmarkRun({
                solver: language,
                runType: 'Local',
                timestamp: runTimestamp,
                results: historyResults
            });
            console.log(`Recorded history for ${language} in SQLite`);
        } catch (hErr) {
            console.error("Error writing history to SQLite:", hErr);
        }

        res.json({
            success: !error,
            stdout,
            stderr,
            exitCode: error ? error.code : 0,
            timestamp: runTimestamp  // Return timestamp for client-side update
        });
    });
});

const METADATA_FILE = path.join(__dirname, '../Languages/metadata.json');

// 2.5 Get Metadata for specific language
app.get('/api/metadata/:lang', (req, res) => {
    const lang = req.params.lang;
    try {
        let currentData = {};
        if (fs.existsSync(METADATA_FILE)) {
            currentData = JSON.parse(fs.readFileSync(METADATA_FILE, 'utf8'));
        }
        const meta = (currentData.languageMetadata && currentData.languageMetadata[lang]) || {};
        res.json(meta);
    } catch (e) {
        console.error("Error fetching metadata:", e);
        res.status(500).json({ error: e.message });
    }
});

// Save Metadata
app.post('/api/save-metadata', (req, res) => {
    const { lang, metadata } = req.body;
    if (!lang || !metadata) {
        return res.status(400).json({ error: 'Language and metadata required' });
    }

    try {
        let currentData = {};
        if (fs.existsSync(METADATA_FILE)) {
            currentData = JSON.parse(fs.readFileSync(METADATA_FILE, 'utf8'));
        }

        // Ensure structure
        if (!currentData.languageMetadata) currentData.languageMetadata = {};

        // Update
        const existing = currentData.languageMetadata[lang] || {};
        currentData.languageMetadata[lang] = { ...existing, ...metadata, lastUpdated: new Date().toISOString() };

        fs.writeFileSync(METADATA_FILE, JSON.stringify(currentData, null, 4));
        console.log(`Updated metadata for ${lang}`);
        res.json({ success: true });
    } catch (e) {
        console.error("Error saving metadata:", e);
        res.status(500).json({ error: e.message });
    }
});

// Lock/Unlock Language
app.post('/api/lock', (req, res) => {
    const { lang, locked } = req.body;
    if (!lang || typeof locked !== 'boolean') return res.status(400).json({ error: "Missing lang or locked state" });

    try {
        let currentData = {};
        if (fs.existsSync(METADATA_FILE)) {
            currentData = JSON.parse(fs.readFileSync(METADATA_FILE, 'utf8'));
        }

        if (!currentData.languageMetadata) currentData.languageMetadata = {};
        const existing = currentData.languageMetadata[lang] || {};

        currentData.languageMetadata[lang] = { ...existing, locked: locked, lastUpdated: new Date().toISOString() };

        fs.writeFileSync(METADATA_FILE, JSON.stringify(currentData, null, 4));
        res.json({ success: true, locked });
    } catch (e) {
        console.error("Lock error:", e);
        res.status(500).json({ error: e.message });
    }
});

const multer = require('multer');
const upload = multer({ dest: path.join(__dirname, 'uploads/') });

// Upload Media
app.post('/api/upload-media', upload.single('file'), (req, res) => {
    const { lang } = req.body;
    if (!req.file || !lang) {
        return res.status(400).json({ error: 'File and Language required' });
    }

    try {
        const langDir = path.join(LANGUAGES_DIR, lang, 'Media');
        if (!fs.existsSync(langDir)) {
            fs.mkdirSync(langDir, { recursive: true });
        }

        const ext = path.extname(req.file.originalname) || '.png';
        const filename = `${lang}_${Date.now()}${ext}`;
        const targetPath = path.join(langDir, filename);

        // Move file (copy then unlink because simple rename might fail across partitions/devices in some envs)
        try {
            fs.renameSync(req.file.path, targetPath);
        } catch (renameErr) {
            if (renameErr.code === 'EXDEV') {
                // Cross-device link not permitted, use copy and unlink
                fs.copyFileSync(req.file.path, targetPath);
                fs.unlinkSync(req.file.path);
            } else {
                throw renameErr;
            }
        }

        res.json({ success: true, filename: filename });
    } catch (e) {
        console.error("Error uploading media:", e);
        res.status(500).json({ error: e.message });
    }
});

// Download Media from URL
app.post('/api/download-media', async (req, res) => {
    const { url, lang } = req.body;
    if (!url || !lang) {
        return res.status(400).json({ error: 'URL and Language required' });
    }

    try {
        const fetch = (await import('node-fetch')).default;
        const response = await fetch(url);
        if (!response.ok) throw new Error(`Failed to fetch image: ${response.statusText}`);

        const buffer = await response.buffer();
        const ext = path.extname(url).split('?')[0] || '.png';
        const safeExt = ext.replace(/[^a-z0-9.]/gi, ''); // Simple sanitization
        const filename = `${lang}_${Date.now()}${safeExt}`;

        const langDir = path.join(LANGUAGES_DIR, lang, 'Media');
        if (!fs.existsSync(langDir)) {
            fs.mkdirSync(langDir, { recursive: true });
        }

        const filePath = path.join(langDir, filename);
        fs.writeFileSync(filePath, buffer);

        res.json({ success: true, filename: filename });
    } catch (e) {
        console.error("Error downloading media:", e);
        res.status(500).json({ error: e.message });
    }
});

// Logo Processing Endpoints
const logoProcessor = require('./logo_processor');

// Upload logo endpoint
app.post('/api/upload-logo', upload.single('logo'), async (req, res) => {
    try {
        const language = req.body.language;
        const file = req.file;

        if (!file || !language) {
            return res.status(400).json({ error: 'Missing file or language parameter' });
        }

        console.log(`Processing uploaded logo for ${language}: ${file.originalname}`);

        // Read file buffer
        const fileBuffer = fs.readFileSync(file.path);

        // Process logo (convert SVG if needed, apply tailoring)
        const logoPath = await logoProcessor.processUploadedLogo(
            fileBuffer,
            file.originalname,
            language
        );

        // Clean up uploaded file
        fs.unlinkSync(file.path);

        res.json({ success: true, path: logoPath });
    } catch (error) {
        console.error('Error uploading logo:', error);
        res.status(500).json({ error: error.message });
    }
});

// Fetch logo from URL endpoint
app.post('/api/fetch-logo', async (req, res) => {
    try {
        const { url, language } = req.body;

        if (!url || !language) {
            return res.status(400).json({ error: 'Missing url or language parameter' });
        }

        console.log(`Fetching logo from URL for ${language}: ${url}`);

        const logoPath = await logoProcessor.fetchLogoFromUrl(url, language);

        res.json({ success: true, path: logoPath });
    } catch (error) {
        console.error('Error fetching logo:', error);
        res.status(500).json({ error: error.message });
    }
});

// Generate Report
app.post('/api/generate-report', (req, res) => {
    console.log('Generating benchmark report...');

    // Use tsx for TypeScript execution (installed globally in Dockerfile)
    // Full-featured report with all visualizations and metadata
    const command = 'tsx Metrics/generate_report_only.ts';

    // CWD should be the root of the project (parent of server)
    // In Docker: /app (since server is at /app/server)
    const projectRoot = path.join(__dirname, '..');

    exec(command, { cwd: projectRoot, timeout: 120000 }, (error, stdout, stderr) => {
        if (stdout) console.log(stdout);
        if (stderr && !stderr.includes('ExperimentalWarning')) {
            console.error(stderr);
        }

        if (error) {
            console.error(`Report generation error: ${error}`);
            return res.status(500).json({
                success: false,
                error: 'Report generation failed',
                details: stderr || error.message
            });
        } else {
            console.log('Report generated successfully using TypeScript generator.');
            res.json({ success: true, generator: 'typescript' });
        }
    });
});

const SESSION_FILE = path.join(__dirname, '../session_state.json');

// Get Session State
app.get('/api/session-state', (req, res) => {
    res.header('Cache-Control', 'no-store');
    try {
        if (fs.existsSync(SESSION_FILE)) {
            const data = JSON.parse(fs.readFileSync(SESSION_FILE, 'utf8'));
            res.json(data);
        } else {
            res.json({}); // Empty state if no file
        }
    } catch (error) {
        console.error('Error reading session state:', error);
        res.status(500).json({ error: 'Failed to read session state' });
    }
});

// Save Session State
app.post('/api/session-state', (req, res) => {
    try {
        const state = req.body;
        // Validate? Not strictly necessary for this tool.
        fs.writeFileSync(SESSION_FILE, JSON.stringify(state, null, 2));
        res.json({ success: true });
    } catch (error) {
        console.error('Error saving session state:', error);
        res.status(500).json({ error: 'Failed to save session state' });
    }
});

// Helper: normalize language name for directory lookup
function normalizeLanguageName(lang) {
    if (!lang) return lang;
    // Convert display names to directory format
    const mapping = {
        'C#': 'C_Sharp',
        'F#': 'F_Sharp',
        'C++': 'C++'
    };
    return mapping[lang] || lang;
}

// Get metrics for a specific language (latest run)
app.get('/api/metrics/:language', (req, res) => {
    try {
        const lang = normalizeLanguageName(req.params.language);
        const metricsPath = path.join(LANGUAGES_DIR, lang, 'metrics.json');

        if (!fs.existsSync(metricsPath)) {
            return res.status(404).json({ error: 'Metrics not found for language: ' + lang });
        }

        const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));

        // metrics.json is an array of runs - get the latest one
        if (Array.isArray(metrics) && metrics.length > 0) {
            // Sort by timestamp descending and get the first
            const sorted = [...metrics].sort((a, b) =>
                new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime()
            );
            res.json(sorted[0]);
        } else {
            res.json(metrics);
        }
    } catch (error) {
        console.error('Error reading metrics:', error);
        res.status(500).json({ error: 'Failed to read metrics' });
    }
});

// Get C baseline metrics
app.get('/api/metrics/baseline/c', (req, res) => {
    try {
        const metricsPath = path.join(LANGUAGES_DIR, 'C', 'metrics.json');

        if (!fs.existsSync(metricsPath)) {
            return res.status(404).json({ error: 'C baseline metrics not found' });
        }

        const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));

        // Get the latest run
        if (Array.isArray(metrics) && metrics.length > 0) {
            const sorted = [...metrics].sort((a, b) =>
                new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime()
            );
            res.json(sorted[0]);
        } else {
            res.json(metrics);
        }
    } catch (error) {
        console.error('Error reading C baseline:', error);
        res.status(500).json({ error: 'Failed to read C baseline' });
    }
});

// Get all variants for a language (US-005)
app.get('/api/variants/:language', (req, res) => {
    try {
        const lang = normalizeLanguageName(req.params.language);
        const metricsPath = path.join(LANGUAGES_DIR, lang, 'metrics.json');

        if (!fs.existsSync(metricsPath)) {
            return res.status(404).json({ error: 'Language not found: ' + lang });
        }

        const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));

        if (!Array.isArray(metrics)) {
            // Single run without variant info
            return res.json([{
                variant: 'default',
                timestamp: metrics.timestamp || null,
                runType: metrics.runType || 'Local'
            }]);
        }

        // Extract unique variants from all runs
        const variants = metrics.map(run => ({
            variant: run.variant || 'default',
            timestamp: run.timestamp || null,
            runType: run.runType || 'Local'
        }));

        // Sort by timestamp descending (most recent first)
        variants.sort((a, b) =>
            new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime()
        );

        res.json(variants);
    } catch (error) {
        console.error('Error reading variants:', error);
        res.status(500).json({ error: 'Failed to read variants' });
    }
});

// Get metrics for a specific variant (US-006)
app.get('/api/metrics/:language/:variant', (req, res) => {
    try {
        const lang = normalizeLanguageName(req.params.language);
        const variant = req.params.variant;
        const metricsPath = path.join(LANGUAGES_DIR, lang, 'metrics.json');

        if (!fs.existsSync(metricsPath)) {
            return res.status(404).json({ error: 'Language not found: ' + lang });
        }

        const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));

        if (!Array.isArray(metrics)) {
            // Single run - check if variant matches
            const runVariant = metrics.variant || 'default';
            if (runVariant === variant || variant === 'default') {
                return res.json(metrics);
            }
            return res.status(404).json({ error: 'Variant not found: ' + variant });
        }

        // Find the run matching the variant (use most recent if multiple)
        const matchingRuns = metrics.filter(run =>
            (run.variant || 'default') === variant
        );

        if (matchingRuns.length === 0) {
            return res.status(404).json({ error: 'Variant not found: ' + variant });
        }

        // Return most recent run for this variant
        const sorted = matchingRuns.sort((a, b) =>
            new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime()
        );

        res.json(sorted[0]);
    } catch (error) {
        console.error('Error reading variant metrics:', error);
        res.status(500).json({ error: 'Failed to read variant metrics' });
    }
});

// ============================================================
// History API Endpoints (SQLite)
// ============================================================

// Get all benchmark history
app.get('/api/history', (req, res) => {
    try {
        const limit = parseInt(req.query.limit) || 500;
        const solver = req.query.solver;
        const since = req.query.since;

        const history = historyDb.getHistory({ limit, solver, since });
        res.json(history);
    } catch (error) {
        console.error('Error fetching history:', error);
        res.status(500).json({ error: 'Failed to fetch history' });
    }
});

// Get history for a specific solver
app.get('/api/history/:solver', (req, res) => {
    try {
        const solver = req.params.solver;
        const limit = parseInt(req.query.limit) || 100;

        const history = historyDb.getHistoryBySolver(solver, limit);
        res.json(history);
    } catch (error) {
        console.error('Error fetching solver history:', error);
        res.status(500).json({ error: 'Failed to fetch solver history' });
    }
});

// Get statistics for a solver
app.get('/api/history/:solver/stats', (req, res) => {
    try {
        const solver = req.params.solver;
        const stats = historyDb.getSolverStats(solver);
        res.json(stats);
    } catch (error) {
        console.error('Error fetching solver stats:', error);
        res.status(500).json({ error: 'Failed to fetch solver stats' });
    }
});

// Get recent activity
app.get('/api/history/recent/:hours', (req, res) => {
    try {
        const hours = parseInt(req.params.hours) || 24;
        const activity = historyDb.getRecentActivity(hours);
        res.json(activity);
    } catch (error) {
        console.error('Error fetching recent activity:', error);
        res.status(500).json({ error: 'Failed to fetch recent activity' });
    }
});

// Migrate from JSON to SQLite (one-time use)
app.post('/api/history/migrate', (req, res) => {
    try {
        const jsonPath = path.join(__dirname, '../benchmark_history.json');
        const result = historyDb.migrateFromJson(jsonPath);
        res.json(result);
    } catch (error) {
        console.error('Error during migration:', error);
        res.status(500).json({ error: 'Migration failed', details: error.message });
    }
});

// Initialize database on startup
historyDb.initDatabase();

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});
