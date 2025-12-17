const express = require('express');
const cors = require('cors');
const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');

const app = express();
const port = process.env.PORT || 9001;

app.use(cors());
app.use(express.json());

// Serve  directory for static assets (timestamp.js, images, etc.)
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
    let command = `./setupAndRunMe.sh`;
    if (matrix) {
        const matrixArg = `../../../Matrices/${matrix}`;
        command += ` ${matrixArg}`;
    }

    exec(command, { cwd: langDir }, (error, stdout, stderr) => {
        if (error) {
            console.error(`Exec error: ${error}`);
        }

        // 2. Read new metrics (generated by the script)
        let newMetrics = [];
        if (fs.existsSync(metricsFile)) {
            try {
                const content = fs.readFileSync(metricsFile, 'utf8');
                // Script might output arrays, we expect array
                newMetrics = JSON.parse(content);
            } catch (e) {
                console.error("Error reading new metrics:", e);
            }
        }

        // 3. Merge Phase
        // If we ran a specific matrix, we merge. If we ran ALL (matrix is falsy), we assume script handled all/overwrite is fine?
        // Actually, if we run ALL, the script overwrites everything, which is correct.
        // We only need to merge if we ran a *subset* (i.e. matrix is defined).

        let finalMetrics = newMetrics;
        if (matrix && previousMetrics.length > 0) {
            // Create a map of existing results
            const metricsMap = new Map(previousMetrics.map(m => [m.matrix, m]));

            // Update/Add new results
            if (Array.isArray(newMetrics)) {
                newMetrics.forEach(m => {
                    metricsMap.set(m.matrix, m);
                });
            }

            finalMetrics = Array.from(metricsMap.values());

            // Write merged back
            try {
                fs.writeFileSync(metricsFile, JSON.stringify(finalMetrics, null, 2));
            } catch (e) {
                console.error("Error writing merged metrics:", e);
            }
        }

        // Capture History
        try {
            const historyFile = path.join(__dirname, '../benchmark_history.json');
            let history = [];
            if (fs.existsSync(historyFile)) {
                history = JSON.parse(fs.readFileSync(historyFile, 'utf8'));
            }
            history.push({
                solver: language,
                runType: 'Local',
                timestamp: new Date().toISOString(),
                results: finalMetrics
            });
            fs.writeFileSync(historyFile, JSON.stringify(history, null, 2));
            console.log(`Appended history for ${language}`);
        } catch (hErr) {
            console.error("Error writing history:", hErr);
        }

        res.json({
            success: !error,
            stdout,
            stderr,
            exitCode: error ? error.code : 0
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

        // Move file (copy then unlink because simple rename might fail across partitions/devices in some envs, but rename is usually fine)
        fs.renameSync(req.file.path, targetPath);

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
            // Try JavaScript fallback
            console.log('Attempting JavaScript fallback generator...');
            const fallbackCommand = 'node Metrics/generate_report_simple.js';
            exec(fallbackCommand, { cwd: projectRoot }, (fallbackError, fallbackStdout, fallbackStderr) => {
                if (fallbackStdout) console.log(fallbackStdout);
                if (fallbackStderr) console.error(fallbackStderr);

                if (fallbackError) {
                    return res.status(500).json({
                        success: false,
                        error: 'Both TypeScript and JavaScript generators failed',
                        details: { tsx: stderr || error.message, js: fallbackStderr || fallbackError.message }
                    });
                }
                console.log('Report generated successfully using JavaScript fallback.');
                res.json({ success: true, generator: 'javascript' });
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

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});
