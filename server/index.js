const express = require('express');
const cors = require('cors');
const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');
const os = require('os');

const app = express();
const isRunningInDocker = false;

// Use default port 9002 for local host
const port = process.env.PORT || 9002;

console.log(`Server running on Host (${os.platform()})`);

app.use(cors());
app.use(express.json());

// Serve dist directory for Vite build assets (CSS, JS, sourcemaps)
app.use(express.static(path.join(__dirname, '../dist')));

// Serve root directory for static assets (timestamp.js, images, etc.)
app.use(express.static(path.join(__dirname, '..')));

// Serve server directory for static assets
app.use(express.static(path.join(__dirname, '.')));

// Serve the Benchmark Report at Root (Vite build: dist/index-v2.html)
// Falls back to index.html (legacy) if Vite build not available
app.get('/', (req, res) => {
    const vitePath = path.join(__dirname, '../dist/index-v2.html');
    const legacyPath = path.join(__dirname, '../index.html');

    // Try Vite build first (modern, modular UI)
    if (fs.existsSync(vitePath)) {
        res.sendFile(vitePath);
    }
    // Fallback to legacy build if Vite not available
    else if (fs.existsSync(legacyPath)) {
        console.warn('Vite build not found, serving legacy index.html. Run: npm run build');
        res.sendFile(legacyPath);
    }
    // No report available
    else {
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

// Serve language media assets
app.get('/media/:lang/:file', (req, res) => {
    const { lang, file } = req.params;
    // Basic sanitization
    const safeLang = normalizeLanguageName(lang).replace(/[^a-zA-Z0-9_#+\-\.]/g, '');
    const safeFile = file.replace(/[^a-zA-Z0-9_.\-]/g, '');
    
    const filePath = path.join(__dirname, '../Algorithms/BruteForce', safeLang, 'Media', safeFile);
    
    if (fs.existsSync(filePath)) {
        res.sendFile(filePath);
    } else {
        res.status(404).send('Media not found');
    }
});

const LANGUAGES_DIR = path.join(__dirname, '../Algorithms/BruteForce');
const MATRICES_DIR = path.join(__dirname, '../Matrices');

// Get git info for debug overlay
app.get('/api/git-info', (req, res) => {
    try {
        exec('git rev-parse --abbrev-ref HEAD', (err, branch) => {
            exec('git rev-parse --short HEAD', (err2, hash) => {
                res.json({
                    branch: branch ? branch.trim() : 'unknown',
                    hash: hash ? hash.trim() : 'unknown',
                    timestamp: new Date().toISOString()
                });
            });
        });
    } catch (error) {
        console.error('Error getting git info:', error);
        res.status(500).json({ error: 'Failed to get git info' });
    }
});

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
    const { language, matrix, useDocker } = req.body;

    if (!language) {
        return res.status(400).json({ error: 'Language is required' });
    }

    const langDir = path.join(LANGUAGES_DIR, language);
    // Only construct matrix path if matrix is provided
    const matrixPath = matrix ? path.join(MATRICES_DIR, matrix) : null;

    // Relative path to matrix from the language directory (where script runs)
    // server/../Algorithms/BruteForce/Lang -> server/../Matrices/1.matrix
    // We need to match how runBenchmarks.sh passes arguments
    // runBenchmarks.sh passes: ../../../Matrices/${m}.matrix
    // The CWD when running runMe.sh is the language directory.
    // So `../../Matrices` from `Algorithms/BruteForce/Lang` refers to `Matrices` at root.

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

    const executionMode = useDocker ? 'Docker' : 'Local';
    console.log(`Running ${language} with ${matrix || 'ALL matrices'} (${executionMode})...`);

    // Build the base command
    let runMeCommand = './runMe.sh';
    if (matrix) {
        const matrixArg = `../../../Matrices/${matrix}`;
        runMeCommand += ` ${matrixArg}`;
    }

    // Determine execution strategy based on server location and requested mode
    let command, execOptions;

    // Common exec options: 10 minute timeout, 50MB buffer for large outputs
    const commonExecOptions = {
        timeout: 600000,  // 10 minutes
        maxBuffer: 50 * 1024 * 1024  // 50MB
    };

    if (isRunningInDocker) {
        // Server is inside Docker - always run directly (we're already in the container)
        if (!useDocker) {
            console.warn(`Note: Local mode requested but server is in Docker. Running in container.`);
        }
        command = runMeCommand;
        execOptions = { ...commonExecOptions, cwd: langDir };
    } else {
        // Server is on host machine
        if (useDocker) {
            // Docker mode: run via docker-compose exec
            const dockerLangDir = `/app/Algorithms/BruteForce/${language}`;
            command = `docker-compose exec -T app bash -c "cd ${dockerLangDir} && ${runMeCommand}"`;
            execOptions = { ...commonExecOptions, cwd: path.join(__dirname, '..') }; // Project root
        } else {
            // Local mode: run directly on host
            command = runMeCommand;
            execOptions = { ...commonExecOptions, cwd: langDir };
        }
    }

    exec(command, execOptions, (error, stdout, stderr) => {
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

        res.json({
            success: !error,
            stdout,
            stderr,
            exitCode: error ? error.code : 0,
            timestamp: runTimestamp  // Return timestamp for client-side update
        });
    });
});

const METADATA_FILE = path.join(__dirname, '../Algorithms/metadata.json');

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

// Get Solver Source Code
app.get('/api/source/:lang', (req, res) => {
    const lang = decodeURIComponent(req.params.lang);
    const algo = req.query.algorithm || 'BruteForce';
    const langDir = path.join(__dirname, '..', 'Algorithms', algo, lang);

    console.log(`[Source API] Requested: ${lang} (${algo}), Path: ${langDir}, Docker: ${isRunningInDocker}`);

    if (!fs.existsSync(langDir)) {
        console.error(`[Source API] Directory not found: ${langDir}`);
        return res.status(404).json({ error: 'Language not found', path: langDir });
    }

    try {
        let solverFile;
        let explicitPath = null;

        // 1. Try to get from benchmark_config.json
        if (fs.existsSync(CONFIG_FILE)) {
            try {
                const config = JSON.parse(fs.readFileSync(CONFIG_FILE, 'utf8'));
                const key = `${algo}/${lang}`;
                if (config.languages && config.languages[key] && config.languages[key].source_file) {
                    // Resolve path relative to project root
                    explicitPath = path.join(__dirname, '..', config.languages[key].source_file);
                    if (fs.existsSync(explicitPath)) {
                        console.log(`[Source API] Found explicit source path in config: ${explicitPath}`);
                        solverFile = path.basename(explicitPath);
                    } else {
                        console.warn(`[Source API] Config path not found on disk: ${explicitPath}`);
                        explicitPath = null;
                    }
                }
            } catch (e) {
                console.warn(`[Source API] Failed to read config for source lookup: ${e.message}`);
            }
        }

        // 2. Fallback to directory scanning if no config match
        if (!explicitPath) {
            const files = fs.readdirSync(langDir);
            const lowerAlgo = algo.toLowerCase();
            
            solverFile = files.find(f => {
                const lower = f.toLowerCase();
                // Ignore artifacts
                if (f.endsWith('.class') || f.endsWith('.o') || f.endsWith('.beam') || f.endsWith('.exe') || f.endsWith('.dll') || f.endsWith('.jar')) return false;
                if (f === 'runMe.sh' || f === 'metrics.json' || f === 'README.md' || f === 'common.sh') return false;
                if (f.startsWith('.')) return false;

                // Algorithm specific checks
                if (algo === 'BruteForce') {
                    if (lang === 'Java' && f === 'Sudoku.java') return true;
                    if (lang === 'EmacsLisp' && f === 'sudoku.el') return true;
                    if (lang === 'Pascal' && f === 'sudoku.pas') return true;
                    return lower.startsWith('sudoku.');
                }
                if (algo === 'DLX') return lower.includes('dlx') && !lower.includes('test');
                if (algo === 'CP') return lower.includes('cp') && !lower.includes('test');
                return false;
            });
            
            // Fallback for DLX/CP if strict check failed (look for *any* source file if not found)
            if (!solverFile) {
                 const validExtensions = ['.c', '.cpp', '.cc', '.rs', '.go', '.java', '.js', '.ts', '.py', '.rb', '.pl', '.php', '.bas', '.f90', '.pas', '.nim', '.cr', '.zig', '.v', '.vala', '.jl', '.kt', '.swift', '.clj', '.ex', '.lisp', '.hs', '.ml', '.cs', '.fs', '.el', '.io', '.factor'];
                 solverFile = files.find(f => {
                    const ext = path.extname(f).toLowerCase();
                    return validExtensions.includes(ext) && !f.toLowerCase().includes('test');
                 });
            }
            
            // General Fallback
            if (!solverFile) {
                 const validExtensions = ['.c', '.cpp', '.cc', '.rs', '.go', '.java', '.js', '.ts', '.py', '.rb', '.pl', '.php', '.bas', '.f90', '.pas', '.nim', '.cr', '.zig', '.v', '.vala', '.jl', '.kt', '.swift', '.clj', '.ex', '.lisp', '.hs', '.ml', '.cs', '.fs', '.io', '.factor'];
                 solverFile = files.find(f => {
                    const ext = path.extname(f).toLowerCase();
                    return validExtensions.includes(ext) && !f.toLowerCase().includes('test');
                 });
            }
        }

        if (!solverFile && !explicitPath) {
            console.error(`[Source API] Solver file not found in: ${langDir}`);
            const files = fs.readdirSync(langDir); // Re-read for logging
            return res.status(404).json({ error: 'Solver file not found', files: files });
        }

        const filePath = explicitPath || path.join(langDir, solverFile);
        
        // Check file size
        const stats = fs.statSync(filePath);
        if (stats.size > 1024 * 1024) { // 1MB limit
            console.warn(`[Source API] File too large: ${filePath} (${stats.size} bytes)`);
            return res.json({ 
                filename: solverFile, 
                source: `// Source file is too large to display (${(stats.size / 1024 / 1024).toFixed(2)} MB).\n// File: ${solverFile}` 
            });
        }

        const source = fs.readFileSync(filePath, 'utf8');
        console.log(`[Source API] Success: ${filePath} (${source.length} bytes)`);
        res.json({ filename: solverFile, source: source });
    } catch (e) {
        console.error("[Source API] Error reading source:", e);
        res.status(500).json({ error: e.message });
    }
});

// Get Solver Readme
app.get('/api/readme/:lang', (req, res) => {
    const lang = decodeURIComponent(req.params.lang);
    const algo = req.query.algorithm || 'BruteForce';
    const langDir = path.join(__dirname, '..', 'Algorithms', algo, lang);

    if (!fs.existsSync(langDir)) {
        return res.status(404).json({ error: 'Language not found' });
    }

    const readmePath = path.join(langDir, 'README.md');
    if (!fs.existsSync(readmePath)) {
        return res.status(404).json({ error: 'README.md not found' });
    }

    try {
        const stats = fs.statSync(readmePath);
        if (stats.size > 1024 * 1024) {
            return res.json({ 
                filename: 'README.md', 
                source: `// README file is too large to display (${(stats.size / 1024 / 1024).toFixed(2)} MB).` 
            });
        }
        const source = fs.readFileSync(readmePath, 'utf8');
        res.json({ filename: 'README.md', source: source });
    } catch (e) {
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
    const { lang, isLogo } = req.body;
    if (!req.file || !lang) {
        return res.status(400).json({ error: 'File and Language required' });
    }

    try {
        const langDir = path.join(LANGUAGES_DIR, lang, 'Media');
        if (!fs.existsSync(langDir)) {
            fs.mkdirSync(langDir, { recursive: true });
        }

        const ext = path.extname(req.file.originalname) || '.png';
        // If isLogo flag is set, use canonical logo name (overwrites existing)
        const filename = isLogo === 'true' ? `${lang}_logo${ext}` : `${lang}_${Date.now()}${ext}`;
        const targetPath = path.join(langDir, filename);

        // If logo, remove existing logo files with different extensions first
        if (isLogo === 'true') {
            const existingLogos = ['.png', '.jpg', '.svg'].map(e => path.join(langDir, `${lang}_logo${e}`));
            existingLogos.forEach(p => {
                if (fs.existsSync(p) && p !== targetPath) {
                    fs.unlinkSync(p);
                    console.log(`Removed old logo: ${p}`);
                }
            });
        }

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

        console.log(`Uploaded ${isLogo === 'true' ? 'logo' : 'media'}: ${targetPath}`);
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

// Generate Report via Vite build
app.post('/api/generate-report', (req, res) => {
    console.log('Building Vite report...');

    const command = isRunningInDocker
        ? 'npm run build'
        : 'npm run build';

    // CWD should be the root of the project (parent of server)
    const projectRoot = path.join(__dirname, '..');

    exec(command, { cwd: projectRoot, timeout: 120000 }, (error, stdout, stderr) => {
        if (stdout) console.log(stdout);
        if (stderr && !stderr.includes('ExperimentalWarning')) {
            console.error(stderr);
        }

        if (error) {
            console.error(`Build error: ${error}`);
            return res.status(500).json({
                success: false,
                error: 'Build failed',
                details: stderr || error.message
            });
        } else {
            console.log('Build completed successfully. dist/index-v2.html is ready.');
            res.json({ success: true, generator: 'vite' });
        }
    });
});

const SESSION_FILE = path.join(__dirname, '../session_state.json');
const CONFIG_FILE = path.join(__dirname, '../benchmark_config.json');

// Get Benchmark Config
app.get('/api/config', (req, res) => {
    try {
        if (fs.existsSync(CONFIG_FILE)) {
            const data = JSON.parse(fs.readFileSync(CONFIG_FILE, 'utf8'));
            res.json(data);
        } else {
            res.status(404).json({ error: 'Config file not found' });
        }
    } catch (error) {
        console.error('Error reading config:', error);
        res.status(500).json({ error: 'Failed to read config' });
    }
});

// Save Benchmark Config
app.post('/api/config', (req, res) => {
    try {
        const config = req.body;
        // Basic validation: ensure scoring_weights exists if we're updating it
        if (config.scoring_weights) {
            const weights = config.scoring_weights;
            const total = (weights.time || 0) + (weights.memory || 0);
            // We don't strictly enforce 1.0 here yet, but we could
            console.log(`Updating scoring weights: Time=${weights.time}, Memory=${weights.memory}`);
        }

        fs.writeFileSync(CONFIG_FILE, JSON.stringify(config, null, 2));
        res.json({ success: true });
    } catch (error) {
        console.error('Error saving config:', error);
        res.status(500).json({ error: 'Failed to save config' });
    }
});

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
        'C++': 'C++',
        'Objective-C': 'Objective-C',
    };
    return mapping[lang] || lang;
}

// Helper: get the latest run from a metrics array (sorted by timestamp descending)
function getLatestRun(metrics) {
    if (!Array.isArray(metrics) || metrics.length === 0) {
        return metrics;
    }
    const sorted = [...metrics].sort((a, b) =>
        new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime()
    );
    return sorted[0];
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
        res.json(getLatestRun(metrics));
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
        res.json(getLatestRun(metrics));
    } catch (error) {
        console.error('Error reading C baseline:', error);
        res.status(500).json({ error: 'Failed to read C baseline' });
    }
});

// Get all variants for a language (US-005) - aggregated across all algorithms
app.get('/api/variants/:language', (req, res) => {
    try {
        const lang = normalizeLanguageName(req.params.language);
        const algorithms = ['BruteForce', 'DLX', 'CP'];
        let allVariants = [];

        algorithms.forEach(algo => {
            const metricsPath = path.join(__dirname, '..', 'Algorithms', algo, lang, 'metrics.json');
            if (fs.existsSync(metricsPath)) {
                try {
                    const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));
                    if (Array.isArray(metrics)) {
                        metrics.forEach(run => {
                            allVariants.push({
                                variant: `${run.variant || 'default'} (${algo})`, // Disambiguate
                                originalVariant: run.variant || 'default',
                                algorithm: algo,
                                timestamp: run.timestamp || null,
                                runType: run.runType || 'Local'
                            });
                        });
                    } else {
                         allVariants.push({
                            variant: `default (${algo})`,
                            originalVariant: 'default',
                            algorithm: algo,
                            timestamp: metrics.timestamp || null,
                            runType: metrics.runType || 'Local'
                        });
                    }
                } catch (e) {
                    console.warn(`Error reading metrics for ${algo}/${lang}:`, e.message);
                }
            }
        });

        if (allVariants.length === 0) {
            return res.status(404).json({ error: 'Language not found in any algorithm' });
        }

        // Sort by timestamp descending
        allVariants.sort((a, b) =>
            new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime()
        );

        res.json(allVariants);
    } catch (error) {
        console.error('Error reading variants:', error);
        res.status(500).json({ error: 'Failed to read variants' });
    }
});

// Get metrics for a specific variant (US-006) - now supports algo selection
app.get('/api/metrics/:language/:variant', (req, res) => {
    try {
        const lang = normalizeLanguageName(req.params.language);
        let variantParam = req.params.variant;
        
        // Extract algo if present in "Variant (Algo)" format
        let targetAlgo = null;
        const algoMatch = variantParam.match(/(.*) \((BruteForce|DLX|CP)\)$/);
        if (algoMatch) {
            variantParam = algoMatch[1];
            targetAlgo = algoMatch[2];
        }

        // Search locations
        const algorithms = targetAlgo ? [targetAlgo] : ['BruteForce', 'DLX', 'CP'];
        
        for (const algo of algorithms) {
            const metricsPath = path.join(__dirname, '..', 'Algorithms', algo, lang, 'metrics.json');
            if (fs.existsSync(metricsPath)) {
                const metrics = JSON.parse(fs.readFileSync(metricsPath, 'utf8'));
                
                if (!Array.isArray(metrics)) {
                    if ((metrics.variant || 'default') === variantParam) {
                        // Inject algorithm type if missing
                        if (!metrics.algorithmType) metrics.algorithmType = algo;
                        return res.json(metrics);
                    }
                } else {
                    const matchingRuns = metrics.filter(run => 
                        (run.variant || 'default') === variantParam
                    );
                    if (matchingRuns.length > 0) {
                        // Return most recent
                        matchingRuns.sort((a, b) => new Date(b.timestamp || 0).getTime() - new Date(a.timestamp || 0).getTime());
                        const result = matchingRuns[0];
                        if (!result.algorithmType) result.algorithmType = algo;
                        return res.json(result);
                    }
                }
            }
        }

        return res.status(404).json({ error: 'Variant not found: ' + variantParam });
    } catch (error) {
        console.error('Error reading variant metrics:', error);
        res.status(500).json({ error: 'Failed to read variant metrics' });
    }
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});
