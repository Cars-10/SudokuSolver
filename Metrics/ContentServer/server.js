
import express from 'express';
import cors from 'cors';
import multer from 'multer';
import { fileURLToPath } from 'url';
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

import path from 'path';
import fs from 'fs/promises';
import { existsSync, mkdirSync } from 'fs';
import fetch from 'node-fetch';
import { exec } from 'child_process';
import util from 'util';

const execPromise = util.promisify(exec);

const app = express();
const PORT = 9101;

// Helper to find CleanedUp root
const DATA_ROOT = path.resolve(__dirname, '../../CleanedUp');
const CLEANED_UP_ROOT = DATA_ROOT;
app.use(express.static(CLEANED_UP_ROOT));

// Ensure CleanedUp exists (it should, via mount)
if (!existsSync(CLEANED_UP_ROOT)) {
    console.error(`WARNING: ${CLEANED_UP_ROOT} does not exist. Ensure volume mount is correct.`);
}

// Multer storage for pasted/uploaded images
const storage = multer.diskStorage({
    destination: async function (req, file, cb) {
        const lang = req.body.lang;
        if (!lang) return cb(new Error("No language specified"), '');

        const mediaDir = path.join(CLEANED_UP_ROOT, 'Languages', lang, 'Media');
        try {
            await fs.mkdir(mediaDir, { recursive: true });
            cb(null, mediaDir);
        } catch (e) {
            cb(e, '');
        }
    },
    filename: function (req, file, cb) {
        // preserve extension or default to png
        const ext = path.extname(file.originalname) || '.png';
        const name = path.basename(file.originalname, ext) || 'pasted_image';
        cb(null, `${name}-${Date.now()}${ext}`);
    }
});
const upload = multer({ storage: storage });

// --- Endpoints ---

// 1. Get Metadata (Combined)
app.get('/api/metadata/:lang', async (req, res) => {
    const lang = req.params.lang;
    const metaPath = path.join(CLEANED_UP_ROOT, 'Languages', lang, 'metadata.json');
    try {
        const data = await fs.readFile(metaPath, 'utf-8');
        res.json(JSON.parse(data));
    } catch (e) {
        // Return 404 if not found, frontend falls back to hardcoded
        res.status(404).json({ error: "No custom metadata found" });
    }
});

// 2. Save Metadata
app.post('/api/save-metadata', async (req, res) => {
    const { lang, metadata } = req.body;
    if (!lang || !metadata) return res.status(400).json({ error: "Missing lang or metadata" });

    const langDir = path.join(CLEANED_UP_ROOT, 'Languages', lang);
    const metaPath = path.join(langDir, 'metadata.json');

    try {
        // Ensure dir exists (should exist if lang is valid)
        if (!existsSync(langDir)) {
            return res.status(404).json({ error: `Language directory ${lang} not found` });
        }

        // Merge with existing if exists? Or overwrite? 
        // Let's Read-Modify-Write if needed, but frontend probably sends full object. 
        // Let's just overwrite for now, frontend is source of truth during edit.
        // Actually, safer to read existing and merge in case we only send partial updates.
        let existing = {};
        try {
            const current = await fs.readFile(metaPath, 'utf-8');
            existing = JSON.parse(current);
        } catch (e) { }

        const final = { ...existing, ...metadata, lastUpdated: new Date().toISOString() };

        await fs.writeFile(metaPath, JSON.stringify(final, null, 2));
        res.json({ success: true, metadata: final });
    } catch (e) {
        console.error("Save error:", e);
        res.status(500).json({ error: e.message });
    }
});

// 2.5 Lock/Unlock Language
app.post('/api/lock', async (req, res) => {
    const { lang, locked } = req.body;
    if (!lang || typeof locked !== 'boolean') return res.status(400).json({ error: "Missing lang or locked state" });

    const langDir = path.join(CLEANED_UP_ROOT, 'Languages', lang);
    const metaPath = path.join(langDir, 'metadata.json');

    try {
        if (!existsSync(langDir)) {
            return res.status(404).json({ error: `Language directory ${lang} not found` });
        }

        let existing = {};
        try {
            const current = await fs.readFile(metaPath, 'utf-8');
            existing = JSON.parse(current);
        } catch (e) { }

        const final = { ...existing, locked: locked, lastUpdated: new Date().toISOString() };

        await fs.writeFile(metaPath, JSON.stringify(final, null, 2));
        res.json({ success: true, metadata: final });
    } catch (e) {
        console.error("Lock error:", e);
        res.status(500).json({ error: e.message });
    }
});

// 3. Upload Media (Paste/File Select)
app.post('/api/upload-media', upload.single('file'), (req, res) => {
    if (!req.file) return res.status(400).json({ error: "No file uploaded" });

    res.json({
        success: true,
        filename: req.file.filename,
        path: req.file.path
    });
});

// 4. Download Media (from URL)
app.post('/api/download-media', async (req, res) => {
    const { lang, url } = req.body;
    if (!lang || !url) return res.status(400).json({ error: "Missing lang or url" });

    const mediaDir = path.join(CLEANED_UP_ROOT, 'Languages', lang, 'Media');

    try {
        await fs.mkdir(mediaDir, { recursive: true });

        const response = await fetch(url);
        if (!response.ok) throw new Error(`Failed to fetch ${url}: ${response.statusText}`);

        const contentType = response.headers.get('content-type');
        let ext = '.png'; // Default
        if (contentType) {
            if (contentType.includes('svg')) ext = '.svg';
            else if (contentType.includes('jpeg') || contentType.includes('jpg')) ext = '.jpg';
            else if (contentType.includes('webp')) ext = '.webp';
        }

        // Try to infer filename from URL
        const urlPath = new URL(url).pathname;
        let filename = path.basename(urlPath);
        if (!filename || filename.length > 50) filename = `downloaded-${Date.now()}`;
        if (!path.extname(filename)) filename += ext;

        const destPath = path.join(mediaDir, filename);
        const arrayBuffer = await response.arrayBuffer();
        const buffer = Buffer.from(arrayBuffer);

        await fs.writeFile(destPath, buffer);

        res.json({ success: true, filename: filename });

    } catch (e) {
        console.error("Download error:", e);
        res.status(500).json({ error: e.message });
    }
});

// 5. Run Solver (Consolidated)
app.post('/api/run', async (req, res) => {
    const { language, matrix } = req.body;

    if (!language || !matrix) {
        return res.status(400).json({ error: 'Language and Matrix are required' });
    }

    console.log(`[Request] Run ${language} on ${matrix}`);

    // CleanedUp is at /data/CleanedUp inside the container
    const solverDir = path.join(CLEANED_UP_ROOT, 'Languages', language);
    const dockerFile = path.join(solverDir, 'Dockerfile');
    const matrixName = matrix.replace('.matrix', '');

    // Check for Dockerfile
    let useDocker = false;
    try {
        await fs.access(dockerFile);
        useDocker = true;
    } catch {
        useDocker = false;
    }

    try {
        if (useDocker) {
            // --- DOCKER EXECUTION ---
            const safeLangName = language.toLowerCase().replace(/[^a-z0-9]/g, '');
            const imageName = `sudoku-${safeLangName}`;

            console.log(`[Docker] Building image ${imageName}...`);
            // verify build
            await execPromise(`docker build -t ${imageName} .`, { cwd: solverDir });

            // Run
            const HOST_MATRICES_DIR = "/Users/cars10/GIT/SudokuSolver/Matrices";
            const containerMatrixPath = `../Matrices/${matrix}`;

            const command = `docker run --rm -v "${HOST_MATRICES_DIR}:/usr/src/Matrices" ${imageName} "${containerMatrixPath}"`;

            console.log(`[Docker] Executing: ${command}`);
            const { stdout, stderr } = await execPromise(command);

            res.json({
                success: true,
                mode: 'Docker',
                stdout: stdout,
                stderr: stderr
            });

        } else {
            // --- LOCAL EXECUTION ---
            const solverPath = path.join(solverDir, 'setupAndRunMe.sh');

            try {
                await fs.access(solverPath);
            } catch {
                return res.status(404).json({ error: `Solver script not found for ${language}` });
            }

            const globalScript = path.resolve(solverPath, '../../runMeGlobal.sh');
            // Force bash execution
            const command = `/bin/bash ${globalScript} ${language} ${matrixName}`;
            const cwd = path.dirname(globalScript);

            console.log(`[Local] Executing: ${command}`);

            const { stdout, stderr } = await execPromise(command, {
                cwd: cwd,
                timeout: 180000
            });

            res.json({
                success: true,
                mode: 'Local',
                stdout: stdout,
                stderr: stderr
            });
        }

    } catch (error) {
        console.error("Execution error:", error);
        res.status(500).json({
            success: false,
            error: error.message,
            stdout: error.stdout || "",
            stderr: error.stderr || ""
        });
    }
});

app.listen(PORT, () => {
    console.log(`Sudoku Content Server running on port ${PORT}`);
    console.log(`Mount point: ${CLEANED_UP_ROOT}`);
});
