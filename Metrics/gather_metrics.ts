import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';
import { exec } from 'child_process';
import * as util from 'util';
import { fileURLToPath } from 'url';
import puppeteer from 'puppeteer';
import { languageHistories, quotes, personalities, methodologyTexts, languageMetadata } from './LanguagesMetadata.ts';
export { languageHistories, quotes, personalities, methodologyTexts, languageMetadata };

const execPromise = util.promisify(exec);
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export interface MetricResult {
    matrix: string;
    time: number;
    iterations: number;
    memory: number;
    cpu_user: number;
    cpu_sys: number;
    status: string;
    output?: string;
}

export interface SolverMetrics {
    solver: string;
    runType?: string; // 'Local' | 'Docker' | 'AI' | 'Manual'
    timestamp: string;
    results: MetricResult[];
}

// --- Logic ---

export async function readReferenceOutputs(rootDir: string): Promise<Record<string, string>> {
    const refPath = path.join(rootDir, 'Matrices', 'ReferenceForAllMatrixRun.txt');
    try {
        const content = await fs.readFile(refPath, 'utf-8');
        const refs: Record<string, string> = {};
        const parts = content.split('../Matrices/');
        for (const part of parts) {
            if (!part.trim()) continue;
            const lines = part.split('\n');
            const matrixName = lines[0].trim();
            const output = lines.slice(1).join('\n').trim();
            refs[matrixName] = output;
        }
        return refs;
    } catch (e) {
        console.error('Failed to read reference outputs:', e);
        return {};
    }
}

export async function findSolvers(rootDir: string): Promise<string[]> {
    // Find all runMe_ai.sh files in Manual and AI-2025
    console.log(`Searching for solvers in Manual and AI-2025...`);

    const manualSolvers = await glob(path.join(rootDir, 'Manual', '*/runMe_ai.sh'));
    const aiSolvers = await glob(path.join(rootDir, 'AI-2025', '*/runMe_ai.sh'));
    // For CleanedUp, we look for language directories directly
    const cleanedUpSolvers = await glob(path.join(rootDir, 'CleanedUp', 'Languages', '*', 'setupAndRunMe.sh'));

    const allSolvers = [...manualSolvers, ...aiSolvers, ...cleanedUpSolvers];
    const filteredSolvers = allSolvers.filter(s => !s.includes('/Racket/') && !s.includes('/Cobol/'));

    console.log(`Found ${filteredSolvers.length} solvers (excluding Racket and Cobol):`, filteredSolvers);
    return filteredSolvers;
}

export async function runSolver(scriptPath: string, matrixPattern: string): Promise<SolverMetrics | null> {
    let solverDir: string;
    let langName: string;
    let parentDir: string;

    if (scriptPath.endsWith('.sh')) {
        solverDir = path.dirname(scriptPath);
        langName = path.basename(solverDir);
        parentDir = path.basename(path.dirname(solverDir));
    } else {
        // Assume it's a directory (CleanedUp/Languages/Lang)
        solverDir = scriptPath;
        langName = path.basename(solverDir);
        parentDir = path.basename(path.dirname(solverDir)); // Should be 'Languages'
    }

    let solverName = langName;
    let runType = 'Local';

    if (parentDir === 'Manual') {
        runType = 'Manual';
    } else if (parentDir === 'AI-2025') {
        runType = 'AI';
    } else if (parentDir === 'Languages') {
        runType = 'Local';
    }

    console.log(`Running solver: ${solverName} on ${matrixPattern}`);

    try {
        let command: string;
        let cwd: string;

        if (parentDir === 'Languages') {
            // Use runMeGlobal.sh
            // matrixPattern is like "1.matrix", runMeGlobal expects "1"
            const matrixName = matrixPattern.replace('.matrix', '');
            const globalScript = path.resolve(solverDir, '../../runMeGlobal.sh');
            command = `${globalScript} ${langName} ${matrixName}`;
            cwd = path.dirname(globalScript); // Run from CleanedUp dir
        } else {
            // Legacy behavior
            command = `./runMe_ai.sh "../../Matrices/${matrixPattern}"`;
            cwd = solverDir;
        }

        const { stdout, stderr } = await execPromise(command, {
            cwd: cwd,
            timeout: 180000 // 3 minutes
        });

        try {
            const results = JSON.parse(stdout);
            return {
                solver: solverName,
                runType: runType,
                timestamp: new Date().toISOString(),
                results: results
            };
        } catch (e) {
            console.error(`Failed to parse JSON output from ${solverName}:`, e);
            console.log("Stdout:", stdout);
            return null;
        }
    } catch (e: any) {
        if (e.killed || e.signal === 'SIGTERM' || e.code === 143) {
            console.error(`Solver ${solverName} timed out after 3 minutes.`);
            return {
                solver: solverName,
                runType: runType,
                timestamp: new Date().toISOString(),
                results: [{
                    matrix: matrixPattern,
                    time: 180, // Cap at timeout
                    iterations: 0,
                    memory: 0,
                    cpu_user: 0,
                    cpu_sys: 0,
                    status: "timeout",
                    output: "Solver timed out after 180s"
                }]
            };
        }
        console.error(`Failed to run solver ${solverName}:`, e);
        return null;
    }
}

export async function runDockerSolver(scriptPath: string, matrixPattern: string): Promise<SolverMetrics | null> {
    let solverDir: string;
    let langName: string;

    if (scriptPath.endsWith('.sh')) {
        solverDir = path.dirname(scriptPath);
        langName = path.basename(solverDir);
    } else {
        solverDir = scriptPath;
        langName = path.basename(solverDir);
    }

    const solverName = langName;
    const runType = 'Docker';
    console.log(`Running solver in Docker: ${solverName} on ${matrixPattern}`);

    const dockerFile = path.join(solverDir, 'Dockerfile');
    try {
        await fs.access(dockerFile);
    } catch {
        console.log(`Skipping ${langName}: No Dockerfile found.`);
        return null;
    }

    try {
        // 1. Build Image
        // Sanitize image name: lowercase, replace special chars with nothing or safe chars
        const safeLangName = langName.toLowerCase().replace(/[^a-z0-9]/g, '');
        const imageName = `sudoku-${safeLangName}`;
        console.log(`Building Docker image: ${imageName}...`);
        await execPromise(`docker build -t ${imageName} .`, { cwd: solverDir });

        // 2. Run Container
        // Mount Matrices directory to /usr/src/Matrices (standard convention in our Dockerfiles?)
        // The Dockerfile usually copies setupAndRunMe.sh which expects to be run.
        // We need to mount the Matrices directory so the container can access the matrix files.
        // Assuming the repo structure: Root/Matrices and Root/CleanedUp/Languages/Lang

        // Resolve Matrices path relative to solverDir
        // solverDir is .../CleanedUp/Languages/Lang
        // Matrices is .../Matrices (which is ../../../Matrices from Lang)
        // actually it is /Users/cars10/GIT/SudokuSolver/Matrices

        // Let's find the repo root to be safe
        const repoRoot = path.resolve(solverDir, '../../../');
        const matricesDir = path.join(repoRoot, 'Matrices');

        // The container expects arguments for the matrix files.
        // The setupAndRunMe.sh inside the container usually takes paths relative to itself or absolute.
        // But we are mounting Matrices to /usr/src/Matrices (or similar).
        // Let's check the C++ Dockerfile again.
        // COPY setupAndRunMe.sh .
        // ENTRYPOINT ["./setupAndRunMe.sh"]
        // And we ran: docker run ... sudoku-cpp ../Matrices/1.matrix
        // So inside the container, we are at WORKDIR /usr/src/sudoku
        // And we mounted host Matrices to /usr/src/Matrices? 
        // Wait, in my manual test I did: -v /Users/.../Matrices:/usr/src/Matrices
        // And passed argument: ../Matrices/1.matrix
        // So inside container: /usr/src/sudoku is CWD. /usr/src/Matrices is sibling.
        // So ../Matrices/1.matrix is valid.

        const matrixName = matrixPattern.replace('.matrix', ''); // "1"
        // But setupAndRunMe.sh takes file paths usually?
        // In C++ setupAndRunMe.sh:
        // MATRICES="$@"
        // if [ -z "$MATRICES" ]; then MATRICES="../../Matrices/*.matrix"; fi
        // It iterates over them.

        // So we should pass "../Matrices/1.matrix" if we mount to /usr/src/Matrices
        // and WORKDIR is /usr/src/sudoku.

        const containerMatrixPath = `../Matrices/${matrixPattern}`;

        const command = `docker run --rm -v "${matricesDir}:/usr/src/Matrices" ${imageName} "${containerMatrixPath}"`;

        console.log(`Executing: ${command}`);
        const { stdout, stderr } = await execPromise(command);

        try {
            // The output should be JSON.
            // However, there might be other output (like "Warning: Reference file...").
            // We need to extract the JSON part.
            // The C++ setupAndRunMe.sh prints the JSON at the end.
            // Let's look for the last occurrence of '[' and ']'?
            // Or just parse the whole thing if it's clean?
            // In the manual run, it had a warning before the JSON.

            const jsonStart = stdout.indexOf('[');
            const jsonEnd = stdout.lastIndexOf(']');
            if (jsonStart === -1 || jsonEnd === -1) {
                throw new Error("No JSON array found in output");
            }

            const jsonStr = stdout.substring(jsonStart, jsonEnd + 1);
            const results = JSON.parse(jsonStr);

            return {
                solver: solverName,
                runType: runType,
                timestamp: new Date().toISOString(),
                results: results
            };

        } catch (e) {
            console.error(`Failed to parse JSON output from Docker ${solverName}:`, e);
            console.log("Stdout:", stdout);
            return null;
        }

    } catch (e: any) {
        console.error(`Failed to run Docker solver ${solverName}:`, e);
        return null;
    }
}

export async function generateHtml(metrics: SolverMetrics[], history: any[], personalities: any, languageMetadata: any, methodologyTexts: any, referenceOutputs: any, allowedMatrices: string[] = []): Promise<string> {

    // Pre-load local logos
    const localLogos = await glob('CleanedUp/logos/*.png');
    const logoMap = new Map<string, string>();
    for (const p of localLogos) {
        const filename = path.basename(p);
        const name = path.basename(p, '.png').toLowerCase();
        logoMap.set(name, `CleanedUp/logos/${filename}`);
    }

    // Read Tailoring Config
    let tailoringConfig = {};
    try {
        const rootDir = path.resolve(__dirname, '..');
        const tailoringPath = path.join(rootDir, 'CleanedUp', 'logos', 'Tailoring.json');
        const tailoringContent = await fs.readFile(tailoringPath, 'utf-8');
        tailoringConfig = JSON.parse(tailoringContent);
    } catch (e) {
        console.warn("Could not read Tailoring.json", e);
    }
    // Read Matrix Files
    const matricesDir = path.resolve(__dirname, '../Matrices');
    let matrixFiles = await glob('*.matrix', { cwd: matricesDir });

    // Filter matrices if allowed list is provided
    if (allowedMatrices && allowedMatrices.length > 0) {
        console.log(`Filtering matrices to: ${allowedMatrices.join(', ')}`);
        matrixFiles = matrixFiles.filter(f => allowedMatrices.includes(f));
    }

    const matrixContents: string[] = [];

    for (const file of matrixFiles) {
        if (file === '[12].matrix') continue; // Skip the combined one if present
        const content = await fs.readFile(path.join(matricesDir, file), 'utf-8');
        matrixContents.push(content);
    }

    const maxMatrices = 5; // Focus on Matrix 1-5

    // Sort results by total time (fastest first)
    const sortedMetrics = [...metrics].sort((a, b) => {
        const timeA = a.results.reduce((acc, r) => acc + r.time, 0);
        const timeB = b.results.reduce((acc, r) => acc + r.time, 0);
        return timeA - timeB;
    });

    // Find C baseline
    const cMetrics = metrics.find(m => m.solver === 'C' || m.solver === 'C (Manual)');
    const cTimes = cMetrics ? cMetrics.results.map(r => r.time) : [];
    const cTotalIters = cMetrics ? cMetrics.results.reduce((a, b) => a + b.iterations, 0) : 0;

    // Filter out mismatches
    // Filter out mismatches - REVERTED to allow client-side toggle
    // if (cTotalIters > 0) {
    //     metrics = metrics.filter(m => {
    //         if (m.solver === 'C' || m.solver === 'C (Manual)') return true;
    //         const totalIters = m.results.reduce((a, b) => a + b.iterations, 0);
    //         if (totalIters !== cTotalIters) {
    //             console.log(`Marking ${m.solver} as mismatch: ${totalIters} vs ${cTotalIters}`);
    //             // return false; // Don't exclude, just mark
    //         }
    //         return true;
    //     });
    // }

    // C Baselines for Composite Score
    const cTotalTime = cTimes.reduce((a, b) => a + b, 0);
    const cTotalMem = cMetrics ? Math.max(...cMetrics.results.map(r => r.memory)) : 1; // Max RSS
    const cTotalCpu = cMetrics ? cMetrics.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0) : 1;

    // Calculate mismatch count
    let mismatchCount = 0;
    if (cTotalIters > 0) {
        mismatchCount = metrics.filter(m => {
            if (m.solver === 'C' || m.solver === 'C (Manual)') return false;
            const totalIters = m.results.reduce((a, b) => a + b.iterations, 0);
            return totalIters !== cTotalIters;
        }).length;
    }

    let html = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <!-- <meta http-equiv="refresh" content="60"> Removed for smart refresh -->
    <script>
        (function() {
            // Smart Refresh for File Protocol
            // We can't use fetch() on file:// due to CORS.
            // Instead, we poll by injecting a script tag that loads a timestamp.
            
            let initialTimestamp = null;
            
            window.checkTimestamp = function() {
                // This function is called after timestamp.js loads (if we wanted a callback)
                // But timestamp.js just sets window.latestTimestamp
                if (typeof window.latestTimestamp !== 'undefined') {
                    if (initialTimestamp === null) {
                        initialTimestamp = window.latestTimestamp;
                    } else if (window.latestTimestamp !== initialTimestamp) {
                        console.log('File changed, reloading...');
                        window.location.reload();
                    }
                }
            };

            function poll() {
                if (!document.body) return; // Wait for body
                
                const script = document.createElement('script');
                // Add cache buster
                script.src = 'timestamp.js?t=' + Date.now();
                script.onload = () => {
                    window.checkTimestamp();
                    if (document.body.contains(script)) {
                        document.body.removeChild(script); // Clean up
                    }
                };
                script.onerror = () => {
                    // console.warn('Failed to load timestamp.js');
                    if (document.body && document.body.contains(script)) {
                        document.body.removeChild(script);
                    }
                };
                document.body.appendChild(script);
            }
            
            // Initial check - wait for DOM
            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', poll);
            } else {
                poll();
            }
            
            // Poll every 2 seconds
            setInterval(poll, 2000);
        })();
    </script>
    <link rel="icon" type="image/png" href="favicon.png">
    <title>Sudoku Benchmark - Neon</title>
    <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap" rel="stylesheet">
    <style>
        :root {
            --bg: #0d0d12;
            --surface: #16161e;
            --primary: #00ff9d;
            --secondary: #00b8ff;
            --text: #e0e0e0;
            --muted: #5c5c66;
            --border: #2a2a35;
        }
        body {
            font-family: 'JetBrains Mono', monospace;
            background-color: var(--bg);
            color: var(--text);
            margin: 0;
            padding: 40px;
            overflow-x: hidden; /* Hide horizontal scrollbar */
        }
        body.fullscreen-active {
            overflow: hidden; /* Hide all scrollbars in fullscreen */
        }
        h1 {
            text-align: center;
            color: var(--primary);
            text-transform: uppercase;
            letter-spacing: 2px;
            margin-bottom: 40px;
            text-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            overflow-x: auto;
        }
        table {
            width: 100%;
            border-collapse: separate;
            border-spacing: 0 4px;
        }
        th {
            text-align: center;
            padding: 15px;
            color: var(--secondary);
            font-size: 1.1em;
            text-transform: uppercase;
            border-bottom: 2px solid var(--border);
        }
        td {
            background-color: var(--surface);
            padding: 15px;
            border-top: 1px solid var(--border);
            border-bottom: 1px solid var(--border);
            transition: transform 0.2s, box-shadow 0.2s;
        }
        tr:hover td {
            background-color: #1c1c26;
            border-color: var(--secondary);
            transform: scale(1.01);
            box-shadow: 0 0 15px rgba(0, 184, 255, 0.1);
            z-index: 10;
            position: relative;
        }
        td:hover {
            background-color: #252530 !important;
            border: 1px solid var(--primary) !important;
            z-index: 30 !important;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.3) !important;
        }
        .active-row td {
            background-color: #1c1c26;
            border-color: var(--primary);
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.2);
            z-index: 20;
            position: relative;
            animation: glow 1.5s infinite alternate;
        }
        @keyframes glow {
            from { box-shadow: 0 0 10px rgba(0, 255, 157, 0.1); }
            to { box-shadow: 0 0 20px rgba(0, 255, 157, 0.3); }
        }
        .lang-col {
            font-weight: bold;
            color: var(--primary);
            border-left: 3px solid transparent;
            cursor: pointer;
            text-decoration: underline;
            text-decoration-style: dotted;
            text-decoration-color: var(--secondary);
        }
        .lang-col:hover {
            border-left-color: var(--primary);
        }
        .lang-logo {
            width: 20px;
            height: 20px;
            margin-right: 8px;
            vertical-align: middle;
            object-fit: contain;
        }
        .lang-year {
            font-size: 0.7em;
            color: var(--muted);
            font-weight: normal;
            text-decoration: none;
            margin-top: 2px;
        }
        .active-row .lang-col {
            border-left-color: var(--primary);
        }
        .cell-content {
            display: flex;
            flex-direction: column;
            gap: 4px;
        }
        .time {
            font-size: 1.1em;
            font-weight: bold;
            color: #fff;
        }
        .meta {
            font-size: 0.75em;
            color: var(--muted);
            display: flex;
            gap: 10px;
        }
        .meta span {
            display: inline-block;
        }
        .total-time {
            font-size: 1.2em;
            color: var(--secondary);
            font-weight: bold;
        }
        .placeholder-row td {
            background-color: #3d0d0d !important;
            border-color: #ff4444;
        }
        .placeholder-row:hover td {
            background-color: #4d1111 !important;
            box-shadow: 0 0 15px rgba(255, 68, 68, 0.2);
        }
        .placeholder-row .lang-col {
            color: #ff4444;
        }
        .suspect td {
            /* border-top: 1px dashed #ffcc00; */
            /* border-bottom: 1px dashed #ffcc00; */
        }
        .suspect .lang-col {
            color: #ffcc00;
        }
        .suspect:hover td {
            box-shadow: 0 0 15px rgba(255, 204, 0, 0.2);
        }
        
        .imposter {
            color: #ff4444;
            font-weight: bold;
            text-decoration: underline;
            text-decoration-style: wavy;
        }
        
        /* Fastest/Slowest Highlights */
        .fastest-row td {
            border-top: 1px solid var(--primary);
            border-bottom: 1px solid var(--primary);
            background: rgba(0, 255, 157, 0.05);
        }
        .fastest-row .lang-col::after {
            content: " 👑";
            font-size: 0.8em;
        }
        .slowest-row td {
            border-top: 1px solid var(--danger);
            border-bottom: 1px solid var(--danger);
            background: rgba(255, 68, 68, 0.05);
        }
        .slowest-row .lang-col::after {
            content: " 🐢";
            font-size: 0.8em;
        }
        
        /* Tooltip */
        #tooltip {
            position: fixed;
            display: none;
            background: rgba(13, 13, 18, 0.95);
            border: 1px solid var(--primary);
            padding: 15px;
            color: var(--primary);
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            max-width: 300px;
            z-index: 1000;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.2);
            pointer-events: none;
            backdrop-filter: blur(5px);
        }
        #tooltip::before {
            content: ">> SYSTEM MSG";
            display: block;
            font-size: 0.7em;
            color: var(--muted);
            margin-bottom: 5px;
            border-bottom: 1px solid var(--border);
            padding-bottom: 3px;
        }

        /* Controls */
        .controls {
            margin-bottom: 20px;
            display: flex;
            gap: 10px;
            justify-content: flex-start;
            flex-wrap: wrap;
            position: relative;
        }
        .manual-tag, .ai-tag {
            font-size: 0.75em;
            color: var(--muted);
            font-weight: normal;
            margin-left: 4px;
        }
        .filter-active-red {
            background-color: #ff4444 !important;
            color: #fff !important;
            border-color: #ff4444 !important;
            box-shadow: 0 0 10px rgba(255, 68, 68, 0.4);
        }
        .btn-red {
            background: rgba(255, 68, 68, 0.2);
            border-color: #ff4444;
            color: #ff4444;
        }
        .btn-red.active {
            background: #ff4444;
            color: #fff;
            box-shadow: 0 0 10px rgba(255, 68, 68, 0.4);
        }
        .btn {
            background: var(--surface);
            border: 1px solid var(--primary);
            color: var(--primary);
            padding: 8px 16px;
            font-family: 'JetBrains Mono', monospace;
            cursor: pointer;
            transition: all 0.2s;
            text-transform: uppercase;
            font-size: 0.8em;
        }
        .btn:hover {
            background: var(--primary);
            color: var(--bg);
            box-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
        }
        .btn.active {
            background: var(--primary);
            color: var(--bg);
        }

        /* Modal */
        .matrix-canvas {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            z-index: 1000;
            pointer-events: none; /* Let clicks pass through initially */
        }
        
        /* Red Pill Slide-In Animation */
        .matrix-slide-enter {
            top: -100vh !important;
            left: 0 !important;
            transition: top 1.5s ease-in-out;
        }
        .matrix-slide-active {
            top: 0 !important;
            left: 0 !important;
        }
        
        /* Content Slide-Out Animation */
        .content-slide-exit {
            transition: transform 1.5s ease-in-out, opacity 1.5s ease-in-out;
        }
        .content-slide-active {
            transform: translateY(100vh);
            opacity: 0.3;
        }

        /* Blue Pill Slide-Down Animation */
        .slide-down-slow {
            animation: slideDown 3s ease-out forwards;
        }
        @keyframes slideDown {
            from { top: -100%; }
            to { top: 0; }
        }
        .modal-overlay {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.8);
            z-index: 2000;
            backdrop-filter: blur(5px);
            justify-content: center;
            align-items: center;
        }
        .modal-content {
            background: var(--surface);
            border: 1px solid var(--primary);
            padding: 30px;
            border-radius: 8px;
            max-width: 500px;
            width: 90%;
            position: relative;
            box-shadow: 0 0 30px rgba(0, 255, 157, 0.2);
            text-align: center;
        }
        .modal-close {
            position: absolute;
            top: 10px;
            right: 15px;
            color: var(--muted);
            cursor: pointer;
            font-size: 1.5em;
        }
        .modal-close:hover {
            color: var(--primary);
        }
        .modal-image {
            width: 150px;
            height: 150px;
            border-radius: 50%;
            object-fit: cover;
            border: 3px solid var(--primary);
            margin-bottom: 20px;
            box-shadow: 0 0 20px rgba(0, 255, 157, 0.3);
        }
        .modal-title {
            font-size: 2em;
            color: var(--primary);
            margin-bottom: 10px;
        }
        .modal-subtitle {
            font-size: 0.9em;
            color: var(--secondary);
            margin-bottom: 20px;
            text-transform: uppercase;
            letter-spacing: 1px;
        }
        .modal-desc {
            color: var(--text);
            line-height: 1.6;
            margin-bottom: 20px;
        }
        .lang-col {
            cursor: pointer;
            text-decoration: underline;
            text-decoration-style: dotted;
            text-decoration-color: var(--muted);
        }
        .lang-col:hover {
            color: #fff;
            text-decoration-color: var(--primary);
        }
        
        /* Matrix Sort Buttons */
        .sort-group {
            display: flex;
            gap: 2px;
            margin-top: 5px;
            justify-content: center;
        }
        .sort-btn {
            background: #2a2a35;
            border: none;
            color: #888;
            font-size: 0.7em;
            padding: 2px 6px;
            cursor: pointer;
            border-radius: 3px;
            font-family: 'JetBrains Mono', monospace;
        }
        .sort-btn:hover {
            background: var(--primary);
            color: var(--bg);
        }
        
        /* Rotation for Descending Sort */
        .rotate-180 {
            transform: rotate(180deg);
            display: inline-block;
        }
        .sort-btn, .btn {
            transition: transform 0.3s, background 0.2s, color 0.2s;
        }
        
        .top-bar {
            display: flex;
            justify-content: flex-start;
            align-items: center;
            gap: 20px;
            margin-bottom: 20px;
            flex-wrap: wrap;
            position: relative;
        }
        .solver-counter {
            font-size: 2.5em;
            font-weight: bold;
            color: var(--primary);
            text-shadow: 0 0 15px rgba(0, 255, 157, 0.4);
            font-family: 'JetBrains Mono', monospace;
            /* Positioned in header now */
        }
        
        /* Fullscreen overlay for solver counter - matches main page header */
        #fullscreen-header {
            display: none;
            position: fixed;
            top: 40px;
            left: 0;
            right: 0;
            z-index: 10000;
            pointer-events: none;
            padding: 0 40px;
        }
        #fullscreen-header .header-counters {
            display: flex;
            justify-content: flex-start;
            align-items: center;
            gap: 20px;
            flex-wrap: wrap;
        }
        .mismatch-counter {
            font-size: 1.2em;
            font-weight: bold;
            color: #ff4444;
            text-shadow: 0 0 10px rgba(255, 68, 68, 0.4);
            font-family: 'JetBrains Mono', monospace;
            margin-right: 20px;
        }
        .header-counters {
            position: absolute;
            top: 20px;
            right: 20px;
            display: flex;
            flex-direction: column;
            align-items: flex-end;
            z-index: 100;
            background: rgba(13, 13, 18, 0.8);
            padding: 10px 20px;
            border: 1px solid var(--primary);
            border-radius: 4px;
        }
        .personality-intro {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            color: var(--secondary);
            max-width: 400px;
            text-align: right;
            border-right: 2px solid var(--border);
            padding-right: 20px;
            line-height: 1.4;
            font-style: italic;
        }
        
        /* Score Styling */
        .score-fast {
            color: var(--primary);
            font-weight: bold;
        }
        .score-slow {
            color: var(--danger);
        }
        .total-score {
            font-size: 1.2em;
            font-weight: bold;
            color: var(--primary);
            text-align: right;
        }

        
        /* Toggle Button */
        .toggle-btn {
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .toggle-icon {
            width: 12px;
            height: 12px;
            border-radius: 50%;
            background: #333;
            transition: background 0.2s;
        }
        .btn.active .toggle-icon {
            background: var(--primary);
            box-shadow: 0 0 5px var(--primary);
        }
        
        /* Iteration Mismatch Highlight */
        .mismatch-iterations td {
            background-color: rgba(255, 165, 0, 0.3) !important;
        }
        .modal-location, .modal-benefits {
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            color: var(--secondary);
            margin-top: 10px;
            line-height: 1.4;
        }
        .modal-benefits {
            color: var(--primary);
            font-style: italic;
        }
        
        /* Matrix Screensaver */
        #matrix-canvas {
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            z-index: 10;
            display: none; /* Hidden by default */
            border-radius: 8px;
            pointer-events: none; /* Let clicks pass through if needed, though we hide chart anyway */
        }
        
        #d3-chart-container {
            position: relative;
        }

        /* Vertical Slide Animations */
        @keyframes slideDownEnter {
            from { top: -100vh; }
            to { top: 0; }
        }

        @keyframes slideDownExit {
            from { transform: translateY(0); opacity: 1; }
            to { transform: translateY(100vh); opacity: 0; }
        }

        .slide-down-enter {
            animation: slideDownEnter 1.5s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }

        .slide-down-exit {
            animation: slideDownExit 1.5s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }
    </style>
    <style>
        /* Pill UI */
        .pill-container {
            display: flex;
            align-items: center;
        }

        .pill-combined {
            display: flex;
            width: 100px;
            height: 32px;
            border-radius: 16px;
            overflow: hidden;
            border: 1px solid rgba(255, 255, 255, 0.2);
            backdrop-filter: blur(4px);
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
            transition: transform 0.2s ease;
        }

        .pill-combined:hover {
            transform: scale(1.05);
        }

        .pill-half {
            flex: 1;
            height: 100%;
            cursor: pointer;
            transition: background 0.2s ease, box-shadow 0.2s ease;
            position: relative;
        }

        .pill-half.blue {
            background: linear-gradient(135deg, rgba(50, 50, 255, 0.4), rgba(0, 0, 180, 0.6));
            box-shadow: inset 2px 2px 4px rgba(255, 255, 255, 0.2);
        }

        .pill-half.blue:hover {
            background: linear-gradient(135deg, rgba(80, 80, 255, 0.6), rgba(20, 20, 200, 0.8));
            box-shadow: inset 2px 2px 6px rgba(255, 255, 255, 0.4);
        }

        .pill-half.red {
            background: linear-gradient(135deg, rgba(255, 50, 50, 0.4), rgba(180, 0, 0, 0.6));
            box-shadow: inset -2px -2px 4px rgba(0, 0, 0, 0.2); /* Subtle shadow on other side */
        }

        .pill-half.red:hover {
            background: linear-gradient(135deg, rgba(255, 80, 80, 0.6), rgba(200, 20, 20, 0.8));
            box-shadow: inset 2px 2px 6px rgba(255, 255, 255, 0.4);
        }

        /* Matrix Screensaver */
        #matrix-canvas {
            position: fixed; /* Changed to fixed for full screen overlay */
            top: 0;
            left: 0;
            width: 100vw;
            height: 100vh;
            z-index: 9999; /* Very high z-index */
            display: none; /* Hidden by default */
            pointer-events: auto; /* Capture clicks to exit */
        }

        /* Animations */
        @keyframes slideDownEnter {
            from { top: -100vh; }
            to { top: 0; }
        }

        @keyframes slideDownExit {
            from { transform: translateY(0); opacity: 1; }
            to { transform: translateY(100vh); opacity: 0; }
        }

        @keyframes slideUpEnter {
            from { transform: translateY(100vh); opacity: 0; }
            to { transform: translateY(0); opacity: 1; }
        }

        .slide-down-enter {
            animation: slideDownEnter 1.5s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }

        .slide-down-exit {
            animation: slideDownExit 1.5s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }
        
        .slide-up-enter {
             animation: slideUpEnter 1.0s cubic-bezier(0.25, 1, 0.5, 1) forwards;
        }

        /* Main content wrapper for sliding */
        .content-wrapper {
            transition: transform 1.5s cubic-bezier(0.25, 1, 0.5, 1), opacity 1.5s ease;
        }

        /* Cell Popup Menu */
        .popup-menu {
            display: none;
            position: absolute;
            background: var(--surface);
            border: 1px solid var(--primary);
            box-shadow: 0 0 15px rgba(0, 255, 157, 0.2);
            z-index: 10000;
            min-width: 150px;
            border-radius: 4px;
            padding: 5px 0;
        }
        .popup-item {
            padding: 8px 15px;
            cursor: pointer;
            color: var(--text);
            font-size: 0.9em;
            transition: background 0.2s;
        }
        .popup-item:hover {
            background: rgba(0, 255, 157, 0.1);
            color: var(--primary);
        }
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <canvas id="matrix-canvas"></canvas>
    <div id="tooltip"></div>
    
    <!-- Modal -->
    <div id="langModal" class="modal-overlay" onclick="closeModal(event)">
        <div class="modal-content">
            <span class="modal-close" onclick="closeModal(event)">&times;</span>
            <img id="modalImg" class="modal-image" src="" alt="Creator">
            <div id="modalTitle" class="modal-title">Language</div>
            <div id="modalSubtitle" class="modal-subtitle">Creator • Date</div>
            <div id="modalLocation" class="modal-location">Location</div>
            <div id="modalBenefits" class="modal-benefits">Benefits</div>
            <div id="modalDesc" class="modal-desc">Description goes here.</div>
        </div>
    </div>

    <!-- Methodology Modal -->
    <div id="methodModal" class="modal-overlay" onclick="closeMethodology(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeMethodology(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Scoring Methodology</div>
            <div class="modal-desc">
                <p>The <strong>Composite Score</strong> is a normalized metric designed to compare performance against the C baseline across multiple dimensions.</p>
                
                <h3 style="color: var(--secondary);">The Baseline: C</h3>
                <p>The <strong>C</strong> implementation is the reference standard (1.0) for Time, Memory, and CPU usage.</p>
                
                <h3 style="color: var(--secondary);">The Formula</h3>
                <div style="background: #000; padding: 10px; border-radius: 4px; text-align: center; font-family: monospace; margin: 10px 0; border: 1px solid var(--border);">
                    Score = (3.0 &times; CpuRatio + 2.0 &times; MemRatio + 1.0 &times; TimeRatio) / 6.0
                </div>
                <p style="font-size: 0.9em; text-align: center; color: var(--muted);">
                    Where Ratio = (Solver Value) / (C Value)<br>
                    Weights: CPU (3), Memory (2), Time (1)
                </p>
                
                <h3 style="color: var(--secondary);">Interpretation</h3>
                <ul style="list-style: none; padding: 0;">
                    <li style="margin-bottom: 8px;"><strong style="color: var(--primary);">1.0</strong> : Parity. Matches C's performance profile.</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #ff0055;">&gt; 1.0</strong> : Less Efficient. Higher resource usage than C.</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #00b8ff;">&lt; 1.0</strong> : More Efficient. Lower resource usage than C.</li>
                </ul>
                <p style="font-size: 0.9em; color: var(--muted); text-align: center; margin-top: 20px;"><em>Lower scores are better.</em></p>
            </div>
        </div>
    </div>

    <!-- Fullscreen header overlay - matches main page header -->
    <div id="fullscreen-header">
        <div class="header-counters">
            <div class="solver-counter">SOLVER ${metrics.length} OF ${metrics.length}</div>
            ${mismatchCount > 0 ? `<div class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>` : ''}
        </div>
    </div>

    <div id="main-content" class="content-wrapper">
    <h1>Sudoku Benchmark Results</h1>
    
    <div class="header-counters">
        <div class="solver-counter">SOLVER ${metrics.length} OF ${metrics.length}</div>
        ${mismatchCount > 0 ? `<div id="header-mismatch-count" class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>` : ''}
        <div id="screensaver-pill" style="display: none; font-size: 0.8em; color: var(--secondary); margin-top: 5px; align-self: flex-start;">
            <span style="display: inline-block; width: 8px; height: 8px; background: #00ff9d; border-radius: 50%; margin-right: 5px; box-shadow: 0 0 5px #00ff9d;"></span>
            Screensaver Active
        </div>
    </div>
    
    <div style="width: 95%; margin: 0 auto 40px auto; background: #16161e; padding: 20px; border-radius: 8px; border: 1px solid #2a2a35; height: 500px; position: relative;">
        <div style="position: absolute; top: 10px; right: 20px; z-index: 100; display: flex; gap: 10px;">
            <button class="btn active" id="btn-chart-line" onclick="switchChart('line')">Line Chart</button>
            <button class="btn" id="btn-chart-jockey" onclick="switchChart('jockey')">Jockey Leaderboard</button>
            <div class="pill-container" onclick="event.stopPropagation();">
                <div class="pill-combined">
                    <div class="pill-half blue" title="Take the Blue Pill (Slide Effect)" onclick="event.stopPropagation(); window.startScreensaver('blue')"></div>
                    <div class="pill-half red" title="Take the Red Pill (Instant Matrix)" onclick="event.stopPropagation(); window.startScreensaver('red')"></div>
                </div>
            </div>
        </div>
        <div id="d3-chart-container" style="width: 100%; height: 100%;"></div>

    </div>
    
    <div class="top-bar">
        <!-- Removed solver-counter from here -->
        <div id="personality-intro" class="personality-intro">
            Welcome to the Sudoku Benchmark. Click on any language name for creator details. Use the controls to sort data and analyze performance metrics across different languages.
        </div>
        <div class="controls">
            <input type="text" id="search-input" class="btn" placeholder="Filter Language..." onkeyup="filterLanguages()" style="cursor: text; width: 150px;">
            <select id="personality-selector" class="btn" onchange="changePersonality()">
                <option value="Standard">Standard</option>
                <option value="Neuromancer">Neuromancer</option>
                <option value="The Jockey">The Jockey</option>
                <option value="The Professor">The Professor</option>
                <option value="The Surfer">The Surfer</option>
            </select>
            <button class="btn" onclick="sortRows('lang', this)">Name</button>
            <button class="btn active" onclick="sortRows('time', this)">Time (Fastest)</button>
            <button class="btn" onclick="sortRows('mem', this)">Memory (Highest)</button>
            <button class="btn" onclick="sortRows('iters', this)">Iterations</button>
            <button class="btn" onclick="sortRows('score', this)">Total Score</button>
            <button class="btn" id="toggleMismatchesBtn" onclick="toggleMismatches()">
                <span>Filter Mismatches</span>
            </button>
            <button class="btn" onclick="showMethodology()">Methodology</button>
        </div>
    </div>

    <!-- SVG Filters for Logo Tailoring -->
    <svg style="display: none;">
        <defs>
            <filter id="filter-invert">
                <feColorMatrix in="SourceGraphic" type="matrix" values="-1 0 0 0 1 
                                                                          0 -1 0 0 1 
                                                                          0 0 -1 0 1 
                                                                          0 0 0 1 0"/>
            </filter>
            <filter id="filter-transparent-white">
                <feColorMatrix in="SourceGraphic" type="matrix" values="1 0 0 0 0
                                                                          0 1 0 0 0
                                                                          0 0 1 0 0
                                                                          -1 -1 -1 1 0"/>
            </filter>
        </defs>
    </svg>

    <div class="container">
        <table>
            <thead>
                <tr>
                    <th>
                        Language
                        <div class="sort-group">
                            <button class="sort-btn" onclick="sortRows('lang', this)" title="Sort by Name">N</button>
                            <button class="sort-btn" onclick="sortRows('year', this)" title="Sort by Year">Y</button>
                        </div>
                    </th>
                    <th>
                        Score
                        <div class="sort-group">
                            <button class="sort-btn" onclick="sortRows('score', this)" title="Sort by Score">S</button>
                        </div>
                    </th>
`;

    for (let i = 0; i < maxMatrices; i++) {
        html += `<th>
            Matrix ${i + 1}
            <div class="sort-group">
                <button class="sort-btn" onclick="sortMatrix(${i}, 'time', this)" title="Sort by Time">S</button>
                <button class="sort-btn" onclick="sortMatrix(${i}, 'iters', this)" title="Sort by Iterations">I</button>
                <button class="sort-btn" onclick="sortMatrix(${i}, 'mem', this)" title="Sort by Memory">M</button>
                <button class="sort-btn" onclick="sortMatrix(${i}, 'score', this)" title="Sort by Score">Sc</button>
            </div>
        </th>`;
    }

    html += `<th>
        <span>Total Time</span>
        <div class="sort-group">
            <button class="sort-btn" onclick="sortRows('time', this)" title="Sort by Total Time">S</button>
        </div>
    </th>
    </tr></thead><tbody>`;

    for (const m of sortedMetrics) {
        const lang = m.solver;
        const times = m.results.map(r => r.time);
        const iters = m.results.map(r => r.iterations);
        const mems = m.results.map(r => r.memory);

        const totalTime = times.reduce((a, b) => a + b, 0);
        const totalIters = iters.reduce((a, b) => a + b, 0);
        const maxMem = mems.length > 0 ? Math.max(...mems) : 0;

        // Efficiency Score: Memory (MB) / Seconds
        const memMbTotal = maxMem / 1024 / 1024;
        const efficiencyScore = totalTime > 0 ? memMbTotal / totalTime : 0;

        // Composite Score (vs C)
        // Score = (3*CpuRatio + 2*MemRatio + 1*TimeRatio) / 6

        const totalCpu = m.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0);

        const timeRatio = (cTotalTime > 0) ? (totalTime / cTotalTime) : 0;
        const memRatio = (cTotalMem > 0) ? (maxMem / cTotalMem) : 0;
        const cpuRatio = (cTotalCpu > 0) ? (totalCpu / cTotalCpu) : 0;

        const normalizedScore = (3.0 * cpuRatio + 2.0 * memRatio + 1.0 * timeRatio) / 6.0;

        // Find min/max for highlighting
        const minTime = Math.min(...sortedMetrics.map(m => m.results.reduce((a, b) => a + b.time, 0)));
        const maxTime = Math.max(...sortedMetrics.map(m => m.results.reduce((a, b) => a + b.time, 0)));

        const isFastest = totalTime === minTime;
        const isSlowest = totalTime === maxTime;

        // Suspect Logic
        const isSuspect = m.results.length !== maxMatrices;

        // Iteration Mismatch Logic
        const cTotalIters = cMetrics ? cMetrics.results.reduce((a, b) => a + b.iterations, 0) : 0;
        // Baseline is C (Local)
        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
        const isMismatch = !isBaseline && cTotalIters > 0 && totalIters !== cTotalIters;

        let rowClass = "";
        if (isSuspect) rowClass += " suspect";
        if (isMismatch) rowClass += " mismatch-iterations";

        // Quote
        const baseLang = lang; // lang is already clean now
        const runType = m.runType || 'Local';
        const quote = (personalities['Standard'] as any)[baseLang] || (personalities['Standard'] as any)[lang] || "A mystery wrapped in code.";
        const safeQuote = quote.replace(/'/g, "&apos;") + ` Efficiency: ${efficiencyScore.toFixed(2)} MB/s`;

        // Metadata
        const meta = languageMetadata[baseLang] || languageMetadata[lang] || {};
        const year = meta.date || "0000";
        const displayNameRaw = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
        let displayName = displayNameRaw;

        let typeIcon = '';
        if (runType === 'Docker') {
            typeIcon = '<span title="Docker Container" style="margin-left:5px; font-size: 0.8em;">🐳</span>';
        } else if (runType === 'AI') {
            typeIcon = '<span class="ai-tag" title="AI Generated">(AI)</span>';
        } else {
            typeIcon = '<span title="Local Run" style="margin-left:5px; font-size: 0.8em;">💻</span>';
        }

        const historyText = (languageHistories[baseLang] || languageHistories[lang] || "Unknown.").replace(/'/g, "&apos;");

        const localLogo = logoMap.get(baseLang.toLowerCase());
        const logoUrl = localLogo || meta.logo || meta.image;

        // Data Attributes
        let matrixDataAttrs = "";
        for (let i = 0; i < maxMatrices; i++) {
            const r = m.results[i];
            const t = r ? r.time : 999999;
            const it = r ? r.iterations : -1;
            const mem = r ? r.memory : -1;

            let score = 0;
            if (r && cTimes[i] && cTimes[i] > 0) {
                score = t / cTimes[i];
            }

            matrixDataAttrs += ` data-m${i}-time='${t}' data-m${i}-iters='${it}' data-m${i}-mem='${mem}' data-m${i}-score='${score.toFixed(2)}'`;
        }

        html += `<tr class="${rowClass} ${isFastest ? 'fastest-row' : ''} ${isSlowest ? 'slowest-row' : ''}" 
            data-lang="${lang}" 
            data-year="${year}" 
            data-time="${totalTime.toFixed(6)}" 
            data-iters="${totalIters}" 
            data-mem="${maxMem}" 
            data-score="${normalizedScore.toFixed(2)}"
            data-score-breakdown="Time: ${timeRatio.toFixed(2)}x | Mem: ${memRatio.toFixed(2)}x | CPU: ${cpuRatio.toFixed(2)}x"
            data-quote="${quote}" data-history='${historyText}' ${matrixDataAttrs}>
            <td class='lang-col'>
                ${logoUrl ? `<img src="${logoUrl}" alt="${displayNameRaw}" class="lang-logo">` : ''}
                <div style="display: inline-block; vertical-align: middle;">
                    <div>${displayName}${typeIcon}</div>
                    <div class='lang-year'>${year}</div>
                </div>
            </td>
            <td class="score-col">
                <div class="total-score" style="text-align: center; color: ${normalizedScore <= 1.0 ? 'var(--primary)' : '#ff0055'};">
                    ${normalizedScore.toFixed(2)}
                </div>
            </td>`;

        for (let i = 0; i < maxMatrices; i++) {
            const r = m.results[i];
            if (r) {
                const memMb = r.memory / 1024 / 1024;

                let scoreDisplay = "";
                if (cTimes[i] && cTimes[i] > 0) {
                    const scoreVal = r.time / cTimes[i];
                    scoreDisplay = `${scoreVal.toFixed(2)}x`;
                }

                html += `<td class="matrix-cell" data-matrix-index="${i}">
                    <div class="cell-content">

                        <div class="time" title="Wall Clock Time">${r.time.toFixed(5)}s</div>
                        <div class="meta">
                            ${(() => {
                        const cRes = cMetrics?.results.find(res => res.matrix === r.matrix);
                        const cIterations = cRes ? cRes.iterations : null;
                        // Ensure strict number comparison and handle potential type issues if JSON parsing was loose
                        const rIter = Number(r.iterations);
                        const cIter = Number(cIterations);
                        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
                        const isImposter = !isBaseline && cIterations !== null && rIter !== cIter;

                        return `<span title="Iterations: ${rIter} vs C: ${cIter}" class="${isImposter ? 'imposter' : ''}">#${r.iterations}</span>`;
                    })()}
                            <span title="Memory">${memMb.toFixed(1)}M</span>
                        </div>
                    </div>
                </td>`;
            } else {
                html += `<td class="matrix-cell" data-matrix-index="${i}"><span style='color: #333'>-</span></td>`;
            }
        }

        html += `<td class='total-time'><div style='display:flex;flex-direction:column;align-items:flex-end;'><div>${totalTime.toFixed(4)}s</div><div style='font-size:0.6em;color:#5c5c66;'>${totalIters.toLocaleString()} iters</div></div></td></tr>`;
    }

    html += `
        </tbody></table></div>
        <script>
            const personalities = ${JSON.stringify(personalities)};
            const languageMetadata = ${JSON.stringify(languageMetadata)};
            const methodologyTexts = ${JSON.stringify(methodologyTexts)};
            
            let currentSort = { metric: 'time', dir: 1 }; // 1 = Asc, -1 = Desc

            // Search Logic
            function filterLanguages() {
                const input = document.getElementById('search-input');
                const filter = input.value.toUpperCase();
                const tbody = document.querySelector('tbody');
                const rows = tbody.getElementsByTagName('tr');

                for (let i = 0; i < rows.length; i++) {
                    const row = rows[i];
                    const lang = row.getAttribute('data-lang');
                    if (lang) {
                        if (lang.toUpperCase().indexOf(filter) > -1) {
                            row.style.display = "";
                        } else {
                            row.style.display = "none";
                        }
                    }
                }
            }

            // Sorting Logic
            function sortRows(metric, btn) {
                const tbody = document.querySelector('tbody');
                const rows = Array.from(tbody.querySelectorAll('tr'));
                
                // Toggle direction
                if (currentSort.metric === metric) {
                    currentSort.dir *= -1;
                } else {
                    currentSort.metric = metric;
                    currentSort.dir = 1;
                }
                
                // Update buttons
                document.querySelectorAll('.btn, .sort-btn').forEach(b => {
                    b.classList.remove('active');
                    b.classList.remove('rotate-180');
                });
                
                if (btn) {
                    btn.classList.add('active');
                    if (currentSort.dir === -1) {
                        btn.classList.add('rotate-180');
                    }
                }

                rows.sort((a, b) => {
                    const aVal = a.getAttribute('data-' + metric);
                    const bVal = b.getAttribute('data-' + metric);
                    
                    if (metric === 'lang') {
                        return aVal.localeCompare(bVal) * currentSort.dir;
                    } else if (metric === 'year') {
                        return (parseInt(aVal) - parseInt(bVal)) * currentSort.dir;
                    } else {
                        return (parseFloat(aVal) - parseFloat(bVal)) * currentSort.dir;
                    }
                });

                rows.forEach(row => tbody.appendChild(row));
            }
            
            function sortMatrix(index, metric, btn) {
                const tbody = document.querySelector('tbody');
                const rows = Array.from(tbody.querySelectorAll('tr'));
                const attr = 'data-m' + index + '-' + metric;
                const fullMetric = 'm' + index + '_' + metric;
                
                if (currentSort.metric === fullMetric) {
                    currentSort.dir *= -1;
                } else {
                    currentSort.metric = fullMetric;
                    currentSort.dir = metric === 'time' || metric === 'score' ? 1 : -1;
                }

                // Update buttons
                document.querySelectorAll('.btn, .sort-btn').forEach(b => {
                    b.classList.remove('active');
                    b.classList.remove('rotate-180');
                });
                
                if (btn) {
                    btn.classList.add('active');
                    if (currentSort.dir === -1) {
                        btn.classList.add('rotate-180');
                    }
                }

                rows.sort((a, b) => {
                    const aVal = parseFloat(a.getAttribute(attr));
                    const bVal = parseFloat(b.getAttribute(attr));
                    return (aVal - bVal) * currentSort.dir;
                });

                rows.forEach(row => tbody.appendChild(row));
            }
            
            // Mismatch Filter
            function toggleMismatches() {
                const btn = document.getElementById('toggleMismatchesBtn');
                const isFilterActive = btn.classList.toggle('active');

                if (isFilterActive) {
                    btn.classList.add('filter-active-red');
                    btn.querySelector('span').textContent = "Show All";
                } else {
                    btn.classList.remove('filter-active-red');
                    btn.querySelector('span').textContent = "Filter Mismatches";
                }

                const rows = document.querySelectorAll('tbody tr');
                rows.forEach(row => {
                    if (isFilterActive) {
                        if (row.classList.contains('mismatch-iterations')) {
                            row.style.display = 'none';
                        } else {
                            row.style.display = '';
                        }
                    } else {
                        row.style.display = '';
                    }
                });
            }

            // Personality Selector
            function changePersonality() {
                const selector = document.getElementById('personality-selector');
                const persona = selector.value;
                const intro = document.getElementById('personality-intro');
                
                // Update Intro Text
                if (persona === "Neuromancer") intro.innerText = "The sky above the port was the color of television, tuned to a dead channel.";
                else if (persona === "The Jockey") intro.innerText = "And they're off! The race is on!";
                else if (persona === "The Professor") intro.innerText = "Let us analyze the computational complexity.";
                else if (persona === "The Surfer") intro.innerText = "Catch the wave of data, dude.";
                else intro.innerText = "Welcome to the Sudoku Benchmark. Click on any language name for creator details.";

                // Update Tooltips
                const rows = document.querySelectorAll('tbody tr');
                rows.forEach(row => {
                    const lang = row.getAttribute('data-lang');
                    const quotes = personalities[persona] || personalities['Standard'];
                    let quote = quotes[lang] || quotes['default'] || "Unknown.";
                    
                    // Append Efficiency
                    const score = row.getAttribute('data-score');
                    quote += " Efficiency: " + parseFloat(score).toFixed(2) + " MB/s";
                    
                    row.setAttribute('data-quote', quote);
                });
                // Update Methodology Modal
                const methodDesc = document.querySelector('#methodModal .modal-desc');
                if (methodDesc) {
                    methodDesc.innerHTML = methodologyTexts[persona] || methodologyTexts['Standard'];
                }

            }

            // Tooltip Logic
            const tooltip = document.getElementById('tooltip');
            
            // Attach to all cells
            // Attach to all cells
            // Attach to all cells
            
            document.querySelectorAll('tbody td').forEach(cell => {
                cell.addEventListener('mousemove', (e) => {
                    const row = cell.parentElement;
                    const lang = row.getAttribute('data-lang');
                    
                    let content = "";
                    
                    if (cell.classList.contains('lang-col')) {
                        // Language Cell -> Show History
                        const history = row.getAttribute('data-history');
                        if (history) {
                            content = '<strong style="color: var(--primary)">' + lang + '</strong><br>' +
                                '<div style="max-width: 250px; white-space: normal; margin-top: 5px;">' + history + '</div>';
                        }
                    } else if (cell.classList.contains('matrix-cell')) {
                        // Matrix Cell -> Detailed Metrics
                        const matrixIdx = parseInt(cell.getAttribute('data-matrix-index'));
                        const time = row.getAttribute('data-m' + matrixIdx + '-time');
                        const iters = row.getAttribute('data-m' + matrixIdx + '-iters');
                        const mem = row.getAttribute('data-m' + matrixIdx + '-mem');
                        const score = row.getAttribute('data-m' + matrixIdx + '-score');
                        
                        if (time && time !== '999999') {
                            const memMb = (parseFloat(mem) / 1024 / 1024).toFixed(1);
                            content = '<strong style="color: var(--primary)">Matrix ' + (matrixIdx + 1) + '</strong><br>' +
                                '<span style="color: var(--secondary)">' + lang + '</span><br>' +
                                '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                                'Time: <span style="color: #fff">' + parseFloat(time).toFixed(5) + 's</span><br>' +
                                'Score: <span style="color: ' + (parseFloat(score) <= 1 ? 'var(--primary)' : '#ff0055') + '">' + score + 'x</span><br>' +
                                'Iters: ' + parseInt(iters).toLocaleString() + '<br>' +
                                'Mem: ' + memMb + ' MB';
                        } else {
                            content = row.getAttribute('data-quote');
                        }
                    } else if (cell.classList.contains('score-col')) {
                        // Score Cell -> Show Breakdown + Quote
                        const breakdown = row.getAttribute('data-score-breakdown');
                        const quote = row.getAttribute('data-quote');
                        content = '<strong style="color: var(--primary)">Composite Score</strong><br>' +
                                  '<span style="font-size: 0.8em; color: var(--secondary)">' + breakdown + '</span><br>' +
                                  '<hr style="border: 0; border-bottom: 1px solid var(--border); margin: 5px 0;">' +
                                  quote;
                    }

                    if (content) {
                        const tooltip = document.getElementById('tooltip');
                        tooltip.style.display = 'block';
                        tooltip.style.left = (e.clientX + 15) + 'px';
                        tooltip.style.top = (e.clientY + 15) + 'px';
                        tooltip.innerHTML = content;
                    }
                });
                
                cell.addEventListener('mouseleave', () => {
                    const tooltip = document.getElementById('tooltip');
                    tooltip.style.display = 'none';
                });
            });
            
            // Add click handler to language cells to show modal
            document.querySelectorAll('.lang-col').forEach(cell => {
                cell.addEventListener('click', () => {
                    const row = cell.parentElement;
                    const lang = row.getAttribute('data-lang');
                    if (lang) {
                        showLanguageDetails(lang);
                    }
                });
                cell.style.cursor = 'pointer';  // Make it obvious it's clickable
            });
            // Modal Logic
            function showLanguageDetails(lang) {
                const modal = document.getElementById('langModal');
                const meta = languageMetadata[lang];
                if (!meta) return;

                document.getElementById('modalImg').src = meta.image;
                const displayName = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
                document.getElementById('modalTitle').innerText = displayName;
                document.getElementById('modalSubtitle').innerText = meta.creator + " • " + meta.date;
                document.getElementById('modalLocation').innerText = "📍 " + (meta.location || "Unknown Location");
                document.getElementById('modalBenefits').innerText = "✨ " + (meta.benefits || "Unknown Benefits");
                document.getElementById('modalDesc').innerText = meta.description;
                
                modal.style.display = 'flex';
            }

            function closeModal(event) {
                if (event.target.id === 'langModal' || event.target.classList.contains('modal-close')) {
                    document.getElementById('langModal').style.display = 'none';
                }
            }

            function showMethodology() {
                document.getElementById('methodModal').style.display = 'flex';
            }

            function closeMethodology(event) {
                if (event.target.id === 'methodModal' || event.target.classList.contains('modal-close')) {
                    document.getElementById('methodModal').style.display = 'none';
                }
            }

            // Initialize
            toggleMismatches(); // Default hide
            
            // --- D3.js Chart Implementation ---
            (function() {
                // Inject metrics with logo data
                const historyData = ${JSON.stringify(history)};
                const referenceOutputs = ${JSON.stringify(referenceOutputs)};
                const tailoring = ${JSON.stringify(tailoringConfig)};
                const metricsData = ${JSON.stringify(metrics.map(m => {
        const baseLang = m.solver.replace(/ \((Manual|AI)\)$/, '');
        const localLogo = logoMap.get(baseLang.toLowerCase());
        const meta = languageMetadata[baseLang] || languageMetadata[m.solver] || {};
        return {
            ...m,
            logo: localLogo || meta.logo || meta.image || "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Alchemist_symbol_for_process_2.svg/120px-Alchemist_symbol_for_process_2.svg.png"
        };
    }))};
                
                let data = metricsData;
                
                let currentChart = 'line';

                // Expose switchChart globally
                // Expose switchChart globally - DEFINED EARLY TO PREVENT REFERENCE ERRORS
                window.switchChart = function(type) {
                    try {
                        currentChart = type;
                        const btnLine = document.getElementById('btn-chart-line');
                        const btnJockey = document.getElementById('btn-chart-jockey');
                        if (btnLine) btnLine.classList.toggle('active', type === 'line');
                        if (btnJockey) btnJockey.classList.toggle('active', type === 'jockey');
                        
                        // Check if D3 is loaded
                        if (typeof d3 === 'undefined') {
                            throw new Error("D3.js library not loaded. Please check your internet connection.");
                        }

                        const container = d3.select("#d3-chart-container");
                        container.selectAll("*").remove();
                        
                        if (type === 'line') {
                            drawLineChart();
                        } else {
                            drawJockeyChart();
                        }
                    } catch (e) {
                        console.error("Error switching chart:", e);
                        const container = document.getElementById('d3-chart-container');
                        if (container) {
                            container.innerHTML = "<div style='color:#ff4444; padding:20px; text-align:center; font-family:monospace;'>" +
                                "<h3>Chart Error</h3>" +
                                "<p>" + e.message + "</p>" +
                                "</div>";
                        }
                    }
                };

                try {
                    // Check for D3 availability immediately
                    if (typeof d3 === 'undefined') {
                        throw new Error("D3.js library failed to load.");
                    }

                    // Filter out mismatched solvers
                    const cSolver = data.find(s => s.solver === 'C');
                    if (cSolver) {
                        const cIters = {};
                        cSolver.results.forEach(r => cIters[r.matrix] = r.iterations);
                        
                        data = data.filter(s => {
                            if (s.solver === 'C') return true;
                            const hasMismatch = s.results.some(r => {
                                const expected = cIters[r.matrix];
                                return expected && r.iterations !== expected;
                            });
                            return !hasMismatch;
                        });
                    }

                const matrices = ["1.matrix", "2.matrix", "3.matrix", "4.matrix", "5.matrix", "6.matrix"];
                const minTime = ${Math.min(...metrics.flatMap(m => m.results.map(r => r.time)).filter(t => t > 0))};
                const maxTime = ${Math.max(...metrics.flatMap(m => m.results.map(r => r.time)))};
                
                // Color Palette
                const color = d3.scaleOrdinal()
                    .domain(data.map(d => d.solver))
                    .range(["#00ff9d", "#00b8ff", "#ff0055", "#ffcc00", "#bd00ff", "#00ffff"]);



                function drawLineChart() {
                    const container = document.getElementById('d3-chart-container');
                    const width = container.clientWidth;
                    const height = container.clientHeight;
                    const margin = { top: 20, right: 120, bottom: 50, left: 60 };
                    
                    const svg = d3.select("#d3-chart-container")
                        .append("svg")
                        .attr("width", width)
                        .attr("height", height)
                        .append("g")
                        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
                        
                    const chartWidth = width - margin.left - margin.right;
                    const chartHeight = height - margin.top - margin.bottom;

                    // Zoom
                    const zoom = d3.zoom()
                        .scaleExtent([0.5, 5])
                        .filter(function(event) {
                            // Only allow zoom if mouse is within the chart area
                            if (event.type === 'wheel' || event.type === 'mousedown') {
                                const [mx, my] = d3.pointer(event, this);
                                return mx >= 0 && mx <= chartWidth && my >= 0 && my <= chartHeight;
                            }
                            return true;
                        })
                        .on("zoom", (event) => {
                            // Apply transform with margin offset
                            svg.attr("transform", "translate(" + (margin.left + event.transform.x) + "," + (margin.top + event.transform.y) + ") scale(" + event.transform.k + ")");
                        });
                    
                    d3.select("#d3-chart-container svg").call(zoom);
                    
                    // X Axis
                    const x = d3.scalePoint()
                        .domain(matrices)
                        .range([0, chartWidth])
                        .padding(0.5);
                        
                    svg.append("g")
                        .attr("transform", "translate(0," + chartHeight + ")")
                        .call(d3.axisBottom(x))
                        .selectAll("text")
                        .style("fill", "#e0e0e0")
                        .style("font-family", "JetBrains Mono");
                        
                    svg.append("text")
                        .attr("text-anchor", "end")
                        .attr("x", chartWidth)
                        .attr("y", chartHeight + 40)
                        .style("fill", "#5c5c66")
                        .style("font-size", "12px")
                        .text("Matrix Input");

                    // Y Axis
                    const y = d3.scaleLog()
                        .domain([minTime, maxTime])
                        .range([chartHeight, 0]);

                    svg.append("g")
                        .call(d3.axisLeft(y)
                            .ticks(5)
                            .tickFormat((d) => {
                                const ticks = y.ticks(5);
                                if (ticks.indexOf(d) % 2 === 0 || ticks.length <= 3) {
                                    return d >= 1 ? d3.format(".1f")(d) + "s" : d3.format(".2f")(d) + "s";
                                }
                                return "";
                            })
                        )
                        .selectAll("text")
                        .style("fill", "#e0e0e0")
                        .style("font-family", "JetBrains Mono");

                    svg.append("text")
                        .attr("transform", "rotate(-90)")
                        .attr("y", 0 - margin.left)
                        .attr("x", 0 - (chartHeight / 2))
                        .attr("dy", "1em")
                        .style("text-anchor", "middle")
                        .style("fill", "#5c5c66")
                        .style("font-size", "12px")
                        .text("Time (seconds) - Log Scale");
                    
                    // Grid lines
                    svg.append("g")
                        .attr("class", "grid")
                        .attr("opacity", 0.1)
                        .call(d3.axisLeft(y).tickSize(-chartWidth).tickFormat(""));

                    // Line Generator
                    const line = d3.line()
                        .x(d => x(d.matrix))
                        .y(d => y(Math.max(d.time, minTime)));

                    // Draw Lines
                    data.forEach(solver => {
                        const solverData = solver.results.filter(r => matrices.includes(r.matrix));
                        const safeSolverClass = "dot-" + solver.solver.replace(/[^a-zA-Z0-9]/g, '_');

                        svg.append("path")
                            .datum(solverData)
                            .attr("fill", "none")
                            .attr("stroke", color(solver.solver))
                            .attr("stroke-width", 2)
                            .attr("d", line)
                            .attr("class", "line-path");

                        const pointGroup = svg.selectAll("." + safeSolverClass)
                            .data(solverData)
                            .enter().append("g")
                            .attr("class", safeSolverClass)
                            .attr("transform", d => "translate(" + x(d.matrix) + ", " + y(Math.max(d.time, minTime)) + ")");

                        // Logo Image
                        pointGroup.append("image")
                            .attr("xlink:href", solver.logo)
                            .attr("x", -8)
                            .attr("y", -8)
                            .attr("width", 16)
                            .attr("height", 16)
                            .attr("preserveAspectRatio", "xMidYMid meet")
                            .attr("filter", d => {
                                const baseLang = solver.solver.replace(/ \((Manual|AI)\)$/, '');
                                const config = tailoring[baseLang] || tailoring[solver.solver];
                                if (config?.invert) return "url(#filter-invert)";
                                if (config?.transparent_white) return "url(#filter-transparent-white)";
                                return null;
                            });

                        // Docker Icon (Whale)
                        if (solver.runType === 'Docker') {
                            pointGroup.append("text")
                                .text("🐳")
                                .attr("x", 6)
                                .attr("y", 6)
                                .attr("font-size", "10px")
                                .style("pointer-events", "none");
                        }

                        // Interactions
                        pointGroup.on("mouseover", function (event, d) {
                                d3.select(this).raise();
                                d3.select(this).select("image")
                                    .attr("width", 24)
                                    .attr("height", 24)
                                    .attr("x", -12)
                                    .attr("y", -12);
                                
                                const tooltip = document.getElementById('tooltip');
                                tooltip.style.display = 'block';
                                tooltip.style.left = (event.clientX + 15) + 'px';
                                tooltip.style.top = (event.clientY + 15) + 'px';
                                tooltip.innerHTML = "<strong style='color:" + color(solver.solver) + "'>" + solver.solver + "</strong>" + 
                                                    (solver.runType === 'Docker' ? " 🐳" : "") +
                                                    "<br>Matrix: " + d.matrix + "<br>Time: " + d.time.toFixed(6) + "s<br>Iters: " + d.iterations;
                            })
                            .on("mouseout", function () {
                                d3.select(this).select("image")
                                    .attr("width", 16)
                                    .attr("height", 16)
                                    .attr("x", -8)
                                    .attr("y", -8);
                                document.getElementById('tooltip').style.display = 'none';
                            });

// Label
const lastPoint = solverData[solverData.length - 1];
if (lastPoint) {
    svg.append("text")
        .attr("x", x(lastPoint.matrix) + 10)
        .attr("y", y(Math.max(lastPoint.time, minTime)))
        .attr("dy", "0.35em")
        .style("fill", color(solver.solver))
        .style("font-size", "12px")
        .style("font-weight", "bold")
        .text(solver.solver);
}
                    });
                }

function drawJockeyChart() {
    const container = document.getElementById('d3-chart-container');
    const width = container.clientWidth;
    const height = container.clientHeight;
    const margin = { top: 20, right: 60, bottom: 20, left: 150 };

    const svg = d3.select("#d3-chart-container")
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    const chartWidth = width - margin.left - margin.right;
    const chartHeight = height - margin.top - margin.bottom;

    // Zoom
    const zoom = d3.zoom()
        .scaleExtent([0.5, 5])
        .filter(function(event) {
            // Only allow zoom if mouse is within the chart area
            if (event.type === 'wheel' || event.type === 'mousedown') {
                const [mx, my] = d3.pointer(event, this);
                return mx >= 0 && mx <= chartWidth && my >= 0 && my <= chartHeight;
            }
            return true;
        })
        .on("zoom", (event) => {
            svg.attr("transform", "translate(" + (margin.left + event.transform.x) + "," + (margin.top + event.transform.y) + ") scale(" + event.transform.k + ")");
        });
    
    d3.select("#d3-chart-container svg").call(zoom);

                    // Calculate Total Time for sorting
                    const sortedData = [...data].map(s => {
                        const totalTime = s.results.reduce((acc, r) => acc + r.time, 0);
                        return { ...s, totalTime: totalTime > 0 ? totalTime : 0.000001 }; // Prevent 0 for log scale
                    }).sort((a, b) => a.totalTime - b.totalTime); // Fastest first

    const minTotal = d3.min(sortedData, d => d.totalTime);
    const maxTotal = d3.max(sortedData, d => d.totalTime);

    // Y Axis (Solvers)
    const y = d3.scaleBand()
        .domain(sortedData.map(d => d.solver))
        .range([0, chartHeight])
        .padding(0.2);

    // X Axis (Total Time - Log Scale)
    const x = d3.scaleLog()
        .domain([minTotal, maxTotal])
        .range([0, chartWidth]);

    // Draw Tracks (Background Lines)
    svg.selectAll(".track")
        .data(sortedData)
        .enter().append("line")
        .attr("x1", 0)
        .attr("x2", chartWidth)
        .attr("y1", d => y(d.solver) + y.bandwidth() / 2)
        .attr("y2", d => y(d.solver) + y.bandwidth() / 2)
        .attr("stroke", "#2a2a35")
        .attr("stroke-width", 1)
        .attr("stroke-dasharray", "4");

    // Draw Bars (Progress)
    svg.selectAll(".bar")
        .data(sortedData)
        .enter().append("rect")
        .attr("y", d => y(d.solver) + y.bandwidth() / 2 - 2)
        .attr("height", 4)
        .attr("x", 0)
        .attr("width", d => x(d.totalTime))
        .attr("fill", d => color(d.solver))
        .attr("opacity", 0.6);

    // Draw Logos (Jockeys)
    svg.selectAll(".jockey")
        .data(sortedData)
        .enter().append("image")
        .attr("xlink:href", d => d.logo)
        .attr("x", d => x(d.totalTime) - 20) // Center image on end of bar
        .attr("y", d => y(d.solver) + y.bandwidth() / 2 - 10)
        .attr("width", 20)
        .attr("height", 20)
        .attr("preserveAspectRatio", "xMidYMid meet")
        .attr("filter", d => {
            const baseLang = d.solver.replace(/ \((Manual|AI)\)$/, '');
            const config = tailoring[baseLang] || tailoring[d.solver];
            if (config?.invert) return "url(#filter-invert)";
            if (config?.transparent_white) return "url(#filter-transparent-white)";
            return null;
        })
        .on("mouseover", function (event, d) {
            d3.select(this).attr("width", 32).attr("height", 32).attr("x", x(d.totalTime) - 16).attr("y", y(d.solver) + y.bandwidth() / 2 - 16);
            const tooltip = document.getElementById('tooltip');
            tooltip.style.display = 'block';
            tooltip.style.left = (event.clientX + 15) + 'px';
            tooltip.style.top = (event.clientY + 15) + 'px';
            tooltip.innerHTML = "<strong style='color:" + color(d.solver) + "'>" + d.solver + "</strong><br>Total Time: " + d.totalTime.toFixed(4) + "s";
        })
        .on("mouseout", function (event, d) {
            d3.select(this).attr("width", 24).attr("height", 24).attr("x", x(d.totalTime) - 12).attr("y", y(d.solver) + y.bandwidth() / 2 - 12);
            document.getElementById('tooltip').style.display = 'none';
        });

    // Y Axis Labels
    svg.append("g")
        .call(d3.axisLeft(y))
        .selectAll("text")
        .style("fill", "#e0e0e0")
        .style("font-family", "JetBrains Mono")
        .style("font-size", "10px");

    // X Axis
    svg.append("g")
        .attr("transform", "translate(0," + chartHeight + ")")
        .call(d3.axisBottom(x).ticks(5, ".1f"))
        .selectAll("text")
        .style("fill", "#5c5c66");
} // End of drawJockeyChart

                // Initial Draw
                if (typeof d3 !== 'undefined') {
                    drawLineChart();
                } else {
                    throw new Error("D3.js not loaded");
                }

                } catch (e) {
                    console.error("Error initializing D3 charts:", e);
                    // If initialization fails, switchChart is still defined and will show error when clicked
                    // Show error immediately
                    window.switchChart('line'); 
                }
            }) ();
    </script>
    <script>
// Inject Matrix Data
const matrixPuzzles = ${JSON.stringify(matrixContents)};

// Matrix Screensaver Logic
// Define variables in outer scope to be accessible
let startScreensaverGlobal;

(function () {
    try {


        console.log("Initializing Matrix Screensaver...");
        const canvas = document.getElementById('matrix-canvas');
        if (!canvas) console.error("Matrix canvas not found!");
        
        const ctx = canvas.getContext('2d');
        const container = canvas.parentElement;
        const chartContainer = document.getElementById('d3-chart-container');

    let width, height;
    let columns;
    let active = false;
    let ignoreInput = false;
    let animationId;
    let frame = 0;

    // Expose globally immediately
    window.startScreensaver = startScreensaver;
    startScreensaverGlobal = startScreensaver;
    
    // Puzzle Overlay State
    let puzzleY = -1000; // Start way above
    let currentPuzzleIndex = 0;
    let puzzleLines = [];
    
    function prepareNextPuzzle() {
        console.log('prepareNextPuzzle called, matrixPuzzles.length:', matrixPuzzles.length);
        if (matrixPuzzles.length === 0) return;
        const rawText = matrixPuzzles[currentPuzzleIndex];
        puzzleLines = rawText.split('\\n');
        console.log('Prepared puzzle with', puzzleLines.length, 'lines');
        // Start from the BOTTOM of the screen
        puzzleY = height || window.innerHeight; 
        currentPuzzleIndex = (currentPuzzleIndex + 1) % matrixPuzzles.length;
        specialRows.clear(); // Reset special rows for new puzzle
    }

    // Matrix characters (Katakana + Latin)
    const chars = '/Cars10アァカサタナハマヤャラワガザダバパイィキシチニヒミリヰギジヂビピウゥクスツヌフムユュルグズブヅプエェケセテネヘメレヱゲゼデベペオォコソトノホモヨョロヲゴゾドボポヴッン0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

    function resize() {
        if (document.fullscreenElement) {
            canvas.style.position = 'fixed';
            canvas.style.top = '0';
            canvas.style.left = '0';
            canvas.style.width = '100vw';
            canvas.style.height = '100vh';
            canvas.style.zIndex = '1000';
            width = window.innerWidth;
            height = window.innerHeight;
        } else {
            canvas.style.position = 'absolute';
            canvas.style.top = '0';
            canvas.style.left = '0';
            canvas.style.width = '100%';
            canvas.style.height = '100%';
            canvas.style.zIndex = '10';
            // Use current parent, which might change
            const parent = canvas.parentElement || container;
            width = parent.clientWidth;
            height = parent.clientHeight;
        }

        canvas.width = width;
        canvas.height = height;

        const fontSize = 14;
        columns = Math.ceil(width / fontSize);

        // Initialize drops
        drops = [];
        for (let i = 0; i < columns; i++) {
            drops[i] = Math.random() * -100; // Start above screen
        }
    }

    // Glow State
    let glowHue = 0; // 0 for Red, 240 for Blue
    let glowDirection = 1;
    
    // Dancing Highlight State
    let dancingRow = 0;
    let dancingCol = 0;
    let lastDanceTime = 0;
    
    // Slide-in Animation State
    let slideInComplete = false;
    
    // Easter Egg State
    let lastCars10Time = 0;
    let specialRows = new Set();
    
    let currentMode = 'red'; // Default to full screen

    function draw() {
        if (active) {
            animationId = requestAnimationFrame(draw);
        }

        frame++;
        if (frame % 2 !== 0) return;

        // Black background with opacity for trail effect
        // Lower opacity = longer trails
        ctx.fillStyle = 'rgba(0, 0, 0, 0.03)';
        ctx.fillRect(0, 0, width, height);

        ctx.fillStyle = '#0F0'; // Green text
        ctx.font = '14px monospace';

        // Draw Matrix Rain
        for (let i = 0; i < drops.length; i++) {
            const x = i * 14;
            const y = drops[i] * 14;
            // Occasional Easter Egg
            if (Math.random() > 0.999) {
                const text = "/cars10";
                ctx.fillStyle = '#FFF';
                ctx.shadowColor = '#FFF';
                ctx.shadowBlur = 8;
                ctx.font = 'bold 16px monospace'; // Slightly larger/bold
                for (let k = 0; k < text.length; k++) {
                    ctx.fillText(text[k], x, y + (k * 14));
                }
                ctx.shadowBlur = 0;
                ctx.font = '14px monospace'; // Reset
                ctx.fillStyle = '#0F0'; // Reset
            } else {
                const text = chars.charAt(Math.floor(Math.random() * chars.length));
                ctx.fillText(text, x, y);
            }

            // Reset drop to top randomly after it has crossed the screen
            if (y > height && Math.random() > 0.975) {
                drops[i] = 0;
            }

            drops[i]++;
        }

        // Check if rain has reached halfway
        let rainHalfway = false;
        const gridHeight = height / 14; // 14px font for rain
        for (let i = 0; i < drops.length; i++) {
            if (drops[i] > gridHeight / 2) {
                rainHalfway = true;
                break;
            }
        }

        // Draw Puzzle Overlay (only in fullscreen mode, after slide-in completes)
        if (currentMode === 'red' && puzzleLines.length > 0 && slideInComplete) {
            // 1. Calculate Font Size
            // Goal: Row fits 2/3 of screen width
            const targetWidth = width * 0.66;
            const charCount = 17; // 9 digits + 8 spaces
            let fontSize = Math.floor(targetWidth / charCount * 1.6); // Multiplier to adjust

            // "Font size should also increase by 30%" - ONLY for Red Mode (Full Screen)
            // For Blue Mode (Chart), we scale it down a bit relative to that
            if (currentMode === 'red') {
                fontSize = Math.floor(fontSize * 0.675); // Half the previous size
            } else {
                // Blue mode: maybe keep it standard or slightly smaller
                fontSize = Math.floor(fontSize * 0.8);
            }

            ctx.font = 'bold ' + fontSize + 'px "JetBrains Mono", monospace';
            ctx.textAlign = 'center';
            
            // Base color for fallback
            const color = '#FF0055'; // Bright neon red
            
            // Reduced glow for "fading trails" look
            ctx.shadowBlur = 5;
            ctx.shadowColor = color;
            ctx.fillStyle = color;

            const lineHeight = fontSize * 3.0; // Double spacing (simulates blank row)
            const startX = width / 2;

            // 3. Dancing Highlight Logic
            if (Date.now() - lastDanceTime > 200) {
                dancingRow = Math.floor(Math.random() * 9);
                dancingCol = Math.floor(Math.random() * 9);
                lastDanceTime = Date.now();
            }

            // 4. Easter Egg Logic: /cars10
            if (Date.now() - lastCars10Time > 1000) { // Every 1 second
                 const visibleRows = [];
                 for (let i = 0; i < puzzleLines.length; i++) {
                    const lineY = puzzleY + (i * lineHeight);
                    // Check if fully visible on screen
                    if (lineY > lineHeight && lineY < height - lineHeight) {
                        visibleRows.push(i);
                    }
                 }
                 
                 if (visibleRows.length > 0) {
                     // Pick a random row that isn't already special
                     const candidates = visibleRows.filter(r => !specialRows.has(r));
                     if (candidates.length > 0) {
                         const randomRow = candidates[Math.floor(Math.random() * candidates.length)];
                         specialRows.add(randomRow);
                     }
                 }
                 lastCars10Time = Date.now();
            }

            // Define columns to draw based on mode
            let xPositions = [];
            if (currentMode === 'red') {
                // Single centered column for fullscreen
                xPositions = [width * 0.5];
            } else {
                // Single centered column for chart mode
                xPositions = [width * 0.5];
            }

            // 3. Iterate over each column position
            for (const startX of xPositions) {
                // Determine visible rows for this column
                const visibleRows = [];
                for (let i = 0; i < puzzleLines.length; i++) {
                    const lineY = puzzleY + (i * lineHeight);
                    
                    // Only consider visible rows
                    if (lineY > -lineHeight * 0.2 && lineY < height + lineHeight * 0.2) {
                        visibleRows.push(i);
                    }
                }
                
                // No background clearing - completely transparent
                // Numbers will appear directly over Matrix rain
                
                // Draw each line of the puzzle
                for (let i = 0; i < puzzleLines.length; i++) {
                    const lineY = puzzleY + (i * lineHeight);
                    
                    // Only draw if visible
                    if (lineY > -lineHeight && lineY < height + lineHeight) {
                        const line = puzzleLines[i];
                        


                        const parts = line.trim().split(/\s+/);
                        
                        if (parts.length === 9) {
                            const spacing = fontSize * 1.2;
                            // Calculate total width of the puzzle block
                            const totalBlockWidth = (9 * fontSize * 1.2); // 9 chars * spacing
                            
                            // Clear the entire puzzle area for this column to prevent streaks
                            // We do this ONCE per column loop, not per character, but here we are inside the loop.
                            // Actually, let's do it per character but simpler:
                            
                            for (let j = 0; j < parts.length; j++) {
                                const isDancing = (i % 9 === dancingRow) && (j === dancingCol);
                                
                                // Calculate distance from center for scale effect
                                const distFromCenter = Math.abs(lineY - height / 2);
                                const maxDist = height / 2;
                                const centerFactor = 1 - (distFromCenter / maxDist); // 1 at center, 0 at edges
                                
                                // Scale animation: grow at center (applies to all numbers)
                                const scaleBoost = 1 + (centerFactor * 0.3); // 1.0 to 1.3x at center
                                
                                ctx.save();
                                // Tall Numbers: Scale Y by 2, plus center boost
                                ctx.translate(currentX, lineY);
                                ctx.scale(scaleBoost, 2 * scaleBoost);
                                
                                // Draw a semi-transparent background box to hide streaks but keep it "semi transparent"
                                ctx.globalCompositeOperation = 'source-over';
                                ctx.fillStyle = 'rgba(0, 0, 0, 0.8)'; // Semi-transparent black
                                // Clear from way above to way below
                                ctx.fillRect(-fontSize * 0.6, -fontSize * 2, fontSize * 1.2, fontSize * 6);
                                
                                // Draw number
                                ctx.globalCompositeOperation = 'source-over';
                                ctx.shadowBlur = 0; 
                                ctx.shadowColor = 'transparent';
                                ctx.strokeStyle = isDancing ? '#00b8ff' : '#FF0055'; // Blue if dancing, Red otherwise
                                ctx.lineWidth = isDancing ? 4 : 2;
                                
                                if (isDancing) {
                                    ctx.scale(1.5, 1.5); // Extra scale for dancing
                                    ctx.shadowBlur = 10;
                                    ctx.shadowColor = '#00b8ff';
                                }
                                ctx.strokeText(parts[j], 0, 0);
                                
                                ctx.restore();
                                currentX += spacing;
                            }
                        } else {
                            // Header or other text
                            ctx.save();
                            ctx.translate(startX, lineY);
                            ctx.scale(1, 2);
                            ctx.strokeStyle = color;
                            ctx.lineWidth = 2;
                            ctx.strokeText(line, 0, 0);
                            ctx.restore();
                        }
                    }
                }
            }

            // Reset shadow
            ctx.shadowBlur = 0;

            puzzleY -= 5; // Scroll UP (Ascent)

            // Reset if puzzle went off screen (top)
            // Total height of puzzle = lines * lineHeight
            const totalPuzzleHeight = puzzleLines.length * lineHeight;
            if (puzzleY < -totalPuzzleHeight) {
                prepareNextPuzzle();
            }
        } else {
             // If not ready or no lines, ensure we are ready for next
             if (puzzleLines.length === 0) prepareNextPuzzle();
        }
    }


    function startScreensaver(mode) {
        if (active) return;
        active = true;
        ignoreInput = true;
        setTimeout(() => { ignoreInput = false; }, 1500); // Grace period
        
        currentMode = mode || 'red'; // Default to red

        const canvas = document.getElementById('matrix-canvas');
        const content = document.getElementById('main-content');
        const chartContainer = document.getElementById('d3-chart-container');

        if (currentMode === 'red') {
            // Full Screen Mode
            document.body.appendChild(canvas); // Move to body
            canvas.style.position = 'fixed';
            canvas.style.top = '0';
            canvas.style.left = '0';
            canvas.style.width = '100vw';
            canvas.style.height = '100vh';
            canvas.style.zIndex = '1000';
            
            // Add fullscreen class to body to hide scrollbars
            document.body.classList.add('fullscreen-active');
            
            // Show fullscreen header
            const fsHeader = document.getElementById('fullscreen-header');
            if (fsHeader) fsHeader.style.display = 'block';
            
            // Initial State for Animation
            canvas.classList.add('matrix-slide-enter');
            content.classList.add('content-slide-exit');
            
            // Force Reflow
            void canvas.offsetWidth;
            
            // Start Animation
            requestAnimationFrame(() => {
                canvas.classList.add('matrix-slide-active');
                
                // Delay content slide until Matrix is halfway down (0.75s of 1.5s total)
                setTimeout(() => {
                    content.classList.add('content-slide-active');
                }, 750);
                
                // Set flag earlier so numbers appear sooner (0.5s instead of 1.5s)
                setTimeout(() => {
                    slideInComplete = true;
                    console.log('slideInComplete set to true');
                }, 500);
                
                // Trigger Browser Fullscreen AFTER animation starts
                // This prevents the browser's fullscreen transition from interfering
                setTimeout(() => {
                    if (document.documentElement.requestFullscreen) {
                        document.documentElement.requestFullscreen().catch(e => console.log(e));
                    }
                }, 100);
            });
            
        } else {
            // Chart Mode (Blue Pill)
            chartContainer.appendChild(canvas); // Move to chart container
            canvas.style.position = 'absolute';
            canvas.style.top = '0';
            canvas.style.left = '0';
            canvas.style.width = '100%';
            canvas.style.height = '100%';
            canvas.style.zIndex = '10';

            // Add slide-down animation class
            canvas.classList.add('slide-down-slow');
        }

        canvas.style.display = 'block';

        resize();
        window.addEventListener('resize', resize);

        // Reset puzzle state
        puzzleLines = [];
        currentPuzzleIndex = 0;
        prepareNextPuzzle();

        draw();
    }

    function stopScreensaver() {
        if (!active) return;
        active = false;
        slideInComplete = false; // Reset flag
        const canvas = document.getElementById('matrix-canvas');
        const content = document.getElementById('main-content');

        canvas.style.display = 'none';
        
        // Remove fullscreen class from body
        document.body.classList.remove('fullscreen-active');
        
        // Hide fullscreen header
        const fsHeader = document.getElementById('fullscreen-header');
        if (fsHeader) fsHeader.style.display = 'none';
        
        // Remove animation classes
        canvas.classList.remove('slide-down-slow');
        canvas.classList.remove('matrix-slide-enter');
        canvas.classList.remove('matrix-slide-active');
        content.classList.remove('content-slide-exit');
        content.classList.remove('content-slide-active');

        // Exit Browser Fullscreen if active
        if (document.fullscreenElement) {
            document.exitFullscreen().catch(e => console.log(e));
        }

        cancelAnimationFrame(animationId);
        window.removeEventListener('resize', resize);
        
        canvas.className = '';
        content.className = 'content-wrapper';

        // Reset to body for safety
        document.body.appendChild(canvas);
    }

    // Idle Timer with Persistence
    const idleLimit = 5 * 60 * 1000; // 5 minutes

    function getLastActive() {
        return parseInt(localStorage.getItem('lastActive') || Date.now().toString());
    }

    function setLastActive() {
        localStorage.setItem('lastActive', Date.now().toString());
    }

    function checkIdle() {
        const lastActive = getLastActive();
        const diff = Date.now() - lastActive;
        if (diff >= idleLimit) {
            startScreensaver();
        }
    }

    setInterval(checkIdle, 1000);

    function resetTimer(e) {
        setLastActive();
        if (active && !ignoreInput) {
            // Only stop on Option key (Alt)
            if (e && e.type === 'keydown' && e.key === 'Alt') {
                stopScreensaver();
            }
        }
    }

    // Events to reset timer
    window.onload = resetTimer;
    document.onmousemove = resetTimer;
    document.onkeydown = resetTimer;
    document.onclick = resetTimer;
    document.onscroll = resetTimer;

    // Initialize
    if (!localStorage.getItem('lastActive')) {
        setLastActive();
    }
    checkIdle();

    // Handle resize
    window.addEventListener('resize', () => {
        if (active) resize();
    });

    // Expose globally

    
    console.log("Matrix Screensaver initialized successfully. startScreensaver is:", typeof window.startScreensaver);
    

    } catch (e) {
        console.error("CRITICAL ERROR in Matrix Screensaver IIFE:", e);
        const errDiv = document.createElement('div');
        errDiv.style.position = 'fixed';
        errDiv.style.top = '0';
        errDiv.style.left = '0';
        errDiv.style.width = '100%';
        errDiv.style.background = 'red';
        errDiv.style.color = 'white';
        errDiv.style.zIndex = '9999';
        errDiv.style.padding = '10px';
        errDiv.innerText = 'SCREENSAVER ERROR: ' + e.toString();
        document.body.appendChild(errDiv);
    }
})();

// Ensure global access


window.showMismatch = function(solverName, matrixName, cell) {
    const solver = metricsData.find(s => s.solver === solverName);
    if (!solver) return;
    const result = solver.results.find(r => r.matrix === matrixName);
    if (!result) return;

    document.getElementById('mismatchTitle').innerText = 'Mismatch: ' + solverName + ' on ' + matrixName;
    
    // Actual Output
    let actual = result.output || 'No output captured.';
    
    document.getElementById('actualOutput').innerText = actual;

    // Expected Output
    const expected = referenceOutputs[matrixName] || 'No reference output found.';
    document.getElementById('expectedOutput').innerText = expected;

    // Rerun Command
    currentRerunCommand = '# Run from SudokuSolver root\\nfind . -name runMe_ai.sh | grep "/' + solverName + '/" | head -n 1 | xargs -I {} sh -c \\'cd $(dirname {}) && ./runMe_ai.sh ../../Matrices/' + matrixName + '\\'';
    
    const modal = document.getElementById('mismatchModal');
    modal.style.display = 'block';
    
    // Anchor to cell
    const rect = cell.getBoundingClientRect();
    const content = modal.querySelector('.modal-content');
    
    // Calculate position (right of cell, aligned top)
    const top = rect.top + window.scrollY;
    const left = rect.right + window.scrollX + 20; // 20px gap
    
    content.style.position = 'absolute';
    content.style.top = top + 'px';
    content.style.left = left + 'px';
    content.style.margin = '0';
    content.style.transform = 'none';
    content.style.width = 'auto';
    content.style.maxWidth = '600px'; // Constrain width if needed
}

window.closeMismatchModal = function() {
    document.getElementById('mismatchModal').style.display = 'none';
}

window.copyRerunCommand = function() {
    const btn = document.getElementById('copyRerunBtn');
    navigator.clipboard.writeText(currentRerunCommand).then(() => {
        const originalText = btn.innerText;
        btn.innerText = 'Copied!';
        setTimeout(() => {
            btn.innerText = originalText;
        }, 2000);
    });
}



</script>
    </body>
    </html>
        `;
    return html;
}

export async function main() {
    const rootDir = process.env.OUTPUT_DIR || path.resolve(__dirname, '..');
    const metricsFile = path.join(rootDir, 'CleanedUp', 'CleanedUp_Metrics.json');
    const htmlFile = path.join(rootDir, 'benchmark_report.html');

    const generateOnly = process.argv.includes('--generate-only');
    let allMetrics: SolverMetrics[] = [];

    if (generateOnly) {
        console.log("Generate-only mode: Reading existing metrics...");
        try {
            const data = await fs.readFile(metricsFile, 'utf-8');
            allMetrics = JSON.parse(data);
            console.log(`Loaded ${allMetrics.length} metrics from ${metricsFile} `);

            const referenceOutputs = await readReferenceOutputs(__dirname + '/../');

            // Read allowed matrices from TypeScript metrics
            const typeScriptMetricsFile = path.join(rootDir, 'Manual', 'TypeScript', 'metrics.json');
            let allowedMatrices: string[] = [];
            try {
                const tsData = await fs.readFile(typeScriptMetricsFile, 'utf-8');
                const tsMetrics = JSON.parse(tsData);
                allowedMatrices = [...new Set(tsMetrics.map((m: any) => m.matrix))] as string[];
                console.log(`Filtering report to ${allowedMatrices.length} matrices from TypeScript metrics.`);
            } catch (e) {
                console.warn("Could not read Manual/TypeScript/metrics.json, defaulting to all.", e);
            }

            const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts, referenceOutputs, allowedMatrices);
            await fs.writeFile(htmlFile, htmlContent);
            // Write timestamp file for smart refresh
            const timestampFile = path.join(rootDir, 'timestamp.js');
            await fs.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()}; `);
            console.log(`Regenerated report at ${htmlFile} `);

        } catch (e) {
            console.error(`Failed to read ${metricsFile}: `, e);
            process.exit(1);
        }
    } else {
        const solverScripts = await findSolvers(rootDir);
        const matrices = ['3.matrix', '4.matrix', '5.matrix'];

        // Load existing metrics to preserve previous results
        try {
            const data = await fs.readFile(metricsFile, 'utf-8');
            allMetrics = JSON.parse(data);
            console.log(`Loaded ${allMetrics.length} existing metrics.`);
        } catch (e) {
            console.log("No existing metrics found, starting fresh.");
        }

        for (const matrix of matrices) {
            console.log(`\n-- - Benchmarking ${matrix} ---\n`);
            for (const script of solverScripts) {
                const metrics = await runSolver(script, matrix);
                if (metrics) {
                    let existingSolver = allMetrics.find(s => s.solver === metrics.solver);
                    if (!existingSolver) {
                        existingSolver = { solver: metrics.solver, timestamp: metrics.timestamp, results: [] };
                        allMetrics.push(existingSolver);
                    }
                    // Append new results, avoiding duplicates
                    for (const res of metrics.results) {
                        existingSolver.results = existingSolver.results.filter(r => r.matrix !== res.matrix);
                        existingSolver.results.push(res);
                    }

                    // Save & Generate & Screenshot after EVERY solver run
                    await fs.writeFile(metricsFile, JSON.stringify(allMetrics, null, 2));
                    // console.log(`Saved metrics to ${ metricsFile } `);

                    const referenceOutputs = await readReferenceOutputs(__dirname + '/../');
                    const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts, referenceOutputs);
                    await fs.writeFile(htmlFile, htmlContent);
                    // Write timestamp file for smart refresh
                    const timestampFile = path.join(rootDir, 'timestamp.js');
                    await fs.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()}; `);

                    console.log(`Report generated at ${htmlFile} `);

                    await captureScreenshot(htmlFile);
                }
            }
        }
    }
}

export async function captureScreenshot(htmlFilePath: string) {
    const screenshotsDir = path.join(path.dirname(htmlFilePath), 'screenshots');
    await fs.mkdir(screenshotsDir, { recursive: true });

    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const screenshotPath = path.join(screenshotsDir, `benchmark_${timestamp}.png`);

    console.log("Capturing screenshot...");
    const browser = await puppeteer.launch({ headless: true });
    const page = await browser.newPage();

    // Set viewport to 1920x1080 for high quality video frames
    await page.setViewport({ width: 1920, height: 1080 });

    await page.goto(`file://${htmlFilePath}`, { waitUntil: 'networkidle0' });

    // Wait for D3 chart to render
    try {
        await page.waitForSelector('#d3-chart-container svg', { timeout: 5000 });
    } catch (e) {
        console.warn("Chart selector not found or timed out, proceeding with screenshot anyway.");
    }

    // Wait for table to populate
    try {
        await page.waitForSelector('tbody tr', { timeout: 5000 });
    } catch (e) {
        console.warn("Table rows not found or timed out.");
    }

    // Modify page for screenshot: Sort by Score and Hide rows > 10
    await page.evaluate(() => {
        // 1. Sort by Score (Ascending)
        // We need to find the sort button for Score and click it, or call the function directly
        // The function is sortRows('score', btn)
        // Let's call it directly if possible, or simulate click

        // Reset sort to ensure we start clean
        // Accessing the global function defined in the HTML
        (window as any).currentSort = { metric: 'score', dir: -1 }; // Set to -1 so the next call flips it to 1 (Asc)
        (window as any).sortRows('score', null);

        // 2. Hide rows beyond top 10
        const rows = document.querySelectorAll('tbody tr');
        rows.forEach((row, index) => {
            if (index >= 10) {
                (row as HTMLElement).style.display = 'none';
            } else {
                (row as HTMLElement).style.display = ''; // Force show
            }
        });

        // 3. Hide the "Show Imposters" button and other controls to clean up UI
        const controls = document.querySelector('.controls');
        if (controls) (controls as HTMLElement).style.display = 'none';

        const personalityIntro = document.getElementById('personality-intro');
        if (personalityIntro) (personalityIntro as HTMLElement).style.display = 'none';

        // 4. Stop Screensaver if active
        if ((window as any).stopScreensaver) (window as any).stopScreensaver();
    });

    await page.screenshot({ path: screenshotPath, fullPage: true });

    await browser.close();
    console.log(`Screenshot saved to ${screenshotPath}`);
}



if (process.argv[1] === fileURLToPath(import.meta.url)) {
    main().catch(console.error);
}
