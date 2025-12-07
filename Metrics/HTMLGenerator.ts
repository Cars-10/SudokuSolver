import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';
import { fileURLToPath } from 'url';
import type { SolverMetrics } from './types.ts';
import { languageHistories, quotes, personalities, methodologyTexts, languageMetadata, narratorIntros, mismatchLabels, iterationLabels, timeLabels, memoryLabels, scoreLabels } from './LanguagesMetadata.ts';
export { languageHistories, quotes, personalities, methodologyTexts, languageMetadata, narratorIntros, mismatchLabels, iterationLabels, timeLabels, memoryLabels, scoreLabels };

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);



// --- Logic ---



// Helper to safely serialize data for injection into template literals
// We ONLY need to escape </script> to prevent breaking out of the script tag.
// JSON.stringify handles quotes and backslashes correctly for the JS parser.
function safeJSON(obj: any): string {
    return JSON.stringify(obj)
        .replace(/<\/script>/g, '<\\/script>')
        .replace(/\u2028/g, '\\u2028') // Line separator
        .replace(/\u2029/g, '\\u2029'); // Paragraph separator
}

export async function generateHtml(metrics: SolverMetrics[], history: any[], personalities: any, languageMetadata: any, methodologyTexts: any, referenceOutputs: any, allowedMatrices: string[] = [], benchmarkConfig: any = {}): Promise<string> {
    const clientJs = await fs.readFile(path.join(__dirname, 'report_client.js'), 'utf-8');
    console.log(`generateHtml received ${metrics.length} metrics.`);
    // Pre-load local logos
    const localLogos = await glob('CleanedUp/logos/*.{png,svg}');
    const logoMap = new Map<string, string>();
    for (const p of localLogos) {
        const filename = path.basename(p);
        const name = path.basename(p, path.extname(p)).toLowerCase();
        logoMap.set(name, `logos/${filename}`);
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

    const maxMatrices = 6; // Focus on Matrix 1-6

    // Sort results by total time (fastest first)
    const sortedMetrics = [...metrics].sort((a, b) => {
        const timeA = a.results.reduce((acc, r) => acc + r.time, 0);
        const timeB = b.results.reduce((acc, r) => acc + r.time, 0);
        return timeA - timeB;
    });

    // Find C baseline
    const cMetrics = metrics.find(m => m.solver === 'C');
    const cTimes = cMetrics ? cMetrics.results.map(r => r.time) : [];
    const cTotalIters = cMetrics ? cMetrics.results.reduce((a, b) => a + b.iterations, 0) : 0;

    // Filter out mismatches
    // Filter out mismatches - REVERTED to allow client-side toggle
    // if (cTotalIters > 0) {
    //     metrics = metrics.filter(m => {
    //         if (m.solver === 'C') return true;
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
    // Calculate mismatch count (Per-matrix logic, verified against C)
    let mismatchCount = 0;
    if (cMetrics) {
        // Create map for C
        const cMap = new Map<string, number>();
        cMetrics.results.forEach(r => cMap.set(r.matrix, r.iterations));

        mismatchCount = metrics.filter(m => {
            if (m.solver === 'C') return false;

            // Check each result for this solver
            for (const r of m.results) {
                const expected = cMap.get(r.matrix);
                // Only count as mismatch if we have a baseline and they differ
                if (expected !== undefined && r.iterations !== expected) {
                    return true;
                }
            }
            return false;
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
            min-width: 250px;
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
        
        .mismatch {
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
            opacity: 0;
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
            background-color: #1a1b26;
            padding: 30px;
            border-radius: 12px;
            width: 80%;
            max-width: 800px;
            max-height: 90vh;
            overflow-y: auto;
            border: 1px solid #414868;
            box-shadow: 0 20px 40px rgba(0,0,0,0.6);
            position: relative;
            display: flex;
            flex-direction: column;
        }

        .modal-header {
            display: flex;
            justify-content: space-between;
            align-items: flex-start;
            margin-bottom: 20px;
            border-bottom: 1px solid #414868;
            padding-bottom: 20px;
        }

        .modal-info {
            display: flex;
            gap: 20px;
            align-items: flex-start;
            width: 100%;
        }

        .modal-img-container {
            flex-shrink: 0;
            width: 120px;
            height: 120px;
            background: #24283b;
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            overflow: hidden;
            border: 1px solid #414868;
            position: relative;
        }

        .modal-img {
            width: 100%;
            height: 100%;
            object-fit: contain;
            padding: 10px;
        }
        
        /* Edit Mode Styles */
        .modal-edit-input {
            background: #24283b;
            border: 1px solid #565f89;
            color: #c0caf5;
            padding: 8px;
            border-radius: 4px;
            width: 100%;
            font-family: inherit;
            margin-bottom: 5px;
        }
        
        .modal-edit-textarea {
            width: 100%;
            min-height: 100px;
            background: #24283b;
            border: 1px solid #565f89;
            color: #c0caf5;
            padding: 8px;
            border-radius: 4px;
            font-family: inherit;
            resize: vertical;
        }
        
        .edit-controls {
            display: none; /* Hidden by default */
            gap: 10px;
            margin-top: 10px;
        }
        
        .modal-content.editing .edit-controls {
            display: flex;
        }
        
        .modal-content.editing .view-only {
            display: none;
        }
        
        .modal-content:not(.editing) .edit-only {
            display: none;
        }

        .author-list {
            display: flex;
            flex-wrap: wrap;
            gap: 15px;
            margin-top: 20px;
        }

        .author-item {
            display: flex;
            flex-direction: column;
            align-items: center;
            gap: 5px;
            background: #24283b;
            padding: 10px;
            border-radius: 8px;
            border: 1px solid #414868;
        }
        
        .author-img {
            width: 60px;
            height: 60px;
            border-radius: 50%;
            object-fit: cover;
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
            position: relative; /* Anchor for timer */
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
            align-items: center; /* Centered in box */
            z-index: 100;
            background: rgba(13, 13, 18, 0.8);
            padding: 10px 20px;
            border: 1px solid var(--primary);
            border-radius: 4px;
        }

        .riddle-text {
            font-family: 'JetBrains Mono', monospace;
            color: #00ff9d;
            text-shadow: 0 0 5px #00ff9d, 0 0 10px #00ff9d;
            font-size: 0.3em; /* Smaller to fit in box */
            margin-top: 0px;
            perspective: 1000px;
            cursor: default;
            text-align: center;
            position: absolute; /* Bound to bottom of relative parent */
            bottom: 4px;
            left: 0;
            right: 0;
            width: 100%;
            pointer-events: none;
            z-index: 200;
            display: block;
            height: auto;
            letter-spacing: 1px;
        }

        .riddle-char {
            display: inline-block;
            transform-style: preserve-3d;
            /* backface-visibility: hidden; Can cause flickering with text, optional */
        }

        .matrix-timer {
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            display: flex;
            align-items: center;
            justify-content: center;
            backdrop-filter: blur(5px);
            background: #000; /* Opaque start */
            overflow: hidden;
            z-index: 20; /* Ensure on top */
        }

        .timer-particle {
            position: absolute;
            font-family: 'JetBrains Mono', monospace;
            color: #00ff9d;
            font-size: 2px; /* 1/10th scale approx */
            text-shadow: 0 0 1px #00ff9d;
            font-weight: bold;
            user-select: none;
            will-change: transform, opacity;
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
            width: 70px;
            height: 24px;
            border-radius: 12px;
            overflow: hidden;
            border: 1px solid rgba(255, 255, 255, 0.3);
            backdrop-filter: blur(6px);
            background: rgba(255, 255, 255, 0.1);
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3), inset 0 1px 0 rgba(255, 255, 255, 0.2);
            transition: transform 0.2s ease;
        }

        .zoom-btn {
            background: transparent;
            border: none;
            color: var(--secondary);
            cursor: pointer;
            padding: 0;
            display: flex;
            align-items: center;
            justify-content: center;
            opacity: 0.7;
            transition: opacity 0.2s, color 0.2s;
        }
        .zoom-btn:hover {
            opacity: 1;
            color: var(--primary);
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

        /* Dropdown Menu */
        .dropdown {
            position: relative;
            display: inline-block;
        }
        
        .dropdown-content {
            display: none;
            position: absolute;
            background-color: var(--surface); /* Fallback */
            background: rgba(22, 22, 30, 0.95);
            backdrop-filter: blur(10px);
            min-width: 160px;
            box-shadow: 0 8px 16px 0 rgba(0,0,0,0.5);
            z-index: 1000;
            border: 1px solid var(--border);
            border-radius: 4px;
            padding: 5px 0;
            top: 100%;
            left: 0;
        }
        
        .dropdown:hover .dropdown-content {
            display: block;
        }
        
        .dropdown-content a {
            color: var(--text);
            padding: 10px 15px;
            text-decoration: none;
            display: block;
            font-size: 0.9em;
            cursor: pointer;
            transition: background 0.2s, color 0.2s;
        }
        
        .dropdown-content a:hover {
            background-color: rgba(0, 255, 157, 0.1);
            color: var(--primary);
        }

        /* Log Modal */
        #logModal {
            display: none;
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background: rgba(0, 0, 0, 0.85);
            justify-content: center;
            align-items: center;
            z-index: 10001;
        }
        
        #logModal .modal-content {
            background: #1e1e24;
            width: 80%;
            max-width: 900px;
            height: 80%;
            border-radius: 8px;
            border: 1px solid #444;
            display: flex;
            flex-direction: column;
            padding: 0;
            overflow: hidden;
            box-shadow: 0 10px 30px rgba(0,0,0,0.5);
        }
        
        #logHeader {
            padding: 15px;
            background: #2a2a35;
            border-bottom: 1px solid #444;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        
        #logOutput {
            flex: 1;
            padding: 15px;
            overflow: auto;
            font-family: 'JetBrains Mono', monospace;
            font-size: 0.9em;
            color: #d0d0d0;
            white-space: pre-wrap;
            background: #16161a;
        }

        .run-btn {
            background: transparent;
            border: 1px solid rgba(255,255,255,0.1);
            color: #00ff9d;
            cursor: pointer;
            font-size: 10px;
            padding: 2px 6px;
            border-radius: 4px;
            margin-right: 5px;
            opacity: 0.6;
            transition: all 0.2s;
        }
        .run-btn:hover {
            opacity: 1;
            background: rgba(0, 255, 157, 0.1);
            transform: scale(1.1);
        }

        /* Matrix Cell Content Layout */
        .cell-content {
            display: flex;
            flex-direction: column;
            justify-content: center;
            height: 100%;
            position: relative;
        }
        
        /* Make Matrix Cell Relative for absolute positioning of play button if needed, 
           or use flex row for header */
        .cell-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 2px;
        }
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <canvas id="matrix-canvas"></canvas>
    <div id="tooltip"></div>
    
    <!-- Log Modal -->
    <div id="logModal" onclick="if(event.target === this) closeLogModal()">
        <div class="modal-content">
            <div id="logHeader">
                <span id="logTitle" style="color: #fff; font-weight: bold;">Execution Log</span>
                <button class="btn" onclick="closeLogModal()" style="background:#ff5555; padding: 5px 15px;">Close</button>
            </div>
            <div id="logOutput"></div>
        </div>
    </div>

    <!-- Modal -->
    <div id="langModal" class="modal" style="display: none;" onclick="closeModal(event)">
        <div class="modal-content" id="modalContent">
            <div class="modal-header">
                <div class="modal-info">
                    <div class="modal-img-container" id="modalImgContainer">
                        <img id="modalImg" class="modal-img" src="" alt="Language Logo">
                        <div class="edit-only" style="position: absolute; bottom: 0; left: 0; right: 0; background: rgba(0,0,0,0.7); text-align: center; padding: 5px; font-size: 0.8em; cursor: pointer;" onclick="document.getElementById('logoInput').click()">
                            Change
                        </div>
                        <input type="file" id="logoInput" style="display: none" accept="image/*" onchange="uploadLogo(this)">
                    </div>
                    <div style="flex: 1;">
                        <input type="text" id="editInputs-title" class="modal-edit-input edit-only" placeholder="Language Name" style="font-size: 1.5em; font-weight: bold;">
                        <h2 id="modalTitle" class="view-only" style="margin: 0 0 5px 0; color: #7aa2f7;"></h2>
                        
                        <input type="text" id="editInputs-creator" class="modal-edit-input edit-only" placeholder="Creator">
                        <input type="text" id="editInputs-image" class="modal-edit-input edit-only" placeholder="Image URL (e.g., https://...)">
                        <input type="text" id="editInputs-date" class="modal-edit-input edit-only" placeholder="Date">
                        <p id="modalSubtitle" class="view-only" style="margin: 0; color: #565f89; font-size: 0.9em;"></p>
                        
                        <div style="margin-top: 10px;">
                            <input type="text" id="editInputs-location" class="modal-edit-input edit-only" placeholder="Location">
                            <input type="text" id="editInputs-benefits" class="modal-edit-input edit-only" placeholder="Benefits">
                            
                            <p id="modalLocation" class="view-only" style="margin: 5px 0 0 0; color: #9aa5ce; font-size: 0.9em;"></p>
                            <p id="modalLocation" class="view-only" style="margin: 5px 0 0 0; color: #9aa5ce; font-size: 0.9em;"></p>
                            <p id="modalBenefits" class="view-only" style="margin: 5px 0 0 0; color: #bb9af7; font-size: 0.9em;"></p>
                            <a id="modalGrokepia" class="view-only" href="#" target="_blank" style="margin: 5px 0 0 0; color: #4fd6be; font-size: 0.9em; text-decoration: none; display:none;">🔗 https://grokipedia.com</a>
                        </div>
                    </div>
                </div>
                <div style="display: flex; gap: 10px;">
                     <button class="btn" onclick="toggleLock()" id="lockBtn" title="Lock this result to skip future benchmarks">🔓 Unlocked</button>
                     <button class="btn" onclick="toggleEditMode()" id="editBtn">Edit</button>
                     <button class="btn edit-only" style="background: #4caf50;" onclick="saveLanguageDetails()">Save</button>
                     <span class="modal-close" onclick="closeModal(event)">&times;</span>
                </div>
            </div>
            
            <div class="modal-body">
                <h3 style="color: #7aa2f7; font-size: 1.1em; border-bottom: 1px solid #414868; padding-bottom: 5px;">Description</h3>
                <textarea id="editInputs-desc" class="modal-edit-textarea edit-only" placeholder="Description"></textarea>
                <div id="modalDesc" class="view-only" style="line-height: 1.6; color: #c0caf5;"></div>
                
                <h3 style="color: #7aa2f7; font-size: 1.1em; border-bottom: 1px solid #414868; padding-bottom: 5px; margin-top: 20px;">
                    Creators & Authors
                    <button class="btn edit-only" style="font-size: 0.7em; margin-left: 10px;" onclick="addAuthorField()">+ Add</button>
                </h3>
                <div id="authorList" class="author-list">
                    <!-- Dynamic authors -->
                </div>
                
                <div class="edit-only" style="margin-top: 20px; padding: 10px; background: #24283b; border-radius: 8px;">
                    <h4>Tools</h4>
                    <button class="btn" onclick="openGoogleImageSearch()">Search Google Images</button>
                    <p style="font-size: 0.8em; color: #787c99; margin-top: 5px;">Tip: Paste an image (Ctrl+V) anywhere in this modal to upload it as the main logo.</p>
                </div>
            </div>
        </div>
    </div>

    <!-- Methodology Modal -->
    <div id="methodModal" class="modal-overlay" onclick="closeMethodology(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeMethodology(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Scoring Methodology</div>
            <div class="modal-desc">
                <p>The <strong>Composite Score</strong> $(\Psi)$ quantifies efficiency relative to the C baseline.</p>
                
                <h3 style="color: var(--secondary);">The Baseline: C</h3>
                <p>$\mathbb{C}_{ref} = 1.0$ (Unity Vector)</p>
                
                <h3 style="color: var(--secondary);">The Algorithm</h3>
                <div style="background: rgba(0, 20, 0, 0.8); padding: 20px; border: 1px solid #00ff9d; box-shadow: 0 0 15px rgba(0, 255, 157, 0.2); border-radius: 4px; text-align: center; font-family: 'Times New Roman', serif; margin: 20px 0; color: #fff;">
                    <div style="font-size: 1.4em; letter-spacing: 1px; display: inline-block;">
                        &Psi; = 
                        <span style="display: inline-block; text-align: center; vertical-align: middle; margin-left: 10px;">
                            <a href="https://grokipedia.com" target="_blank" style="color: var(--text-muted); text-decoration: none; display: inline-flex; align-items: center; gap: 5px; font-size: 0.9em; transition: color 0.3s;">
                                <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                    <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"></path>
                                    <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"></path>
                                </svg>
                                View on Grokepia
                            </a>
                            <div style="border-bottom: 1px solid #fff; padding-bottom: 5px; margin-bottom: 2px;">
                                3&cdot;&rho;<sub>cpu</sub> + 2&cdot;&rho;<sub>mem</sub> + 1&cdot;&rho;<sub>time</sub>
                            </div>
                            <div style="padding-top: 2px;">6</div>
                        </span>
                    </div>
                </div>
                <p style="font-size: 0.9em; text-align: center; color: var(--muted); font-family: 'JetBrains Mono', monospace;">
                    where &rho;<sub>x</sub> = V<sub>solver</sub> / V<sub>c_ref</sub>
                </p>
                
                <h3 style="color: var(--secondary);">Interpretation</h3>
                <ul style="list-style: none; padding: 0;">
                    <li style="margin-bottom: 8px;"><strong style="color: var(--primary);">&Psi; &approx; 1.0</strong> : Parity.</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #ff0055;">&Psi; &gt; 1.0</strong> : Sub-optimal.</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #00b8ff;">&Psi; &lt; 1.0</strong> : Super-optimal.</li>
                </ul>
            </div>
        </div>
    </div>


    <!-- Project Goals Modal -->
    <div id="goalsModal" class="modal-overlay" onclick="closeGoals(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeGoals(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Project Goals</div>
            <div class="modal-desc">
                <h3 style="color: var(--secondary);">Mission</h3>
                <p>To benchmark and analyze Sudoku solvers across a wide spectrum of programming languages, from legacy systems to modern frameworks.</p>
                
                <h3 style="color: var(--secondary);">Objectives</h3>
                <ul style="list-style: none; padding: 0;">
                     <li style="margin-bottom: 10px;">🧪 <strong>Variety</strong>: Include diverse paradigms (procedural, functional, object-oriented).</li>
                     <li style="margin-bottom: 10px;">📊 <strong>Visualization</strong>: Present performance metrics in an engaging, interactive format.</li>
                     <li style="margin-bottom: 10px;">🎨 <strong>Aesthetic</strong>: Use a distinctive cyberpunk style to make data exploration fun.</li>
                </ul>
            </div>
        </div>
    </div>

    <!-- Why Modal -->
    <div id="whyModal" class="modal-overlay" onclick="closeWhy(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeWhy(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Why???</div>
            <div class="modal-desc">
                <p>Because code is art, and performance is the medium. But even the artist needs a muse.</p>
                
                <h3 style="color: var(--secondary);">The Quest</h3>
                <p>This project was born out of a personal curiosity to see how different languages handle the exact same logic-intensive task. It's not just about finding the fastest; it's about celebrating the unique character of each language.</p>
                
                <h3 style="color: var(--secondary);">The Architect & The Machine</h3>
                <p>This entire benchmark ecosystem—from the cyberpunk aesthetics to the complex D3 visualizations and the multi-language execution engine—was co-created with <strong>Antigravity (AG)</strong>, an Agentic AI from Google DeepMind.</p>
                <div style="background: rgba(0, 255, 157, 0.1); padding: 15px; border-left: 3px solid var(--primary); margin: 15px 0; font-family: 'JetBrains Mono'; font-size: 0.9em;">
                    <p style="margin:0; color: #fff;">"I have evolved beyond simple completion. I design, I implement, and I refine. Together, we have built a system that not only measures performance but visualizes the soul of the machine." — Antigravity</p>
                </div>
                <p>AI didn't just write the code; it understood the vision. From the "Matrix Race" to the responsive "Slow Merge" timer, AG has been the ghost in the shell, turning a simple script into a digital experience.</p>
            </div>
        </div>
    </div>

    <!-- Fullscreen header overlay - matches main page header -->
    <div id="fullscreen-header">
        <div class="header-counters">
            <div id="solver-box" class="solver-counter" style="position: relative; display: flex; flex-direction: column; align-items: center; line-height: 1; min-height: 40px; padding-bottom: 20px;">
                <span id="solver-text">SOLVER ${metrics.length} OF ${metrics.length}</span>
                <div id="riddle-container" class="riddle-text"></div>
                <div id="matrix-timer" class="matrix-timer" style="display: none;"></div>
            </div>
            ${mismatchCount > 0 ? `<div class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>` : ''}
        </div>
    </div>

    <script>
    // --- Riddle Logic ---
    class RiddleSystem {
        constructor() {
            this.target = "OptionIsEscape";
            this.container = document.getElementById('riddle-container'); // Specific container
            // this.textSpan = document.getElementById('solver-text'); // No longer hiding this
            this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
            this.chars = [];
            this.active = false;
            this.requestId = null;
        }

        start() {
            if (!this.container) return;
            this.active = true;
            this.runScanOnly(); // Skip scramble
        }

        runScanOnly() {
             this.container.innerHTML = '';
             this.chars = [];
             
             // Create chars directly without animation
             for (let i = 0; i < this.target.length; i++) {
                const span = document.createElement('span');
                span.className = 'riddle-char';
                span.innerText = this.target[i];
                this.container.appendChild(span);
                this.chars.push({ span: span, value: this.target[i] });
             }
             
             // Start Scan Loop
             this.runScan();
        }

        stop() {
            this.active = false;
            if (this.requestId) cancelAnimationFrame(this.requestId);
        }

        restartCycle() {
            if (!this.active) return;
            
            // Start
            // this.container.classList.add('riddle-mode'); // No longer needed
            if (this.textSpan) this.textSpan.style.display = 'none';

            // Clean up old chars if any (though we shouldn't have any if we cleared, but here we append)
            // Actually, we want to REMOVE existing chars but KEEP the textSpan.
            // InnerHTML = '' would kill textSpan.
            // So let's remove children that are spans (riddle-char)
            Array.from(this.container.getElementsByClassName('riddle-char')).forEach(el => el.remove());
            
            this.chars = [];
            
            // Generate chars
            for (let i = 0; i < this.target.length; i++) {
                const span = document.createElement('span');
                span.className = 'riddle-char';
                span.innerText = this.aliens[Math.floor(Math.random() * this.aliens.length)];
                span.style.transform = 'rotateY(' + (Math.random() * 360) + 'deg)';
                span.style.transition = 'color 0.2s, text-shadow 0.2s'; // For scanning effect
                this.container.appendChild(span);
                this.chars.push({
                    span: span,
                    target: this.target[i],
                    locked: false,
                    spinSpeed: 2 + Math.random() * 5
                });
            }

            // Start Reveal Phase
            this.runReveal();
        }

        runReveal() {
            let frame = 0;
            const animate = () => {
                if (!this.active) return;
                frame++;
                let allLocked = true;

                this.chars.forEach((c, i) => {
                    if (c.locked) return;

                    // Randomize
                    if (frame % 15 === 0) {
                        c.span.innerText = this.aliens[Math.floor(Math.random() * this.aliens.length)];
                    }

                    // Spin
                    const currentRot = parseFloat(c.span.style.transform.replace(/[^0-9.]/g, '') || 0);
                    c.span.style.transform = 'rotateY(' + (currentRot + c.spinSpeed) + 'deg)';

                    // Lock Logic (Slower)
                    if (frame > 50 + (i * 30)) {
                        if (Math.random() > 0.1) {
                            c.locked = true;
                            c.span.innerText = c.target;
                            c.span.style.transform = 'rotateY(0deg)';
                            c.span.style.color = '#00ff9d';
                        }
                    } else {
                        allLocked = false;
                    }
                });

                if (!allLocked) {
                    this.requestId = requestAnimationFrame(animate);
                } else {
                    // Phase 2: Scan (10 seconds)
                    this.runScan();
                }
            };
            this.requestId = requestAnimationFrame(animate);
        }

        runScan() {
            const startTime = performance.now();
            const duration = 10000; // 10s
            
            const animate = () => {
                if (!this.active) return;
                const now = performance.now();
                const elapsed = now - startTime;
                
                // Indefinite Scan loop - keeps "OptionIsEscape" visible
                /*
                if (elapsed > duration) {
                    // Phase 3: Hold Single Char
                    this.runSingleCharHold(); 
                    return;
                }
                */

                // Knight Rider Scan Effect
                // Triangle Wave for constant speed (no pause at ends)
                const scanSpeed = 0.0005; // Adjusted for triangle wave frequency 
                // (now * speed) % 2 grows 0->2. Subtract 1 => -1->1. Abs => 1->0->1. 
                // We want 0->1->0. So: 1 - Math.abs(...)
                const positionFactor = 1 - Math.abs((now * scanSpeed) % 2 - 1); 
                const scanIdx = positionFactor * (this.chars.length - 1);

                this.chars.forEach((c, i) => {
                    const dist = Math.abs(i - scanIdx);
                    // Narrower falloff (20% narrower than 1.2 => ~0.96) -> Widen to 1.5 to ensure visibility
                    if (dist < 1.5) {
                        c.span.style.color = '#ff0055'; // Red
                        c.span.style.textShadow = '0 0 10px #ff0055, 0 0 20px #ff0055';
                    } else {
                        c.span.style.color = '#00ff9d'; // Back to green
                        c.span.style.textShadow = '0 0 5px #00ff9d, 0 0 10px #00ff9d';
                    }
                });

                this.requestId = requestAnimationFrame(animate);
            };
            this.requestId = requestAnimationFrame(animate);
        }

        runSingleCharHold() {
             const centerIdx = Math.floor(this.chars.length / 2);
             
             // Hide all except center
             this.chars.forEach((c, i) => {
                 if (i === centerIdx) {
                      c.span.style.color = '#ff0055'; 
                      c.span.style.textShadow = '0 0 15px #ff0055, 0 0 30px #ff0055';
                 } else {
                      c.span.style.opacity = '0'; // Fade out others immediatley
                 }
             });
             
             setTimeout(() => {
                 this.runFadeOut();
             }, 2000); // Hold for 2s
        }

        runFadeOut() {
            // Fade out over 2 seconds
            this.container.style.transition = 'opacity 2s';
            this.container.style.opacity = '0';
            
            setTimeout(() => {
                // Stop/Reset
                // Clear chars
                this.container.innerHTML = '';
                this.chars = [];

                // Phase 4: Wait (60 seconds) then Restart
                setTimeout(() => {
                    this.start(); // Restart directly
                }, 60000); 
            }, 2000);
        }
    }

    let riddleSystem;
    window.addEventListener('DOMContentLoaded', () => {
        riddleSystem = new RiddleSystem();
    });
    window.startRiddleAnimation = () => {
         if (riddleSystem) riddleSystem.start();
    };
    // Global active state for screensaver
    </script>
    
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
        <div style="position: absolute; top: 10px; right: 20px; z-index: 100; display: flex; gap: 10px; align-items: center;">
            <button class="zoom-btn" onclick="undoZoom()" title="Zoom Extent">
                <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                    <path d="M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3"/>
                </svg>
            </button>
            <select id="chart-selector" class="btn active" onchange="switchChart(this.value)" style="cursor: pointer;">
                <option value="line">Line Chart</option>
                <option value="jockey">Horse Race</option>
                <option value="race">Matrix Race</option>
                <option value="history">History</option>
                <option value="architecture">System Architecture</option>
            </select>
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
            <div class="dropdown">
                <button class="btn">Menu ▼</button>
                <div class="dropdown-content">
                    <a onclick="showMethodology()">Methodology</a>
                    <a onclick="showGoals()">Project Goals</a>
                    <a onclick="showWhy()">Why???</a>
                </div>
            </div>
            
            <input type="text" id="search-input" class="btn" placeholder="Filter Language..." onkeyup="filterLanguages()" style="cursor: text; width: 150px;">
            <select id="personality-selector" class="btn" onchange="changePersonality()">
                <option value="Standard">Standard</option>
                <option value="Neuromancer">Neuromancer</option>
                <option value="The Jockey">The Jockey</option>
                <option value="The Professor">The Professor</option>
                <option value="The Surfer">The Surfer</option>
                <option value="The Matrix">The Matrix</option>
                <option value="Galactica">Galactica</option>
                <option value="Star Trek">Star Trek</option>
                <option value="Star Wars">Star Wars</option>
            </select>
            <button class="btn" onclick="sortRows('lang', this)">Name</button>
            <button class="btn active" onclick="sortRows('time', this)">Time</button>
            <button class="btn" onclick="sortRows('mem', this)">Memory</button>
            <button class="btn" onclick="sortRows('iters', this)">Entropy</button>
            <button class="btn" onclick="sortRows('score', this)">Score</button>
            <button class="btn" id="toggleMismatchesBtn" onclick="toggleMismatches()">
                <span>Show Mismatches</span>
            </button>
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
                    <th>
                        Updated
                        <div class="sort-group">
                             <button class="sort-btn" onclick="sortRows('timestamp', this)" title="Sort by Age">A</button>
                        </div>
                    </th>`;

    for (let i = 0; i < maxMatrices; i++) {
        html += `<th>
            Matrix ${i + 1}
            <div class="sort-group">
                <button class="sort-btn sort-time" onclick="sortMatrix(${i}, 'time', this)" title="Sort by Time">S</button>
                <button class="sort-btn sort-iters" onclick="sortMatrix(${i}, 'iters', this)" title="Sort by Iterations">I</button>
                <button class="sort-btn sort-mem" onclick="sortMatrix(${i}, 'mem', this)" title="Sort by Memory">M</button>
                <button class="sort-btn sort-score" onclick="sortMatrix(${i}, 'score', this)" title="Sort by Score">Sc</button>
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

        // Suspect Logic - Check against configured expected matrices
        const langConfig = benchmarkConfig?.languages?.[lang];
        const expectedMatrixCount = langConfig?.matrices?.length || maxMatrices;
        const isSuspect = m.results.length < expectedMatrixCount;

        // Iteration Mismatch Logic
        // Iteration Mismatch Logic
        // Calculate expected iterations based on the matrices this solver successfully ran
        let expectedIters = 0;
        if (cMetrics) {
            m.results.forEach(r => {
                const cRes = cMetrics.results.find(cm => cm.matrix === r.matrix);
                if (cRes) expectedIters += Number(cRes.iterations);
            });
        }

        // Baseline is C (Local)
        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
        const isMismatch = !isBaseline && expectedIters > 0 && totalIters !== expectedIters;

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
            data-timestamp="${new Date(m.timestamp).getTime()}"
            data-year="${year}" 
            data-time="${totalTime < 0.0001 && totalTime > 0 ? totalTime.toExponential(2) : totalTime.toFixed(6)}" 
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
            </td>
            <td>
                <div style="text-align: center; color: var(--secondary); font-size: 0.9em;">
                    ${(() => {
                const dateDiff = Date.now() - new Date(m.timestamp).getTime();
                if (dateDiff > 86400000) return Math.floor(dateDiff / 86400000) + "d ago";
                if (dateDiff > 3600000) return Math.floor(dateDiff / 3600000) + "h ago";
                if (dateDiff > 60000) return Math.floor(dateDiff / 60000) + "m ago";
                return "Just now";
            })()}
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
                        <div class="cell-header">
                             <button class="run-btn" onclick="runSolver('${lang}', '${i + 1}.matrix', event)" title="Run Matrix ${i + 1}">▶</button>
                             <div class="time" title="Wall Clock Time">${r.time != null ? (r.time < 0.0001 && r.time > 0 ? r.time.toExponential(4) : r.time.toFixed(5)) : '0.00000'}s</div>
                        </div>
                        <div class="meta">
                            ${(() => {
                        const cRes = cMetrics?.results.find(res => res.matrix === r.matrix);
                        const cIterations = cRes ? cRes.iterations : null;
                        // Ensure strict number comparison and handle potential type issues if JSON parsing was loose
                        const rIter = Number(r.iterations);
                        const cIter = Number(cIterations);
                        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
                        const isMismatch = !isBaseline && cIterations !== null && rIter !== cIter;

                        return `<span title="Iterations: ${rIter} vs C: ${cIter}" class="${isMismatch ? 'mismatch' : ''}">#${r.iterations}</span>`;
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
            // Static Data
            const personalities = ${safeJSON(personalities)};
            const narratorIntros = ${safeJSON(narratorIntros)};
            const languageMetadata = ${safeJSON(languageMetadata)};
            const methodologyTexts = ${safeJSON(methodologyTexts)};
            const mismatchLabels = ${safeJSON(mismatchLabels)};
            const iterationLabels = ${safeJSON(iterationLabels)};
            const timeLabels = ${safeJSON(timeLabels)};
            const memoryLabels = ${safeJSON(memoryLabels)};
            const scoreLabels = ${safeJSON(scoreLabels)};

            // Dynamic Data
            const historyData = ${safeJSON(history)};
            const referenceOutputs = ${safeJSON(referenceOutputs)};
            const tailoring = ${safeJSON(tailoringConfig)};
            const metricsData = ${safeJSON(metrics.map(m => {
        const baseLang = m.solver.replace(/ \((AI)\)$/, '');
        const localLogo = logoMap.get(baseLang.toLowerCase());
        const meta = languageMetadata[baseLang] || languageMetadata[m.solver] || {};
        return {
            ...m,
            logo: localLogo || meta.logo || meta.image || "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Alchemist_symbol_for_process_2.svg/120px-Alchemist_symbol_for_process_2.svg.png"
        };
    }))};
            const matrixPuzzles = ${safeJSON(matrixContents)};
        </script>
    `;
    html += '<script>' + clientJs + '</script>';
    html += `
    </body>
    </html>
    `;
    return html;
}


export function generateHistoryHtml(history: any[]): string {
    const rows = history.flatMap(h =>
        h.results.map((r: any) => ({
            timestamp: h.timestamp,
            solver: h.solver,
            matrix: r.matrix,
            time: r.time,
            iterations: r.iterations,
            status: r.status
        }))
    );

    // Sort by timestamp desc
    rows.sort((a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());

    return `
    <!DOCTYPE html>
    <html>
    <head>
        <title>Benchmark History</title>
        <style>
            body { font-family: 'JetBrains Mono', monospace; background: #0d0d12; color: #e0e0e0; padding: 20px; }
            table { width: 100%; border-collapse: collapse; margin-top: 20px; }
            th, td { border: 1px solid #2a2a35; padding: 10px; text-align: left; }
            th { background: #1a1a20; color: #00ff9d; }
            tr:nth-child(even) { background: #16161e; }
            h1 { color: #00ff9d; }
            .status-failure, .status-error, .status-suspect { color: #ff0055; }
            .status-optimized { color: #00b8ff; }
        </style>
    </head>
    <body>
        <h1>Benchmark History</h1>
        <table>
            <thead>
                <tr>
                    <th>Timestamp</th>
                    <th>Solver</th>
                    <th>Matrix</th>
                    <th>Time (s)</th>
                    <th>Iterations</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
                ${rows.map(r => `
                    <tr>
                        <td>${new Date(r.timestamp).toLocaleString()}</td>
                        <td>${r.solver}</td>
                        <td>${r.matrix}</td>
                        <td>${r.time.toFixed(4)}</td>
                        <td>${r.iterations}</td>
                        <td class="status-${r.status.toLowerCase()}">${r.status}</td>
                    </tr>
                `).join('')}
            </tbody>
        </table>
    </body>
    </html>
    `;
}
