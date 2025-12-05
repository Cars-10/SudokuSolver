import * as fs from 'fs/promises';
import * as path from 'path';
import { glob } from 'glob';
import { fileURLToPath } from 'url';
import type { SolverMetrics } from './types.ts';
import { languageHistories, quotes, personalities, methodologyTexts, languageMetadata, narratorIntros } from './LanguagesMetadata.ts';
export { languageHistories, quotes, personalities, methodologyTexts, languageMetadata, narratorIntros };

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);



// --- Logic ---



export async function generateHtml(metrics: SolverMetrics[], history: any[], personalities: any, languageMetadata: any, methodologyTexts: any, referenceOutputs: any, allowedMatrices: string[] = []): Promise<string> {
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

    // Calculate mismatch count - only compare iterations for matrices both ran
    let mismatchCount = 0;
    if (cMetrics && cMetrics.results.length > 0) {
        const cItersByMatrix = new Map(cMetrics.results.map(r => [r.matrix, r.iterations]));
        mismatchCount = metrics.filter(m => {
            if (m.solver === 'C') return false;
            // Only compare iterations for matrices this solver actually ran
            for (const result of m.results) {
                const cIters = cItersByMatrix.get(result.matrix);
                if (cIters !== undefined && result.iterations !== cIters) {
                    return true; // Mismatch found
                }
            }
            return false; // All matching matrices have correct iterations
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
            font-size: 1.5em;
            margin: 5px 0;
            perspective: 1000px;
            cursor: default;
            text-align: center;
            width: 100%;
            position: relative;
            z-index: 10005; /* On top of everything */
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
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <canvas id="matrix-canvas"></canvas>
    <div id="tooltip"></div>
    
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
                            <p id="modalBenefits" class="view-only" style="margin: 5px 0 0 0; color: #bb9af7; font-size: 0.9em;"></p>
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
            <div class="solver-counter">
                SOLVER ${metrics.length} OF ${metrics.length}
            </div>
             <div id="matrix-timer" class="matrix-timer" style="display: none;"></div>
            <div id="riddle-container" class="riddle-text"></div>
            ${mismatchCount > 0 ? `<div class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>` : ''}
        </div>
    </div>

    <script>
    // --- Riddle Logic ---
    class RiddleSystem {
        constructor() {
            this.target = "OptionIsEscape";
            this.container = document.getElementById('riddle-container');
            this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
            this.chars = [];
            this.active = false;
            this.requestId = null;
        }

        start() {
            if (!this.container) return;
            this.active = true;
            this.restartCycle();
        }

        stop() {
            this.active = false;
            if (this.requestId) cancelAnimationFrame(this.requestId);
        }

        restartCycle() {
            if (!this.active) return;
            
            // Reset container
            this.container.innerHTML = '';
            this.container.style.opacity = '1';
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
                
                if (elapsed > duration) {
                    // Phase 3: Hold Single Char
                    this.runSingleCharHold(); 
                    return;
                }

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
                // Phase 4: Wait (60 seconds) then Restart
                setTimeout(() => {
                    this.restartCycle();
                }, 60000); 
            }, 2000);
        }
    }

    const riddleSystem = new RiddleSystem();
    window.startRiddleAnimation = () => riddleSystem.start();
    const timerSystem = new MatrixTimerSystem();
    window.startTimerAnimation = () => timerSystem.start();
    window.matrixTimerSystem = timerSystem;

    document.addEventListener('keydown', (e) => {
        if (!active) return;
        if (e.key === 'Control') {
            if (currentMode === 'red') puzzleVisible = !puzzleVisible;
        }
        if (e.key === 'Shift') {
            // Show elapsed time since screensaver started? Or just some numbers.
            // Let's show elapsed time since page load for now, or just random.
            // User said "load the elapsed time".
            const elapsed = (performance.now() / 1000).toFixed(1);
            timerSystem.triggerText(elapsed);
        }
    });
    let puzzleVisible = true;
    
    // --- Matrix Timer Logic ---
    class MatrixTimerSystem {
        constructor() {
            this.container = document.getElementById('matrix-timer');
            this.active = false;
            this.visible = false;
            this.introDone = false;
            this.lastTriggerTime = 0;
            this.interval = 600000; // 10 minutes
            this.showDuration = 10000; 
            this.currentDigit = 6; 
            this.particles = [];
            
            // 5x7 Bitmaps
            this.digits = {
                "0": [
                    0,1,1,1,0,
                    1,0,0,0,1,
                    1,0,0,1,1,
                    1,0,1,0,1,
                    1,1,0,0,1,
                    1,0,0,0,1,
                    0,1,1,1,0
                ],
                "1": [
                    0,0,1,0,0,
                    0,1,1,0,0,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,0,1,0,0,
                    0,1,1,1,0
                ],
                "2": [
                    0,1,1,1,0,
                    1,0,0,0,1,
                    0,0,0,0,1,
                    0,0,0,1,0,
                    0,0,1,0,0,
                    0,1,0,0,0,
                    1,1,1,1,1
                ],
                "3": [
                    0,1,1,1,0,
                    1,0,0,0,1,
                    0,0,0,0,1,
                    0,0,1,1,0,
                    0,0,0,0,1,
                    1,0,0,0,1,
                    0,1,1,1,0
                ],
                "4": [
                    0,0,0,1,0,
                    0,0,1,1,0,
                    0,1,0,1,0,
                    1,0,0,1,0,
                    1,1,1,1,1,
                    0,0,0,1,0,
                    0,0,0,1,0
                ],
                "5": [
                    1,1,1,1,1,
                    1,0,0,0,0,
                    1,1,1,1,0,
                    0,0,0,0,1,
                    0,0,0,0,1,
                    1,0,0,0,1,
                    0,1,1,1,0
                ],
                "6": [
                    0,1,1,1,0,
                    1,0,0,0,0,
                    1,0,0,0,0,
                    1,1,1,1,0,
                    1,0,0,0,1,
                    1,0,0,0,1,
                    0,1,1,1,0
                ],
                ".": [
                    0,0,0,0,0,
                    0,0,0,0,0,
                    0,0,0,0,0,
                    0,0,0,0,0,
                    0,0,0,0,0,
                    0,0,1,0,0, // Dot
                    0,0,0,0,0
                ]
            };
            
            this.aliens = "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789";
            
            const particleCount = 3500; // Increased for "0.0"
            for(let i=0; i<particleCount; i++) {
                const el = document.createElement('div');
                el.className = 'timer-particle';
                el.innerText = this.randomChar();
                this.container.appendChild(el);
                this.particles.push({
                    el: el,
                    x: 0,
                    y: 0,
                    targetX: null,
                    targetY: null,
                    state: 'idle', 
                    startX: 0,
                    startY: 0,
                    progress: 0
                });
            }
        }
        
        randomChar() {
            return this.aliens[Math.floor(Math.random() * this.aliens.length)];
        }
        
        start() {
            this.active = true;
            this.visible = false;
            this.introDone = false;
            this.currentDigit = 7;
            
            // Start with "0.0" immediately
            this.show();
            this.triggerText("0.0");
            this.lastTriggerTime = performance.now(); // Intro start
            
            this.container.style.display = 'block'; 
            this.animate();
        }
        
        stop() {
            this.active = false;
            this.container.style.display = 'none';
        }
        
        show() {
            this.visible = true;
            this.container.style.display = 'block';
            this.visibleStartTime = performance.now();
            
            const w = this.container.clientWidth;
            const h = this.container.clientHeight;
            
            this.particles.forEach(p => {
                p.x = Math.random() * w;
                p.y = Math.random() * h;
                p.state = 'field'; 
                p.el.style.opacity = 1;
                elUpdate(p);
            });
        }
        
        hide() {
            this.visible = false;
            this.container.style.display = 'none';
        }
        
        triggerDigit(num) {
            this.triggerText(num.toString());
        }
        
        triggerText(text) {
            const w = this.container.clientWidth;
            const h = this.container.clientHeight;
            
            // Calculate total width
            const chars = text.split('');
            const scale = Math.min(w, h) / 8; // Adjust scale for multi-char
            const totalGridW = chars.length * 5 * scale + (chars.length - 1) * scale; // 1px gap (scaled)
            
            let currentX = (w - totalGridW) / 2;
            const offsetY = (h - (7 * scale)) / 2;
            
            let usedParticles = 0;
            const particlesPerPixel = 20; 
            
            this.particles.forEach(p => {
                p.targetX = null;
                p.targetY = null;
                p.startX = p.x;
                p.startY = p.y;
                p.progress = 0;
                p.speed = 0.005 + Math.random() * 0.01;
            });
            
            chars.forEach(char => {
                const grid = this.digits[char];
                if (grid) {
                    for(let row=0; row<7; row++) {
                        for(let col=0; col<5; col++) {
                            if(grid[row*5 + col]) {
                                for(let k=0; k<particlesPerPixel; k++) {
                                    if(usedParticles >= this.particles.length) break;
                                    const p = this.particles[usedParticles++];
                                    p.targetX = currentX + col * scale + Math.random() * scale;
                                    p.targetY = offsetY + row * scale + Math.random() * scale;
                                    p.state = 'merging'; 
                                }
                            }
                        }
                    }
                }
                currentX += 6 * scale; // 5 width + 1 gap
            });
            
            for(let i=usedParticles; i<this.particles.length; i++) {
                this.particles[i].state = 'dissolve';
            }
        }
        
        animate() {
            if (!this.active) return;
            
            const now = performance.now();
            
            if (!this.introDone) {
                 // Handling Intro "0.0"
                 if (this.visible && (now - this.visibleStartTime > this.showDuration)) {
                     this.hide();
                     this.introDone = true;
                     this.lastTriggerTime = now; // Reset trigger for next interval
                     // Start countdown sequence from 6 next time? Yes.
                 }
            } else {
                // Regular Interval
                if (now - this.lastTriggerTime > this.interval) {
                    this.show();
                    this.currentDigit--;
                    if (this.currentDigit < 1) this.currentDigit = 6;
                    this.triggerDigit(this.currentDigit);
                    this.lastTriggerTime = now;
                }
                
                if (this.visible && (now - this.visibleStartTime > this.showDuration)) {
                    this.hide();
                }
            }
            
            if (this.visible) {
                const w = this.container.clientWidth;
                const h = this.container.clientHeight;
                
                this.particles.forEach(p => {
                    if (Math.random() < 0.05) p.el.innerText = this.randomChar();
                    
                    if (p.state === 'field') {
                        p.x += (Math.random() - 0.5) * 5;
                        p.y += (Math.random() - 0.5) * 5;
                        if(p.x < 0) p.x = w; if(p.x > w) p.x = 0;
                        if(p.y < 0) p.y = h; if(p.y > h) p.y = 0;
                        
                    } else if (p.state === 'merging') {
                        const dx = p.targetX - p.x;
                        const dy = p.targetY - p.y;
                        p.x += dx * p.speed;
                        p.y += dy * p.speed;
                        if (Math.abs(dx) < 1 && Math.abs(dy) < 1) {
                            p.x = p.targetX;
                            p.y = p.targetY;
                            p.state = 'hold';
                        }
                    } else if (p.state === 'hold') {
                        p.x += (Math.random() - 0.5);
                        p.y += (Math.random() - 0.5);
                    } else if (p.state === 'dissolve') {
                         p.el.style.opacity = Math.max(0, parseFloat(p.el.style.opacity || 1) - 0.01);
                         p.x += (Math.random() - 0.5) * 2;
                         p.y += (Math.random() - 0.5) * 2;
                    }
                    elUpdate(p);
                });
            }
            requestAnimationFrame(() => this.animate());
        }
    }

    const timerSystem = new MatrixTimerSystem();

    window.startRiddleAnimation = startRiddleAnimation;
    window.startTimerAnimation = () => timerSystem.start();
    window.stopTimerAnimation = () => timerSystem.stop();
    
    // I should call these from startScreensaver / stopScreensaver
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
        <div style="position: absolute; top: 10px; right: 20px; z-index: 100; display: flex; gap: 10px;">
            <select id="chart-selector" class="btn active" onchange="switchChart(this.value)" style="cursor: pointer;">
                <option value="line">Line Chart</option>
                <option value="jockey">Jockey Leaderboard</option>
                <option value="race">Matrix Race (Bar Chart)</option>
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
                    <a href="benchmark_history.html" target="_blank">View History</a>
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
                <option value="Battlestar Galactica">Battlestar Galactica</option>
                <option value="Star Trek">Star Trek</option>
                <option value="Star Wars">Star Wars</option>
            </select>
            <button class="btn" onclick="sortRows('lang', this)">Name</button>
            <button class="btn active" onclick="sortRows('time', this)">Time (Fastest)</button>
            <button class="btn" onclick="sortRows('mem', this)">Memory (Highest)</button>
            <button class="btn" onclick="sortRows('iters', this)">Iterations</button>
            <button class="btn" onclick="sortRows('score', this)">Total Score</button>
            <button class="btn" id="toggleMismatchesBtn" onclick="toggleMismatches()">
                <span>Filter Mismatches</span>
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

        // Iteration Mismatch Logic - compare only matrices this solver ran
        const cItersByMatrix = cMetrics ? new Map(cMetrics.results.map(r => [r.matrix, r.iterations])) : new Map();
        // Baseline is C (Local)
        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
        // Check if any matrix has wrong iterations (only for matrices this solver ran)
        const isMismatch = !isBaseline && m.results.some(r => {
            const cIters = cItersByMatrix.get(r.matrix);
            return cIters !== undefined && r.iterations !== cIters;
        });

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

                        <div class="time" title="Wall Clock Time">${r.time < 0.0001 && r.time > 0 ? r.time.toExponential(4) : r.time.toFixed(5)}s</div>
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
            const narratorIntros = ${JSON.stringify(narratorIntros)};
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
                intro.innerText = narratorIntros[persona] || narratorIntros['Standard'] || "Welcome to the Sudoku Benchmark.";

                // Update Tooltips
                const rows = document.querySelectorAll('tbody tr');
                rows.forEach(row => {
                    const lang = row.getAttribute('data-lang');
                    const quotes = personalities[persona] || personalities['Standard'];
                    let quote = quotes[lang] || quotes['default'] || "Unknown.";
                    
                    // Append Efficiency
                    const score = row.getAttribute('data-score');
                    let time = parseFloat(row.getAttribute('data-time'));
                    let timeStr = time < 0.0001 && time > 0 ? time.toExponential(2) + "s" : time.toFixed(6) + "s";
                    quote += " Efficiency: " + parseFloat(score).toFixed(2) + " MB/s | Time: " + timeStr;
                    
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
            let currentEditingLang = null;
            let currentMetadata = null;

            // Updated Show Function
            window.showLanguageDetails = async function(lang) {
                console.log("Opening modal for:", lang);
                currentEditingLang = lang;
                const modal = document.getElementById('langModal');
                const modalContent = document.getElementById('modalContent');
                
                // Fetch dynamic metadata from backend first? 
                // We fallback to static if fetch fails.
                let meta = languageMetadata[lang] || {};
                
                try {
                    const res = await fetch(\`http://localhost:3000/api/metadata/\${lang}\`);
    if (res.ok) {
        const dynamicMeta = await res.json();
        // Merge: dynamic takes precedence, but keep static defaults if missing
        meta = { ...meta, ...dynamicMeta };
    }
} catch (e) {
    console.warn("Could not fetch dynamic metadata:", e);
}

currentMetadata = meta;

// View Mode Population
const img = meta.image || meta.logo || "";
document.getElementById('modalImg').src = img;

const displayName = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
document.getElementById('modalTitle').innerText = displayName;
document.getElementById('modalSubtitle').innerText = (meta.creator || "?") + " • " + (meta.date || "????");
document.getElementById('modalLocation').innerText = "📍 " + (meta.location || "Unknown Location");
document.getElementById('modalBenefits').innerText = "✨ " + (meta.benefits || "Unknown Benefits");
document.getElementById('modalDesc').innerText = meta.description || "No description available.";

// Edit Mode Population
document.getElementById('editInputs-title').value = displayName;
document.getElementById('editInputs-creator').value = meta.creator || "";
document.getElementById('editInputs-image').value = meta.image || meta.logo || "";
document.getElementById('editInputs-date').value = meta.date || "";
document.getElementById('editInputs-location').value = meta.location || "";
document.getElementById('editInputs-benefits').value = meta.benefits || "";
document.getElementById('editInputs-desc').value = meta.description || "";

// Authors Population
const authorList = document.getElementById('authorList');
authorList.innerHTML = '';

const authors = meta.authors || [];
// If no specific authors list, use creator + main image as one entry? or just rely on main header?
// Let's rely on main header for primary, but allow adding more.

authors.forEach((auth, idx) => {
    const div = document.createElement('div');
    div.className = 'author-item';
    div.innerHTML = \`
                        <img src="\${auth.image || ''}" class="author-img">
                        <span class="view-only">\${auth.name}</span>
                        <input type="text" class="modal-edit-input edit-only" value="\${auth.name}" placeholder="Name" onchange="updateAuthor(\${idx}, 'name', this.value)">
                        <input type="text" class="modal-edit-input edit-only" value="\${auth.image}" placeholder="Image URL" onchange="updateAuthor(\${idx}, 'image', this.value)">
                        <button class="btn edit-only" style="background:#ff5555; padding: 2px 5px; font-size: 0.7em;" onclick="removeAuthor(\${idx})"> Remove </button>
                    \`;
    authorList.appendChild(div);
});

modalContent.classList.remove('editing');
document.getElementById('editBtn').innerText = "Edit";
modal.style.display = 'flex';
            };

window.toggleEditMode = function () {
    const content = document.getElementById('modalContent');
    const btn = document.getElementById('editBtn');
    if (content.classList.contains('editing')) {
        content.classList.remove('editing');
        btn.innerText = "Edit";
    } else {
        content.classList.add('editing');
        btn.innerText = "Cancel";
    }
};

window.toggleLock = async function () {
    if (!currentEditingLang) return;
    
    const btn = document.getElementById('lockBtn');
    const isCurrentlyLocked = btn.innerText.includes('Locked 🔒');
    const newLockState = !isCurrentlyLocked;
    
    try {
        const res = await fetch('http://localhost:9101/api/lock', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ lang: currentEditingLang, locked: newLockState })
        });

        if (res.ok) {
            btn.innerText = newLockState ? '🔒 Locked' : '🔓 Unlocked';
            btn.style.background = newLockState ? '#4caf50' : '';
            btn.style.color = newLockState ? '#fff' : '';
        } else {
            alert("Error locking: " + res.statusText);
        }
    } catch (e) {
        alert("Error locking: " + e.message + ". Is ContentServer running?");
    }
};

window.saveLanguageDetails = async function () {
    if (!currentEditingLang) return;

    const newData = {
        creator: document.getElementById('editInputs-creator').value,
        image: document.getElementById('editInputs-image').value,
        date: document.getElementById('editInputs-date').value,
        location: document.getElementById('editInputs-location').value,
        benefits: document.getElementById('editInputs-benefits').value,
        description: document.getElementById('editInputs-desc').value,
        authors: currentMetadata.authors || []
    };

    // Save to backend
    try {
        const res = await fetch('http://localhost:3000/api/save-metadata', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ lang: currentEditingLang, metadata: newData })
        });

        if (res.ok) {
            alert("Saved!");
            window.showLanguageDetails(currentEditingLang); // Refresh
        } else {
            alert("Error saving: " + res.statusText);
        }
    } catch (e) {
        alert("Error saving: " + e.message);
    }
};

window.addAuthorField = function () {
    if (!currentMetadata.authors) currentMetadata.authors = [];
    currentMetadata.authors.push({ name: "New Author", image: "" });
    window.showLanguageDetails(currentEditingLang); // Re-render triggers populate
    document.getElementById('modalContent').classList.add('editing'); // Keep edit mode
};

window.removeAuthor = function (idx) {
    if (!currentMetadata.authors) return;
    currentMetadata.authors.splice(idx, 1);
    window.showLanguageDetails(currentEditingLang);
    document.getElementById('modalContent').classList.add('editing');
};

window.updateAuthor = function (idx, field, value) {
    if (!currentMetadata.authors[idx]) return;
    currentMetadata.authors[idx][field] = value;
};

window.openGoogleImageSearch = function () {
    if (currentEditingLang) {
        window.open(\`https://www.google.com/search?tbm=isch&q=\${encodeURIComponent(currentEditingLang + " programming language logo")}\`, '_blank');
    }
};

// Paste Listener for Modal
document.addEventListener('paste', async (e) => {
    // Only if modal is open and editing
    const modal = document.getElementById('langModal');
    const modalContent = document.getElementById('modalContent');
    if (modal.style.display !== 'flex' || !modalContent.classList.contains('editing')) return;

    const items = (e.clipboardData || e.originalEvent.clipboardData).items;
    for (let index in items) {
        const item = items[index];
        if (item.kind === 'file') {
            const blob = item.getAsFile();
            const formData = new FormData();
            formData.append('file', blob);
            formData.append('lang', currentEditingLang);

            try {
                const res = await fetch('http://localhost:3000/api/upload-media', {
                    method: 'POST',
                    body: formData
                });
                if (res.ok) {
                    const data = await res.json();
                    // Assuming response gives us the local path or URL
                    // We need to serve this file now. 
                    // Since we don't have a file server for CleanedUp yet in the browser (file:// protocol),
                    // we might need to rely on the backend to Serve static files or just assume the path.
                    // For now, let's just update the metdata image path to the absolute path? 
                    // No, browser can't load partial absolute paths easily if security restricted.
                    // But this is a local report. 
                    // "CleanedUp/Languages/..."
                    const relativePath = \`CleanedUp/Languages/\${currentEditingLang}/Media/\${data.filename}\`;

                    // Update Main Logo?
                    // If user pasted, they probably want it as the main logo.
                    // Or we could ask? Let's just set as Main Logo for now.

                    // We need to update existing metadata locally
                    currentMetadata.image = relativePath;
                    document.getElementById('modalImg').src = relativePath;

                    const updateRes = await fetch('http://localhost:3000/api/save-metadata', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ lang: currentEditingLang, metadata: { ...currentMetadata, image: relativePath } })
                    });

                }
            } catch (e) {
                console.error("Upload failed", e);
            }
        }
    }
});

// Upload Input
window.uploadLogo = async function (input) {
    if (input.files && input.files[0]) {
        const formData = new FormData();
        formData.append('file', input.files[0]);
        formData.append('lang', currentEditingLang);

        try {
            const res = await fetch('http://localhost:3000/api/upload-media', {
                method: 'POST',
                body: formData
            });
            if (res.ok) {
                const data = await res.json();
                const relativePath = \`CleanedUp/Languages/\${currentEditingLang}/Media/\${data.filename}\`;
                currentMetadata.image = relativePath;
                document.getElementById('modalImg').src = relativePath;
            }
        } catch (e) {
            alert("Upload failed");
        }
    }
};


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

function showGoals() {
    document.getElementById('goalsModal').style.display = 'flex';
}

function closeGoals(event) {
    if (event.target.id === 'goalsModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('goalsModal').style.display = 'none';
    }
}

function showWhy() {
    document.getElementById('whyModal').style.display = 'flex';
}

function closeWhy(event) {
    if (event.target.id === 'whyModal' || event.target.classList.contains('modal-close')) {
        document.getElementById('whyModal').style.display = 'none';
    }
}

// Initialize
toggleMismatches(); // Default hide

// --- D3.js Chart Implementation ---
(function () {
    // Inject metrics with logo data
    const historyData = ${JSON.stringify(history)
        };
const referenceOutputs = ${JSON.stringify(referenceOutputs)};
const tailoring = ${JSON.stringify(tailoringConfig)};
const metricsData = ${JSON.stringify(metrics.map(m => {
            const baseLang = m.solver.replace(/ \((AI)\)$/, '');
            const localLogo = logoMap.get(baseLang.toLowerCase());
            const meta = languageMetadata[baseLang] || languageMetadata[m.solver] || {};
            return {
                ...m,
                logo: localLogo || meta.logo || meta.image || "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Alchemist_symbol_for_process_2.svg/120px-Alchemist_symbol_for_process_2.svg.png"
            };
        }))};

let data = metricsData;
const allTimes = data.flatMap(d => d.results.map(r => r ? r.time : 999999)).filter(t => t < 999999);
const minTime = allTimes.length ? Math.min(...allTimes) : 0.001;
const maxTime = allTimes.length ? Math.max(...allTimes) : 100;

let currentChart = 'line';

// Expose switchChart globally
// Expose switchChart globally - DEFINED EARLY TO PREVENT REFERENCE ERRORS
// Expose switchChart globally
let raceTicker = null;

window.switchChart = function (type) {
    try {
        currentChart = type;

        // Stop race if running
        if (raceTicker) {
            raceTicker.stop();
            raceTicker = null;
        }

        // Update Dropdown UI
        const selector = document.getElementById('chart-selector');
        if (selector) selector.value = type;

        // Check if D3 is loaded
        if (typeof d3 === 'undefined') {
            throw new Error("D3.js library not loaded. Please check your internet connection.");
        }

        const container = d3.select("#d3-chart-container");
        container.selectAll("*").remove();

        if (type === 'line') {
            drawLineChart();
        } else if (type === 'jockey') {
            drawJockeyChart();
        } else if (type === 'race') {
            drawMatrixRace();
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

function drawMatrixRace() {
    const container = document.getElementById('d3-chart-container');
    const width = container.clientWidth;
    const height = container.clientHeight;
    const margin = { top: 40, right: 40, bottom: 20, left: 150 };

    const svg = d3.select("#d3-chart-container")
        .append("svg")
        .attr("width", width)
        .attr("height", height);

    const raceDuration = 15000; // 15s
    const topN = 15;

    // X scale: 0 to 5 (Matrices)
    const x = d3.scaleLinear()
        .domain([0, 5])
        .range([margin.left, width - margin.right]);

    // Y scale: Rank
    const y = d3.scaleBand()
        .domain(d3.range(topN))
        .range([margin.top, height - margin.bottom])
        .padding(0.1);

    // Solver processing
    const solvers = data
        .filter(d => d.results && d.results.length >= 5)
        .map(d => {
        let cum = 0;
        const checkpoints = d.results.map(r => {
            cum += r.time;
            return { time: r.time, cum: cum, matrix: r.matrix };
        });
        return {
            solver: d.solver,
            checkpoints: checkpoints,
            totalTime: cum,
            progress: 0,
            logo: d.logo || null
        };
    });

    // Race Clock
    const logMin = Math.log(minTime / 10 || 0.0001);
    const logMax = Math.log(maxTime * 1.5 || 100);

    // Initial Draw
    let bars = svg.append("g").selectAll("g");

    // Axis
    svg.append("g")
        .attr("transform", "translate(0," + margin.top + ")")
        .call(d3.axisTop(x).ticks(5).tickFormat(d => "Matrix " + d))
        .attr("color", "#5c5c66")
        .selectAll("text")
        .attr("font-family", "JetBrains Mono")
        .attr("font-size", "12px")
        .attr("color", "#e0e0e0");

    // Current Time Label
    const timeLabel = svg.append("text")
        .attr("x", width - margin.right)
        .attr("y", height - 20)
        .attr("text-anchor", "end")
        .attr("fill", "#00ff9d")
        .attr("font-size", "24px")
        .attr("font-family", "JetBrains Mono")
        .text("0.00s");

    // Slider Controls
    const controls = d3.select("#d3-chart-container")
        .append("div")
        .attr("id", "race-controls")
        .style("position", "absolute")
        .style("bottom", "50px") 
        .style("left", "50%")
        .style("transform", "translateX(-50%)")
        .style("width", "60%")
        .style("display", "flex")
        .style("align-items", "center")
        .style("gap", "15px")
        .style("background", "rgba(13, 13, 18, 0.8)")
        .style("padding", "10px 20px")
        .style("border", "1px solid #00ff9d")
        .style("border-radius", "20px")
        .style("z-index", "100");

    controls.append("span")
        .style("color", "#00ff9d")
        .style("font-family", "JetBrains Mono")
        .style("font-size", "0.9em")
        .text("TIME TRAVEL");

    const slider = controls.append("input")
        .attr("type", "range")
        .attr("min", 0)
        .attr("max", raceDuration)
        .attr("value", 0)
        .style("flex-grow", "1")
        .style("cursor", "pointer")
        .style("accent-color", "#00ff9d");

    let isPaused = false;
    let currentElapsed = 0;
    let lastFrameTime = 0;

    slider.on("input", function(event) {
        isPaused = true;
        currentElapsed = +this.value;
        updateFrame(currentElapsed);
    });

    // Update Function
    function updateFrame(elapsed) {
        // Clamp
        if (elapsed > raceDuration) elapsed = raceDuration;
        if (elapsed < 0) elapsed = 0;

        // Calculate Sim Time
        const normT = elapsed / raceDuration;
        const logCurrent = logMin + normT * (logMax - logMin);
        const simTime = Math.exp(logCurrent);

        // Update Solvers
        solvers.forEach(s => {
            let prog = 0;
            // Find where simTime lands
            // Checkpoints: [ {time: t1, cum: c1}, {time: t2, cum: c2}, ... ]
            /* 
               If simTime < c1: prog = 0 + (simTime - 0) / t1
               If c1 < simTime < c2: prog = 1 + (simTime - c1) / t2
            */
            let prevCum = 0;
            for (let i = 0; i < s.checkpoints.length; i++) {
                const cp = s.checkpoints[i];
                if (simTime < cp.cum) {
                    const fraction = (simTime - prevCum) / cp.time;
                    prog = i + fraction;
                    break;
                } else {
                    prog = i + 1; // Completed this matrix
                    prevCum = cp.cum;
                }
            }
            // Clamp to 5.0
            if (prog > 5) prog = 5;
            s.progress = prog;
        });

        // Rank and Slice
        const ranked = solvers.sort((a, b) => b.progress - a.progress).slice(0, topN);

        // Bind Data (Key by solver name)
        const group = svg.selectAll(".bar-group")
            .data(ranked, d => d.solver);

        // Enter
        const groupEnter = group.enter().append("g")
            .attr("class", "bar-group")
            .attr("transform", (d, i) => "translate(0," + y(i) + ")");

        groupEnter.append("rect")
            .attr("x", x(0))
            .attr("height", y.bandwidth())
            .attr("fill", d => color(d.solver))
            .attr("opacity", 0.8)
            .attr("width", 0); 

        // Label
        groupEnter.append("text")
            .attr("class", "label")
            .attr("y", y.bandwidth() / 2)
            .attr("dy", "0.35em")
            .attr("text-anchor", "end")
            .attr("fill", "#e0e0e0")
            .attr("font-family", "JetBrains Mono")
            .attr("font-size", "12px")
            .text(d => d.solver);
        
        // Icon
        groupEnter.append("image")
            .attr("class", "icon")
            .attr("href", d => d.logo)
            .attr("height", y.bandwidth())
            .attr("width", y.bandwidth())
            .attr("x", margin.left - 140) 
            .attr("y", 0);

        // Update
        const groupUpdate = group.merge(groupEnter);

        // Immediate update for slider dragging (no transition)
        // If paused (scrubbing), use 0 duration. If playing, use small duration.
        const dura = isPaused ? 0 : 50; 
        const ease = d3.easeLinear;

        groupUpdate.transition().duration(dura).ease(ease)
            .attr("transform", (d, i) => "translate(0," + y(i) + ")");

        groupUpdate.select("rect")
            .transition().duration(dura).ease(ease)
            .attr("width", d => x(d.progress) - x(0));

        groupUpdate.select("text")
             .attr("x", margin.left - 10);

        // Exit
        group.exit().remove();

        // Update Clock Label
        timeLabel.text(simTime.toFixed(4) + "s");
        
        // Sync Slider if playing
        if (!isPaused) {
            slider.property("value", elapsed);
        }
    }

    // Animation Loop
    raceTicker = d3.timer((now) => {
        if (!lastFrameTime) lastFrameTime = now;
        const dt = now - lastFrameTime;
        lastFrameTime = now;

        if (!isPaused) {
            currentElapsed += dt;
            if (currentElapsed > raceDuration) {
                currentElapsed = raceDuration; // Don't stop, just stay at end? Or loop?
                // Previously it stopped. Let's start pause state at end.
                isPaused = true; 
            }
            updateFrame(currentElapsed);
        }
    });
}

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
    const minTime = ${Math.min(...metrics.flatMap(m => m.results.map(r => r.time)).filter(t => t > 0))
        };
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
        .filter(function (event) {
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
                const baseLang = solver.solver.replace(/ \((AI)\)$/, '');
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
        .filter(function (event) {
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
            const timeStr = d.totalTime < 0.0001 && d.totalTime > 0 ? d.totalTime.toExponential(4) + "s" : d.totalTime.toFixed(4) + "s";
            tooltip.innerHTML = "<strong style='color:" + color(d.solver) + "'>" + d.solver + "</strong><br>Total Time: " + timeStr;
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

            // Initialize drops and state
            drops = [];
            cars10State = []; // Track index for each column
            for (let i = 0; i < columns; i++) {
                drops[i] = Math.random() * -100; // Start above screen
                cars10State[i] = -1; // -1 means normal random char
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
        let cars10State = []; // Added state array
        const secretMessage = "/Cars10 "; // Normal reading order with trailing space

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
                // Easter Egg Logic
                let char = '';
                let isSpecial = false;

                // If this column is in special state
                if (cars10State[i] >= 0) {
                    const idx = cars10State[i] % secretMessage.length;
                    char = secretMessage[idx];
                    cars10State[i]++; // Move to next char for next frame
                    isSpecial = true;
                } else {
                    // Random chance to start secret
                    // Only if at top of screen to make it clean? Or anytime?
                    // Let's settle for random trigger anytime, but visually it looks best if distinct.
                    // Or maybe trigger when it resets?
                    // Let's stick to standard random char
                    char = chars.charAt(Math.floor(Math.random() * chars.length));
                }

                // Draw
                if (isSpecial) {
                    // Crystal Effect: White with Cyan/Magenta subtle glow
                    ctx.fillStyle = '#FFFFFF';
                    ctx.font = 'bold 14px monospace';

                    // Flash on entry (first 20 frames)
                    if (cars10State[i] < 20) {
                        ctx.shadowColor = '#FFFFFF';
                        ctx.shadowBlur = 15; // Bright flash
                        // Occasional flicker
                        if (Math.random() > 0.5) ctx.fillStyle = '#E0FFFF';
                    } else {
                        // Stable Crystal Glow
                        ctx.shadowColor = 'rgba(200, 255, 255, 0.8)'; // Ice blue glow
                        ctx.shadowBlur = 8;
                    }
                } else {
                    ctx.fillStyle = '#0F0';
                    ctx.font = '14px monospace';
                    // Glow Effect
                    ctx.shadowBlur = 8;
                    ctx.shadowColor = 'rgba(0, 255, 0, 0.5)';
                }

                ctx.fillText(char, x, y);

                // Reset
                ctx.shadowBlur = 0;

                // Reset shadows
                if (isSpecial) {
                    ctx.shadowBlur = 0;
                }

                // Reset drop to top randomly after it has crossed the screen
                if (y > height && Math.random() > 0.975) {
                    drops[i] = 0;
                    // Chance to become special when spawning new drop
                    if (Math.random() > 0.98) { // 2% chance per drop cycle
                        cars10State[i] = 0;
                    } else {
                        cars10State[i] = -1;
                    }
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

            // Draw Puzzle Overlay (after slide-in completes)
            if (puzzleLines.length > 0 && slideInComplete && currentMode === 'red' && puzzleVisible) {
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
                                // Initialize starting X position for this row to center it
                                let currentX = startX - (totalBlockWidth / 2) + (spacing / 2);

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
                if (fsHeader) {
                    fsHeader.style.display = 'block';
                    fsHeader.style.opacity = '1'; // Ensure visibility
                    if (window.startRiddleAnimation) window.startRiddleAnimation();
                    if (window.startTimerAnimation) window.startTimerAnimation();
                }

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

                // For blue mode, we also need to trigger the dancing numbers after animation
                setTimeout(() => {
                    slideInComplete = true;
                }, 1000); // 1s delay (animation is 3s but we can start earlier)
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
            if (window.stopTimerAnimation) window.stopTimerAnimation();

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


window.showMismatch = function (solverName, matrixName, cell) {
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
    currentRerunCommand = '# Run from SudokuSolver root\\nfind . -name runMe_ai.sh | grep "/' + solverName + '/" | head -n 1 | xargs -I {} sh -c \\'cd $(dirname {}) && ./ runMe_ai.sh../../ Matrices / ' + matrixName + '\\'';

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

window.closeMismatchModal = function () {
    document.getElementById('mismatchModal').style.display = 'none';
}

window.copyRerunCommand = function () {
    const btn = document.getElementById('copyRerunBtn');
    navigator.clipboard.writeText(currentRerunCommand).then(() => {
        const originalText = btn.innerText;
        btn.innerText = 'Copied!';
        setTimeout(() => {
            btn.innerText = originalText;
        }, 2000);
    });
}




// Modal Dragging Logic
function makeDraggable(elmnt, header) {
    if (!elmnt || !header) return;
    
    let pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
    
    header.style.cursor = 'move';
    header.onmousedown = dragMouseDown;

    function dragMouseDown(e) {
        if (e.target.classList.contains('modal-close')) return;
        e.preventDefault();
        // get the mouse cursor position at startup:
        pos3 = e.clientX;
        pos4 = e.clientY;
        
        // If element is not absolute/fixed yet, make it so (keeping current visual position)
        const computedStyle = window.getComputedStyle(elmnt);
        if (computedStyle.position !== 'absolute') {
            const rect = elmnt.getBoundingClientRect();
            // We need to calculate position relative to the viewport (since overlay is fixed full screen)
            // rect.left is relative to viewport.
            
            elmnt.style.position = 'absolute';
            elmnt.style.left = rect.left + 'px';
            elmnt.style.top = rect.top + 'px';
            elmnt.style.margin = '0'; // Remove centering margins if any
            elmnt.style.transform = 'none'; // Remove centering transforms
            // Adjust width to fixed pixel value to prevent resizing when switching position
            elmnt.style.width = rect.width + 'px';
        }
        
        document.onmouseup = closeDragElement;
        // call a function whenever the cursor moves:
        document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
        e.preventDefault();
        // calculate the new cursor position:
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // set the element's new position:
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        // stop moving when mouse button is released:
        document.onmouseup = null;
        document.onmousemove = null;
    }
}

// Initialize dragging for known modals
function enableModalDragging() {
    const modals = ['methodModal', 'goalsModal', 'whyModal'];
    modals.forEach(id => {
        const modal = document.getElementById(id);
        if (modal) {
            const content = modal.querySelector('.modal-content');
            const title = modal.querySelector('.modal-title');
            if (content && title) {
                makeDraggable(content, title);
            }
        }
    });
}

// Call initially and on load
enableModalDragging();
window.addEventListener('load', enableModalDragging);

    </script>
    </body>
    </html>
        `;
    return html;
}

export async function generateHistoryHtml(history: SolverMetrics[]): Promise<string> {

    // Group history by language
    const langs = [...new Set(history.map(h => h.solver))];
    const datasets = langs.map((lang, index) => {
        // Filter history for this language and sort by timestamp
        const records = history
            .filter(h => h.solver === lang)
            .sort((a, b) => new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime());

        // We'll plot performance on Matrix 1 as the primary metric for now
        const data = records.map(r => {
            const time = r.results.find(res => res.matrix.includes('1'))?.time || 0;
            return {
                x: r.timestamp,
                y: time
            };
        });

        // Neon Colors Palette
        const colors = [
            '#00ff9d', // Primary Green
            '#00b8ff', // Secondary Blue
            '#ff00ff', // Magenta
            '#ffcc00', // Yellow
            '#ff4444', // Red
            '#9d00ff', // Purple
            '#00ffff', // Cyan
        ];
        const color = colors[index % colors.length];

        return {
            label: lang,
            data: data,
            borderColor: color,
            backgroundColor: color,
            borderWidth: 2,
            tension: 0.4, // Smooth curves
            pointRadius: 4,
            pointHoverRadius: 8
        };
    });

    const html = `
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>Sudoku Benchmark History - Neon</title>
        <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap" rel="stylesheet">
        <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
        <script src="https://cdn.jsdelivr.net/npm/moment@2.29.4/moment.min.js"></script>
        <script src="https://cdn.jsdelivr.net/npm/chartjs-adapter-moment@1.0.1/dist/chartjs-adapter-moment.min.js"></script>
        <style>
            :root {
                --bg: #0d0d12;
                --surface: #16161e;
                --primary: #00ff9d;
                --text: #e0e0e0;
                --border: #2a2a35;
            }
            body {
                font-family: 'JetBrains Mono', monospace;
                background-color: var(--bg);
                color: var(--text);
                margin: 0;
                padding: 40px;
            }
            h1 {
                text-align: center;
                color: var(--primary);
                text-transform: uppercase;
                letter-spacing: 2px;
                text-shadow: 0 0 10px rgba(0, 255, 157, 0.3);
                margin-bottom: 40px;
            }
            .chart-container {
                background: var(--surface);
                border: 1px solid var(--border);
                padding: 20px;
                border-radius: 8px;
                box-shadow: 0 0 30px rgba(0, 255, 157, 0.1);
                position: relative;
                height: 600px;
                width: 90%;
                margin: 0 auto;
            }
            .back-link {
                display: block;
                margin-bottom: 20px;
                color: var(--primary);
                text-decoration: none;
                text-transform: uppercase;
                font-size: 0.9em;
            }
            .back-link:hover {
                text-decoration: underline;
            }
        </style>
    </head>
    <body>
        <a href="benchmark_report.html" class="back-link">← Back to Report</a>
        <h1>Benchmark Performance History</h1>

        <div class="chart-container">
            <canvas id="historyChart"></canvas>
        </div>

        <script>
            const ctx = document.getElementById('historyChart').getContext('2d');
            const data = \${JSON.stringify(datasets)};

            new Chart(ctx, {
                type: 'line',
                data: {
                    datasets: data
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    interaction: {
                        mode: 'index',
                        intersect: false,
                    },
                    scales: {
                        x: {
                            type: 'time',
                            time: {
                                unit: 'day'
                            },
                            grid: {
                                color: '#2a2a35'
                            },
                            ticks: {
                                color: '#e0e0e0'
                            }
                        },
                        y: {
                            type: 'logarithmic',
                            grid: {
                                color: '#2a2a35'
                            },
                            ticks: {
                                color: '#e0e0e0',
                                callback: function (value) {
                                    return value + 's';
                                }
                            },
                            title: {
                                display: true,
                                text: 'Execution Time (Seconds) - Log Scale',
                                color: '#5c5c66'
                            }
                        }
                    },
                    plugins: {
                        legend: {
                            labels: {
                                color: '#e0e0e0',
                                font: {
                                    family: 'JetBrains Mono'
                                }
                            }
                        },
                        tooltip: {
                            backgroundColor: 'rgba(22, 22, 30, 0.9)',
                            titleColor: '#00ff9d',
                            bodyColor: '#e0e0e0',
                            borderColor: '#2a2a35',
                            borderWidth: 1,
                            padding: 10,
                            displayColors: true,
                            callbacks: {
                                label: function (context) {
                                    return context.dataset.label + ': ' + context.parsed.y + 's';
                                }
                            }
                        }
                    }
                }
            });
        </script>
    </body>
    </html>
    `;
    return html;
}

