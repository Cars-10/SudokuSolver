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

// Helper to normalize matrix identifiers (handles both "1" and "1.matrix" formats)
// Old format: "1.matrix", "2.matrix", etc.
// New format: "1", "2", etc.
function normalizeMatrix(m: string | number): string {
    return String(m).replace('.matrix', '');
}

// Compiler fallback mapping for languages
const compilerMapping: Record<string, string> = {
    'C': 'gcc',
    'C++': 'g++',
    'Rust': 'rustc',
    'Go': 'go',
    'Python': 'python3',
    'JavaScript': 'node',
    'TypeScript': 'tsc/node',
    'Java': 'javac',
    'C_Sharp': 'dotnet',
    'PHP': 'php',
    'Ruby': 'ruby',
    'Perl': 'perl',
    'Swift': 'swiftc',
    'Kotlin': 'kotlinc',
    'Scala': 'scalac',
    'Haskell': 'ghc',
    'OCaml': 'ocamlopt',
    'F_Sharp': 'dotnet',
    'Lua': 'lua',
    'Julia': 'julia',
    'R': 'Rscript',
    'Fortran': 'gfortran',
    'Pascal': 'fpc',
    'Ada': 'gnat',
    'D': 'dmd',
    'Nim': 'nim',
    'Crystal': 'crystal',
    'Zig': 'zig',
    'V': 'v',
    'Dart': 'dart',
    'Elixir': 'elixir',
    'Erlang': 'erlc',
    'Clojure': 'clj',
    'Groovy': 'groovy',
    'Bash': 'bash',
    'Awk': 'awk',
    'Sed': 'sed',
    'Assembly': 'nasm',
    'Cobol': 'cobc',
    'Prolog': 'swipl',
};

export async function generateHtml(metrics: SolverMetrics[], history: any[], personalities: any, languageMetadata: any, methodologyTexts: any, referenceOutputs: any, allowedMatrices: string[] = [], benchmarkConfig: any = {}, metadataOverrides: any = {}): Promise<string> {
    // Merge overrides
    const finalLanguageMetadata = { ...languageMetadata, ...(metadataOverrides.languageMetadata || {}) };
    const finalPersonalities = { ...personalities, ...(metadataOverrides.personalities || {}) };
    const finalNarratorIntros = { ...narratorIntros, ...(metadataOverrides.narratorIntros || {}) };

    // Update local variables to use these finals
    // Since we passed in languageMetadata as arg, we might need to shadow it or use a new name.
    // Ideally we'd rename the arg, but that might break other callers?
    // Actually, we can just reassign the args if they were objects but here they are used in the template.
    // Let's use the 'final' variables in the template construction.
    // Note: I will need to ensure the rest of the function (which is huge) uses these new variables.
    // I can't easily replace all usages in one go without replacing the whole file.
    // BUT! I can just reassign the arguments!
    languageMetadata = finalLanguageMetadata;
    personalities = finalPersonalities;
    // narratorIntros depends on if it was passed in?
    // Wait, the signature does NOT include narratorIntros!
    // Line 28: metrics, history, personalities, languageMetadata, methodologyTexts, referenceOutputs, allowedMatrices, benchmarkConfig.
    // It is missing narratorIntros from arguments?
    // Then it must be importing it?
    // If it imports it, I can't overwrite the import binding.
    // I should check if narratorIntros is imported.

    console.log(`generateHtml received ${metrics.length} metrics.`);
    // Pre-load local logos
    const localLogos = await glob('logos/*.{png,svg}');
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
        const tailoringPath = path.join(rootDir, 'logos', 'Tailoring.json');
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
        cMetrics.results.forEach(r => cMap.set(normalizeMatrix(r.matrix), r.iterations));

        mismatchCount = metrics.filter(m => {
            if (m.solver === 'C') return false;

            // Check each result for this solver
            for (const r of m.results) {
                const expected = cMap.get(normalizeMatrix(r.matrix));
                // Only count as mismatch if we have a baseline and they differ
                if (expected !== undefined && r.iterations !== expected) {
                    return true;
                }
            }
            return false;
        }).length;
    }

    const metricsJson = safeJSON(sortedMetrics);
    const historyJson = safeJSON(history);
    const personalitiesJson = safeJSON(personalities);
    const metadataJson = safeJSON(languageMetadata);
    // methodologies is small text, likely safe
    const methodologiesJson = safeJSON(methodologyTexts);

    // Calculate total planned metrics based on languages * matrices
    const totalPlanned = languageMetadata ? Object.keys(languageMetadata).length * (Math.min(matrixFiles.length, 6)) : 0;

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

        

    window.handleZoomExtend = function() {
        const sysView = document.getElementById('system-architecture-view');
        if (sysView && sysView.style.display !== 'none') {
            if (window.systemZoomExtends) window.systemZoomExtends();
        } else {
            if (window.undoZoom) window.undoZoom();
        }
    };

    window.toggleChartLabels = function(value) {
        const chart = document.getElementById('d3-chart-container');
        if (value === 'text') {
            chart.classList.add('show-text-labels');
        } else {
            chart.classList.remove('show-text-labels');
        }
    };

    window.toggleLogoMode = function(btn) {
        const chart = document.getElementById('d3-chart-container');
        const isText = chart.classList.contains('show-text-labels');
        
        if (isText) {
            // Switch to Logos
            window.toggleChartLabels('logos');
            // Restore Image Icon
            btn.innerHTML = \`<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="18" height="18" rx="2" ry="2"/><circle cx="8.5" cy="8.5" r="1.5"/><polyline points="21 15 16 10 5 21"/></svg>\`;
        } else {
            // Switch to Text
            window.toggleChartLabels('text');
            // Show Text Icon
            btn.innerHTML = \`<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 7V4h16v3"/><path d="M9 20h6"/><path d="M12 4v16"/></svg>\`;
        }
    };

    window.toggleChartFullscreen = function() {
        const elem = document.getElementById('chart-wrapper');
        if (!document.fullscreenElement) {
            elem.requestFullscreen().then(() => {
                elem.classList.add('fullscreen-active');
            }).catch(err => {
                console.error(\`Error attempting to enable full-screen mode: \${err.message} (\${err.name})\`);
            });
        } else {
            document.exitFullscreen().then(() => {
                elem.classList.remove('fullscreen-active');
            });
        }
    };

    document.addEventListener('fullscreenchange', (event) => {
        const iframe = document.getElementById('system-iframe');
        if (iframe && iframe.contentDocument && iframe.contentDocument.body) {
            if (document.fullscreenElement) {
                iframe.contentDocument.body.classList.add('fullscreen-mode');
                 // Trigger resize/fit in iframe
                if (iframe.contentWindow.zoomInstance) {
                     setTimeout(() => {
                         iframe.contentWindow.zoomInstance.resize();
                         iframe.contentWindow.zoomInstance.fit();
                         iframe.contentWindow.zoomInstance.center();
                     }, 100);
                }
            } else {
                iframe.contentDocument.body.classList.remove('fullscreen-mode');
            }
        }
    });

            function poll() {
                // Polling disabled - timestamp.js doesn't exist, causes 404 console noise
                // If auto-refresh needed, implement proper WebSocket or Server-Sent Events
                return;
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
    <link rel="stylesheet" href="./Metrics/index.css">
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

        /* ========================================
           MODAL SYSTEM - FLATTENED STRUCTURE
           ======================================== */

        /* Modal Backdrop - Transparent (no dark overlay) */
        .modal {
            display: none;
            position: fixed;
            z-index: 10000;
            left: 0;
            top: 0;
            width: 100vw;
            height: 100vh;
            background: transparent;
            pointer-events: none;
            overflow: hidden;
        }

        .modal.visible {
            display: block !important;
        }

        /* Modal Content Container - Fixed Width for Consistency */
        .modal .modal-content {
            /* Box Model - Fixed sizing for width consistency */
            box-sizing: border-box;
            width: 900px;
            max-width: 90vw;
            max-height: 90vh;

            /* Layout - Simple flexbox, no nesting issues */
            display: flex;
            flex-direction: column;
            position: absolute;

            /* Styling */
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
            border: 2px solid #7aa2f7;
            border-radius: 12px;
            box-shadow: 0 8px 24px rgba(0, 0, 0, 0.5);

            /* Interaction */
            cursor: move;
            pointer-events: auto;
        }

        /* Centered positioning (fallback) */
        .modal.centered .modal-content {
            left: 50%;
            top: 50%;
            transform: translate(-50%, -50%);
        }

        /* Modal Animations */
        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }

        @keyframes slideUp {
            from {
                transform: translateY(50px);
                opacity: 0;
            }
            to {
                transform: translateY(0);
                opacity: 1;
            }
        }

        /* Prevent body scroll when modal open */
        body.modal-open {
            overflow: hidden;
        }

        /* Close button styling */
        .modal-close {
            position: absolute;
            right: 15px;
            top: 15px;
            font-size: 28px;
            font-weight: bold;
            color: #9aa5ce;
            cursor: pointer;
            z-index: 10;
            transition: color 0.2s, transform 0.2s;
            line-height: 1;
            width: 32px;
            height: 32px;
            display: flex;
            align-items: center;
            justify-content: center;
            border-radius: 4px;
        }

        .modal-close:hover {
            color: #f7768e;
            background: rgba(247, 118, 142, 0.1);
            transform: scale(1.1);
        }

        /* Modal Header - Flattened, Direct Child */
        .modal-header {
            /* Box Model - Guaranteed width match */
            box-sizing: border-box;
            width: 100%;
            padding: 20px;
            margin: 0;

            /* Layout - No flex grow/shrink */
            flex: 0 0 auto;
            display: flex;
            flex-direction: column;
            gap: 15px;

            /* Styling */
            border-bottom: 1px solid #414868;
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
        }

        /* Modal Header Top Row (logo + info) */
        .modal-header-top {
            display: flex;
            gap: 20px;
            align-items: flex-start;
        }

        /* Modal Header Bottom Row (buttons) */
        .modal-header-buttons {
            display: flex;
            gap: 10px;
            justify-content: flex-end;
        }

        /* Modal Body - Flattened, Direct Child, NO SCROLLBAR */
        .modal-body {
            /* Box Model - Guaranteed width match */
            box-sizing: border-box;
            width: 100%;
            padding: 20px;
            margin: 0;

            /* Layout - Auto height, no scrollbar */
            flex: 0 1 auto;
            overflow: visible;

            /* No scrollbar needed */
        }

        /* Modal footer */
        .modal-footer {
            padding: 15px 20px;
            border-top: 1px solid #414868;
            background: #1a1b26;
            display: flex;
            justify-content: flex-end;
            gap: 10px;
        }

        /* Edit Mode Toggle */
        .edit-only {
            display: none !important;
        }

        .editing .edit-only {
            display: block !important;
        }

        .editing .view-only {
            display: none !important;
        }

        /* Modal Edit Inputs */
        .modal-edit-input,
        .modal-edit-textarea {
            width: 100%;
            background: #24283b;
            border: 1px solid #414868;
            border-radius: 4px;
            padding: 8px 12px;
            color: #c0caf5;
            font-size: 14px;
            font-family: inherit;
            margin-bottom: 10px;
        }

        .modal-edit-textarea {
            min-height: 100px;
            resize: vertical;
        }

        .modal-edit-input:focus,
        .modal-edit-textarea:focus {
            outline: none;
            border-color: #7aa2f7;
            background: #1a1b26;
        }

        /* Author List Styling */
        .author-list {
            display: flex;
            flex-direction: column;
            gap: 15px;
            margin-top: 10px;
        }

        .author-item {
            display: flex;
            align-items: center;
            gap: 15px;
            padding: 10px;
            background: #24283b;
            border-radius: 8px;
            border: 1px solid #414868;
        }

        .author-img {
            width: 60px;
            height: 60px;
            border-radius: 50%;
            object-fit: cover;
            background: #1a1b26;
            border: 2px solid #7aa2f7;
            flex-shrink: 0;
        }

        .author-item span {
            color: #c0caf5;
            font-weight: 500;
            flex: 1;
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
            .modal .modal-content {
                width: 95%;
                max-height: 95vh;
            }

            .modal-header {
                padding: 15px;
            }

            .modal-body {
                padding: 15px;
            }
        }

        /* Logo Fallback Styling */
        .lang-logo {
            width: 40px;
            height: 40px;
            object-fit: contain;
            background: rgba(122, 162, 247, 0.1);
            border-radius: 6px;
            padding: 4px;
            image-rendering: -webkit-optimize-contrast;
            image-rendering: crisp-edges;
            vertical-align: middle;
            margin-right: 8px;
        }

        /* Tooltip Styling */
        #tooltip {
            position: fixed;
            display: none;
            background: linear-gradient(135deg, #1a1b26 0%, #24283b 100%);
            border: 1px solid #414868;
            border-radius: 8px;
            padding: 12px 16px;
            color: #c0caf5;
            font-size: 14px;
            line-height: 1.6;
            box-shadow: 0 8px 32px rgba(0, 0, 0, 0.6);
            z-index: 9999;
            pointer-events: none;
            max-width: 300px;
            backdrop-filter: blur(8px);
            -webkit-backdrop-filter: blur(8px);
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
        /* Fullscreen Wrapper Centering */
        #chart-wrapper.fullscreen-active {
            background: #000 !important;
            border: none !important;
            padding: 0 !important;
            width: 100% !important;
            height: 100% !important;
            display: flex !important;
            justify-content: center !important;
            align-items: center !important;
        }
        
        /* Node Label Toggling */
        .chart-node-label { display: none; fill: #e0e0e0; font-size: 12px; font-weight: bold; text-shadow: 0 0 2px #000; }
        .show-text-labels .chart-node-label { display: block; }
        .show-text-labels .chart-node-image { display: none; }
        
        /* All columns equal width */
        .lang-col, .score-col, .matrix-cell, .total-time {
            overflow: hidden;
            white-space: nowrap;
            text-overflow: ellipsis;
        }

        /* Hide Chart Buttons (using Pulldown now) */
        .chart-options { display: none !important; }
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <!-- Force browser to reload JavaScript by adding cache-busting timestamp -->
    <script>
        // Cache buster: ${Date.now()}
    </script>
    <canvas id="matrix-canvas"></canvas>
    <div id="tooltip"></div>
    
    <!-- Log Modal -->
    <div id="logModal" class="modal" onclick="if(event.target === this) closeLogModal()">
        <div class="modal-content">
            <div id="logHeader">
                <span id="logTitle" style="color: #fff; font-weight: bold;">Execution Log</span>
                <button class="btn" onclick="closeLogModal()" style="background:#ff5555; padding: 5px 15px;">Close</button>
            </div>
            <div id="logOutput"></div>
        </div>
    </div>

    <!-- Modal - Flattened Structure -->
    <div id="langModal" class="modal" style="display: none;" onclick="closeModal(event)">
        <div class="modal-content" id="modalContent" onclick="event.stopPropagation()">
            <!-- Header: Direct child, no nesting -->
            <div class="modal-header">
                <!-- Close button in top-right corner -->
                <span class="modal-close" onclick="closeModal(event)">&times;</span>

                <!-- Top Row: Logo + Info -->
                <div class="modal-header-top">
                    <div class="modal-img-container" id="modalImgContainer" style="flex-shrink: 0;">
                        <img id="modalImg" class="modal-img" src="" alt="Language Logo" style="width: 100px; height: 100px; object-fit: contain; border-radius: 8px;">
                        <div class="edit-only" style="position: absolute; bottom: 0; left: 0; right: 0; background: rgba(0,0,0,0.7); text-align: center; padding: 5px; font-size: 0.8em; cursor: pointer;" onclick="document.getElementById('logoInput').click()">
                            Change
                        </div>
                        <input type="file" id="logoInput" style="display: none" accept="image/*" onchange="uploadLogo(this)">
                    </div>
                    <div style="flex: 1; min-width: 0;">
                        <input type="text" id="editInputs-title" class="modal-edit-input edit-only" placeholder="Language Name" style="font-size: 1.5em; font-weight: bold;">
                        <h2 id="modalTitle" class="view-only" style="margin: 0 0 5px 0; color: #7aa2f7;"></h2>

                        <input type="text" id="editInputs-creator" class="modal-edit-input edit-only" placeholder="Creator">
                        <input type="text" id="editInputs-image" class="modal-edit-input edit-only" placeholder="Image URL (e.g., https://...)">
                        <input type="text" id="editInputs-date" class="modal-edit-input edit-only" placeholder="Date">
                        <p id="modalSubtitle" class="view-only" style="margin: 0; color: #565f89; font-size: 0.9em;"></p>

                        <input type="text" id="editInputs-location" class="modal-edit-input edit-only" placeholder="Location" style="margin-top: 10px;">
                        <input type="text" id="editInputs-benefits" class="modal-edit-input edit-only" placeholder="Benefits">
                        <input type="text" id="editInputs-website" class="modal-edit-input edit-only" placeholder="Website URL">

                        <p id="modalLocation" class="view-only" style="margin: 10px 0 0 0; color: #9aa5ce; font-size: 0.9em;"></p>
                        <p id="modalBenefits" class="view-only" style="margin: 5px 0 0 0; color: #bb9af7; font-size: 0.9em;"></p>

                        <div class="view-only" style="margin-top: 15px; display: flex; gap: 10px;">
                             <a class="btn" id="btn-website" href="#" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: inline-block;">Website</a>
                             <a class="btn" id="btn-grokipedia" href="#" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: inline-block;">Grokipedia</a>
                             <a class="btn" id="btn-wikipedia" href="#" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: inline-block;">Wikipedia</a>
                        </div>
                    </div>
                </div>

                <!-- Bottom Row: Buttons -->
                <div class="modal-header-buttons">
                     <button class="btn" onclick="toggleLockFromModal(event)" id="lockBtn" title="Lock this result to skip future benchmarks">🔓 Unlocked</button>
                     <button class="btn" onclick="toggleEditMode(event)" id="editBtn">Edit</button>
                     <button class="btn edit-only" style="background: #4caf50;" onclick="saveLanguageDetails(event)">Save</button>
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

    <!-- Slide-out Side Panel -->
    <div id="slidePanel" style="position: fixed; top: 0; right: -600px; width: 600px; height: 100%; background: #1a1b26; border-left: 1px solid #414868; box-shadow: -5px 0 15px rgba(0,0,0,0.5); z-index: 3000; transition: right 0.3s ease-in-out; display: flex; flex-direction: column;">
        <div style="padding: 15px; background: #16161e; border-bottom: 1px solid #414868; display: flex; justify-content: space-between; align-items: center;">
            <h3 id="slideTitle" style="margin: 0; color: #7aa2f7;">External Content</h3>
            <div style="display: flex; gap: 10px;">
                <a id="slideExternalLink" href="#" target="_blank" class="btn" style="text-decoration: none; font-size: 0.8em;">Open in Tab</a>
                <button class="btn" style="background: #ff5555; padding: 5px 10px;" onclick="closeSidePanel()">Close</button>
            </div>
        </div>
        <div style="flex: 1; position: relative;">
            <iframe id="slideFrame" style="width: 100%; height: 100%; border: none;" allow="clipboard-read; clipboard-write"></iframe>
             <div id="slideLoading" style="position: absolute; top:0; left:0; width:100%; height:100%; display:none; background: #1a1b26; align-items:center; justify-content:center; color: #565f89;">
                Loading...
            </div>
        </div>
    </div>

    <!-- Methodology Modal -->
    <div id="methodModal" class="modal-overlay" onclick="closeMethodology(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeMethodology(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Scoring Methodology</div>
            <div class="modal-desc">
                <p>The <strong>Composite Score</strong> (&Psi;) uses the <strong>geometric mean</strong> of four performance ratios, an industry-standard method used by SPEC and Geekbench benchmarks.</p>

                <h3 style="color: var(--secondary);">The Baseline: C</h3>
                <p style="font-family: 'JetBrains Mono', monospace;">&Psi;<sub>C</sub> = 1.0 (Reference Point)</p>

                <h3 style="color: var(--secondary);">The Formula</h3>
                <div style="background: rgba(0, 20, 0, 0.8); padding: 20px; border: 1px solid #00ff9d; box-shadow: 0 0 15px rgba(0, 255, 157, 0.2); border-radius: 4px; text-align: center; font-family: 'Times New Roman', serif; margin: 20px 0; color: #fff;">
                    <div style="font-size: 1.3em; letter-spacing: 1px;">
                        &Psi; = <sup style="font-size: 0.7em;">4</sup>&radic;(&rho;<sub>time</sub> &sdot; &rho;<sub>mem</sub> &sdot; &rho;<sub>iter</sub> &sdot; &rho;<sub>cpu</sub>)
                    </div>
                </div>
                <p style="font-size: 0.9em; text-align: center; color: var(--muted); font-family: 'JetBrains Mono', monospace;">
                    where &rho;<sub>x</sub> = Value<sub>solver</sub> / Value<sub>C</sub>
                </p>

                <h3 style="color: var(--secondary);">Metrics Used</h3>
                <ul style="list-style: none; padding: 0; font-size: 0.95em;">
                    <li style="margin-bottom: 6px;"><strong>&rho;<sub>time</sub></strong> : Total execution time ratio</li>
                    <li style="margin-bottom: 6px;"><strong>&rho;<sub>mem</sub></strong> : Peak memory usage ratio</li>
                    <li style="margin-bottom: 6px;"><strong>&rho;<sub>iter</sub></strong> : Algorithm iterations ratio</li>
                    <li style="margin-bottom: 6px;"><strong>&rho;<sub>cpu</sub></strong> : CPU time (user+sys) ratio</li>
                </ul>

                <h3 style="color: var(--secondary);">Why Geometric Mean?</h3>
                <ul style="list-style: none; padding: 0; font-size: 0.9em; color: var(--text-muted);">
                    <li style="margin-bottom: 6px;">&bull; Prevents one metric from dominating</li>
                    <li style="margin-bottom: 6px;">&bull; Penalizes imbalanced performance</li>
                    <li style="margin-bottom: 6px;">&bull; Industry standard (SPEC, Geekbench)</li>
                </ul>

                <h3 style="color: var(--secondary);">Interpretation</h3>
                <ul style="list-style: none; padding: 0;">
                    <li style="margin-bottom: 8px;"><strong style="color: var(--primary);">&Psi; = 1.0</strong> : Matches C baseline exactly</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #00b8ff;">&Psi; &lt; 1.0</strong> : Outperforms C (lower is better)</li>
                    <li style="margin-bottom: 8px;"><strong style="color: #ff0055;">&Psi; &gt; 1.0</strong> : Underperforms C</li>
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
                     <li style="margin-bottom: 10px;">🤖 <strong>AI Synergy</strong>: Learn how to leverage AI for software development.</li>
                     <li style="margin-bottom: 10px;">🛡️ <strong>Guardrails</strong>: Getting agents to follow a TDD approach to keeping guardrails and agent on task.</li>
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
                <span id="solver-text">SOLVED ${metrics.length} OF ${metrics.length}</span>
                <div id="riddle-container" class="riddle-text"></div>
                <div id="matrix-timer" class="matrix-timer" style="display: none;"></div>
                <div class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>
            </div>
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

    // Random persona selection on load - use window.onload to ensure report_client.js is loaded
    window.addEventListener('load', () => {
        const personaSelector = document.getElementById('personality-selector');
        if (personaSelector) {
            const options = Array.from(personaSelector.options).filter(o => !o.disabled && o.value);
            if (options.length > 0) {
                const randomIndex = Math.floor(Math.random() * options.length);
                personaSelector.value = options[randomIndex].value;
                if (typeof changePersonality === 'function') {
                    changePersonality();
                }
            }
        }
    });
    window.startRiddleAnimation = () => {
         if (riddleSystem) riddleSystem.start();
    };
    // Global active state for screensaver
    </script>
    
    <div id="main-content" class="content-wrapper">
    <h1>Sudoku Benchmark Results</h1>
    
    <div class="header-counters">
        <!-- Relocated Pill Container -->
        <div class="pill-container" onclick="event.stopPropagation();" style="margin: 10px 0;">
            <div class="pill-combined">
                <div class="pill-half blue" title="Take the Blue Pill (Slide Effect)" onclick="event.stopPropagation(); window.startScreensaver('blue')"></div>
                <div class="pill-half red" title="Take the Red Pill (Instant Matrix)" onclick="event.stopPropagation(); window.startScreensaver('red')"></div>
            </div>
        </div>

        <div id="screensaver-pill" style="display: none; font-size: 0.8em; color: var(--secondary); margin-top: 5px; align-self: flex-start;">
            <span style="display: inline-block; width: 8px; height: 8px; background: #00ff9d; border-radius: 50%; margin-right: 5px; box-shadow: 0 0 5px #00ff9d;"></span>
            Screensaver Active
        </div>
    </div>
    
    <div id="chart-wrapper" style="width: 95%; max-width: 1600px; margin: 0 auto 40px auto; background: #16161e; padding: 20px; border-radius: 8px; border: 1px solid #2a2a35; height: 500px; position: relative;">
        <div class="chart-controls">
            <button class="zoom-btn" onclick="toggleLogoMode(this)" title="Toggle Logos/Text" style="margin-right: 2px;">
                <svg id="logo-icon" width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                    <!-- Image Icon -->
                    <rect x="3" y="3" width="18" height="18" rx="2" ry="2"/>
                    <circle cx="8.5" cy="8.5" r="1.5"/>
                    <polyline points="21 15 16 10 5 21"/>
                </svg>
            </button>

            <button class="zoom-btn" onclick="handleZoomExtend()" title="Zoom Extent">
                <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                    <path d="M8 3H5a2 2 0 0 0-2 2v3m18 0V5a2 2 0 0 0-2-2h-3m0 18h3a2 2 0 0 0 2-2v-3M3 16v3a2 2 0 0 0 2 2h3"/>
                </svg>
            </button>
            <button class="zoom-btn" onclick="toggleChartFullscreen()" title="Fullscreen">
                <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                    <path d="M15 3h6v6M9 21H3v-6M21 3l-7 7M3 21l7-7"/>
                </svg>
            </button>

            <select id="chart-selector" class="btn active" onchange="switchChart(this.value)" style="cursor: pointer;">
                <option value="line">Metrics</option>
                <option value="jockey">Horse Race</option>
                <option value="race">Matrix Race</option>
                <option value="history">History</option>
                <option value="architecture">Architecture</option>
            </select>
        </div>
                <div id="d3-chart-container" style="width: 100%; height: 100%; position: relative;">
                    <!-- Chart rendered here -->
                </div>

    </div>
    
    <div class="top-bar">
        <!-- Old solver-stat removed -->
        <div id="personality-intro" class="personality-intro" style="text-align: center; margin: 0 auto 20px auto; max-width: 800px;">
            Welcome to the Polyglot Sudoku Benchmark. Click on any language name for creator details. Use the controls to sort data and analyze performance metrics across different languages.
        </div>
        <div class="controls">

            
            <!-- System View Controls (Hidden by default) -->
            <div id="system-controls" style="display:none; position:absolute; top: 20px; right: 120px; z-index: 100;">
                 <button class="btn" onclick="systemZoomExtends()" style="background: var(--surface); color: var(--primary); border: 1px solid var(--primary); margin-right: 10px;">Zoom Extends</button>
                 <button class="btn" onclick="exitSystemMode()" style="background: rgba(255,0,85,0.2); color: #ff0055; border: 1px solid #ff0055;">Exit System View</button>
            </div>

            <div id="d3-chart"></div>

            <!-- New System Architecture Component (Hidden by default) -->
            <div id="system-architecture-view" style="display:none; width: 100%; height: 100%; background: #000; position: absolute; top:0; left:0; z-index: 90;">
                 <iframe id="system-iframe" src="system_overview.html" style="width:100%; height:100%; border:none;"></iframe>
            </div>
            <select id="personality-selector" class="btn" onchange="changePersonality()">
                <option value="" disabled selected>PERSONA</option>
                <option value="Standard">Standard</option>
                <option value="Neuromancer">Neuromancer</option>
                <option value="Jockey">Jockey</option>
                <option value="Professor">Professor</option>
                <option value="Surfer">Surfer</option>
                <option value="Matrix">Matrix</option>
                <option value="Galactica">Galactica</option>
                <option value="Star Trek">Star Trek</option>
                <option value="Star Wars">Star Wars</option>
                <option value="BTTF">BTTF</option>
                <option value="Babylon 5">Babylon 5</option>
                <option value="Expanse">Expanse</option>
                <option value="Terminator">Terminator</option>
                <option value="LotR">Lord of the Rings</option>
                <option value="Dune">Dune</option>
                <option value="Buck Rogers">Buck Rogers</option>
                <option value="Flash Gordon">Flash Gordon</option>
                <option value="Batman">Batman (1966)</option>
                <option value="Alien">Alien</option>
                <option value="Blade Runner">Blade Runner</option>
                <option value="Farscape">Farscape</option>
                <option value="Apocalypse Now">Apocalypse Now</option>
                <option value="Airplane">Airplane!</option>
                <option value="Fast Times">Fast Times</option>
                <option value="Tron">Tron</option>
                <option value="Bill and Ted">Bill & Ted</option>
                <option value="John Wick">John Wick</option>
                <option value="Dark Knight">Dark Knight</option>
            </select>
            <button class="btn" onclick="sortRows('lang', this)">Name</button>
            <button class="btn active" onclick="sortRows('time', this)">Time</button>
            <button class="btn" onclick="sortRows('mem', this)">Memory</button>
            <button class="btn" onclick="sortRows('score', this)">Score</button>
            <button class="btn" id="toggleMismatchesBtn" onclick="toggleMismatches()">
                <span>Show Mismatches</span>
            </button>
            <div class="dropdown">
                <button class="btn">Info ▾</button>
                <div class="dropdown-content">
                    <a onclick="showMethodology()">Methodology</a>
                    <a onclick="showGoals()">Project Goals</a>
                    <a onclick="showWhy()">Why???</a>
                </div>
            </div>
            <div style="position: relative; display: inline-block;">
                <button class="btn" onclick="toggleLanguageSelector()" id="langSelectorBtn">Languages ▾</button>
                <div id="language-selector-dropdown" style="display: none; position: absolute; top: 100%; left: 0; background: #1a1b26; border: 1px solid var(--primary); padding: 10px; z-index: 1000; min-width: 150px; max-height: 300px; overflow-y: auto;">
                    <!-- Populated by JS -->
                </div>
            </div>
            
            <!-- Search Removed -->
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
                    <th class="lang-col-header">
                        Language
                        <div class="sort-group">
                            <button class="sort-btn" onclick="sortRows('lang', this)" title="Sort by Name">N</button>
                            <button class="sort-btn" onclick="sortRows('year', this)" title="Sort by Year">Y</button>
                        </div>
                    </th>
                    <th>
                        <span id="header-score">Score</span>
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
        <span id="header-time">Total Time</span>
        <div class="sort-group">
            <button class="sort-btn" onclick="sortRows('time', this)" title="Sort by Total Time">S</button>
        </div>
    </th>
    </tr></thead><tbody>`;

    for (const m of sortedMetrics) {
        const lang = m.solver;
        const times = m.results.map(r => r.time);
        const iters = m.results.map(r => r.iterations);
        const mems = m.results.map(r => r.memory || 0).filter(m => !isNaN(m));

        const totalTime = times.reduce((a, b) => a + b, 0);
        const totalIters = iters.reduce((a, b) => a + b, 0);
        const maxMem = mems.length > 0 ? Math.max(...mems) : 0;

        // Efficiency Score: Memory (MB) / Seconds
        const memMbTotal = maxMem / 1024 / 1024;
        const efficiencyScore = totalTime > 0 ? memMbTotal / totalTime : 0;

        // Composite Score (vs C) - Geometric Mean
        // Ψ = (ρ_time · ρ_mem · ρ_iter · ρ_cpu)^(1/4)
        // Floor values at 0.001 to avoid zero/negative issues

        const totalCpu = m.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0);

        const timeRatio = Math.max(0.001, (cTotalTime > 0) ? (totalTime / cTotalTime) : 1);
        const memRatio = Math.max(0.001, (cTotalMem > 0) ? (maxMem / cTotalMem) : 1);
        const cpuRatio = Math.max(0.001, (cTotalCpu > 0) ? (totalCpu / cTotalCpu) : 1);
        const iterRatio = Math.max(0.001, (cTotalIters > 0) ? (totalIters / cTotalIters) : 1);

        // Geometric mean: 4th root of product of all ratios
        const normalizedScore = Math.pow(timeRatio * memRatio * cpuRatio * iterRatio, 0.25);

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
                const cRes = cMetrics.results.find(cm => normalizeMatrix(cm.matrix) === normalizeMatrix(r.matrix));
                if (cRes) expectedIters += Number(cRes.iterations);
            });
        }

        // Baseline is C (Local)
        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
        const isMismatch = !isBaseline && expectedIters > 0 && totalIters !== expectedIters;

        let rowClass = "";
        if (isSuspect) rowClass += " suspect";
        if (isMismatch) rowClass += " mismatch-iterations";

        // Check if language is locked (from session_state.json)
        const isLocked = benchmarkConfig?.lockedLanguages?.includes(lang) || false;

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
            const r = m.results.find((res: any) => normalizeMatrix(res.matrix) === String(i + 1));
            const t = r ? r.time : 999999;
            const it = r ? r.iterations : -1;
            const mem = r ? r.memory : -1;

            let score = 0;
            if (r && cTimes[i] && cTimes[i] > 0) {
                score = t / cTimes[i];
            }

            matrixDataAttrs += ` data-m${i}-time='${t}' data-m${i}-iters='${it}' data-m${i}-mem='${mem}' data-m${i}-score='${score.toFixed(2)}'`;
        }

        // Calculate compiler info early for row data attribute
        const rowCompilerInfo = meta.compiler || compilerMapping[baseLang] || compilerMapping[lang] || '-';

        html += `<tr class="${rowClass} ${isFastest ? 'fastest-row' : ''} ${isSlowest ? 'slowest-row' : ''}"
            data-lang="${lang}"
            data-timestamp="${new Date(m.timestamp).getTime()}"
            data-year="${year}"
            data-time="${totalTime < 0.0001 && totalTime > 0 ? totalTime.toExponential(2) : totalTime.toFixed(6)}"
            data-iters="${totalIters}"
            data-mem="${maxMem}"
            data-memory="${maxMem}"
            data-compiler="${rowCompilerInfo}"
            data-score="${normalizedScore.toFixed(2)}"
            data-score-breakdown="Time: ${timeRatio.toFixed(2)}x | Mem: ${memRatio.toFixed(2)}x | CPU: ${cpuRatio.toFixed(2)}x"
            data-quote="${quote}" data-history='${historyText}' ${matrixDataAttrs}>
            <td class='lang-col'>
                <span class="expand-chevron">›</span>
                ${logoUrl ? `<img src="${logoUrl}" alt="${displayNameRaw}" class="lang-logo">` : ''}
                <div style="display: inline-block; vertical-align: middle;">
                    <div>
                        ${displayName}${typeIcon}
                    </div>
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
                             ${!isLocked ? `<button class="run-btn" onclick="runSolver('${lang}', '${i + 1}.matrix', event)" title="Run Matrix ${i + 1}">⏵</button>` : ''}
                             <div class="time" title="Wall Clock Time">${r.time != null ? (r.time === 0 ? '<0.001' : (r.time < 0.0001 ? r.time.toExponential(2) : r.time.toFixed(5))) : '-'}s</div>
                        </div>
                        <div class="meta">
                            ${(() => {
                        const cRes = cMetrics?.results.find(res => normalizeMatrix(res.matrix) === normalizeMatrix(r.matrix));
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

        html += `<td class='total-time'><div style='display:flex;flex-direction:column;align-items:center;'><div style="display:flex;align-items:center;gap:5px;">${!isLocked ? `<button class="run-btn" onclick="runAllSolver('${lang}', event)" title="Run All Matrices">⏩</button>` : ''}<div>${totalTime.toFixed(3)}s</div></div><div style='font-size:0.6em;color:#5c5c66;'>${totalIters.toLocaleString()} iters</div></div></td></tr>`;

        // Add expanded content row
        const totalCols = 3 + maxMatrices + 1; // Language + Score + Updated + Matrices + Total
        html += `<tr class="expanded-content">
            <td colspan="${totalCols}">
                <div class="expanded-sections">
                    <div class="expanded-section">
                        <div class="section-title">System</div>
                        <div class="section-content">OS: - | CPU: - | RAM: - | Arch: -</div>
                    </div>
                    <div class="expanded-section">
                        <div class="section-title">Compilation</div>
                        <div class="section-content">Compiler: ${rowCompilerInfo} | Flags: - | Level: -</div>
                    </div>
                    <div class="expanded-section">
                        <div class="section-title">Matrix Results</div>
                        <div class="section-content">
                            ${m.results.filter(r => r != null).map((r, idx) => {
                                const memMb = (r.memory || 0) / 1024 / 1024;
                                const time = r.time || 0;
                                const iterations = r.iterations || 0;
                                return `M${idx + 1}: ${time.toFixed(3)}s, ${iterations.toLocaleString()} iter, ${memMb.toFixed(1)}MB`;
                            }).join(' | ')}
                        </div>
                    </div>
                </div>
            </td>
        </tr>`;
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

    // Generate timestamp in CET timezone
    const generatedAt = new Date().toLocaleString('en-GB', {
        timeZone: 'Europe/Berlin',
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        hour12: false
    });

    html += `
    <footer style="text-align: right; padding: 20px 40px; color: #666; font-size: 12px; border-top: 1px solid #333; margin-top: 40px;">
        Report generated: ${generatedAt} CET
    </footer>
    <script src="./Metrics/report_client.js"></script>
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
            table { width: 100%; border-collapse: collapse; margin-top: 20px; table-layout: fixed; }
            th, td { border: 1px solid #2a2a35; padding: 4px 8px; text-align: left; overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
            th { background: #1a1a20; color: #00ff9d; cursor: pointer; user-select: none; }
            th:hover { background: #2a2a35; color: #fff; }
            tr:nth-child(even) { background: #16161e; }
            h1 { color: #00ff9d; font-family: 'JetBrains Mono'; text-transform: uppercase; letter-spacing: 2px; }
            .status-failure, .status-error, .status-suspect { color: #ff0055; font-weight: bold; }
            .status-optimized { color: #00b8ff; }
            
            /* Matrix Timing Style for Time Column */
            .time-val { font-size: 1.1em; font-weight: bold; color: #fff; font-family: 'JetBrains Mono', monospace; }
        </style>
        <script>
            function sortTable(n) {
                var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
                table = document.querySelector("table");
                switching = true;
                dir = "asc";
                while (switching) {
                    switching = false;
                    rows = table.rows;
                    for (i = 1; i < (rows.length - 1); i++) {
                        shouldSwitch = false;
                        x = rows[i].getElementsByTagName("TD")[n];
                        y = rows[i + 1].getElementsByTagName("TD")[n];
                        var xContent = x.innerText.toLowerCase();
                        var yContent = y.innerText.toLowerCase();
                        if (!isNaN(parseFloat(xContent)) && !isNaN(parseFloat(yContent))) {
                             xContent = parseFloat(xContent);
                             yContent = parseFloat(yContent);
                        }
                        if (dir == "asc") {
                            if (xContent > yContent) { shouldSwitch = true; break; }
                        } else if (dir == "desc") {
                            if (xContent < yContent) { shouldSwitch = true; break; }
                        }
                    }
                    if (shouldSwitch) {
                        rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
                        switching = true;
                        switchcount ++;
                    } else {
                        if (switchcount == 0 && dir == "asc") {
                            dir = "desc";
                            switching = true;
                        }
                    }
                }
            }
        </script>
    </head>
    <body>
        <h1>Benchmark History</h1>
        <table>
            <thead>
                <tr>
                    <th onclick="sortTable(0)" style="width: 200px;">Timestamp ↕</th>
                    <th onclick="sortTable(1)">Solver ↕</th>
                    <th onclick="sortTable(2)">Matrix ↕</th>
                    <th onclick="sortTable(3)">Time (s) ↕</th>
                    <th onclick="sortTable(4)">Iterations ↕</th>
                    <th onclick="sortTable(5)">Status ↕</th>
                </tr>
            </thead>
            <tbody>
                ${rows.map(r => `
                    <tr>
                        <td>${new Date(r.timestamp).toLocaleString()}</td>
                        <td style="color: var(--primary); font-weight: bold;">${r.solver}</td>
                        <td>${r.matrix}</td>
                        <td class="time-val">${(r.time ?? 0).toFixed(4)}</td>
                        <td>${r.iterations ?? '-'}</td>
                        <td class="status-${(r.status || 'unknown').toLowerCase()}">${r.status || '-'}</td>
                    </tr>
                `).join('')}
            </tbody>
        </table>
    </body>
    </html>
    `;
}
