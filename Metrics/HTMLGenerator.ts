import * as fs from 'fs/promises';
import * as fsSync from 'fs';
import * as path from 'path';
import { glob } from 'glob';
import { fileURLToPath } from 'url';
import type { SolverMetrics } from './types.ts';
import { orderedLanguages, languageHistories, quotes, personalities, methodologyTexts, languageMetadata, narratorIntros, mismatchLabels, iterationLabels, timeLabels, memoryLabels, scoreLabels } from './LanguagesMetadata.ts';
import { SharedStyles } from './SharedStyles.ts';
export { orderedLanguages, languageHistories, quotes, personalities, methodologyTexts, languageMetadata, narratorIntros, mismatchLabels, iterationLabels, timeLabels, memoryLabels, scoreLabels };

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

import { C_Baselines } from './C_Baselines.ts';
import {
    calculateSensitivityScores,
    calculateRankStability,
    sensitivityMapToArray,
    computeCorrelation,
    identifyOutliers,
    getScorePercentiles,
    WEIGHT_SCENARIOS
} from './scoring-analysis.ts';

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

export async function generateHtml(metrics: SolverMetrics[], history: any[], personalities: any, languageMetadata: any, methodologyTexts: any, referenceOutputs: any, allowedMatrices: string[] = [], benchmarkConfig: any = {}, metadataOverrides: any = {}, options: { staticMode?: boolean } = {}): Promise<string> {
    const staticMode = options.staticMode || false;

    // Read validation issues
    let validationIssues: any[] = [];
    const issuesPath = path.join(__dirname, '..', 'benchmark_issues.json');
    try {
        if (fsSync.existsSync(issuesPath)) {
            const issuesContent = await fs.readFile(issuesPath, 'utf-8');
            validationIssues = JSON.parse(issuesContent);
        }
    } catch (e) {
        console.warn('Could not read benchmark_issues.json:', e);
    }

    // Deep merge overrides (merge fields within each language, don't replace entire language objects)
    const finalLanguageMetadata = { ...languageMetadata };
    if (metadataOverrides.languageMetadata) {
        Object.keys(metadataOverrides.languageMetadata).forEach(lang => {
            finalLanguageMetadata[lang] = {
                ...(languageMetadata[lang] || {}),
                ...metadataOverrides.languageMetadata[lang]
            };
        });
    }
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
    const rootDir = path.resolve(__dirname, '..');
    const languagesDir = path.join(rootDir, 'Algorithms/BruteForce');
    const logoMap = new Map<string, string>();

    // Priority 1: Look for <lang>_logo.* in Algorithms/BruteForce/<lang>/Media/ (canonical location)
    const canonicalLogos = await glob(`${languagesDir}/*/Media/*_logo.{png,svg,jpg}`);
    for (const p of canonicalLogos) {
        const parts = p.split(path.sep);
        const mediaIdx = parts.indexOf('Media');
        if (mediaIdx > 0) {
            const langDir = parts[mediaIdx - 1];
            const langName = langDir.toLowerCase();
            const filename = path.basename(p);
            logoMap.set(langName, `Algorithms/BruteForce/${langDir}/Media/${filename}`);
        }
    }

    // Priority 2: Fall back to any image in Algorithms/BruteForce/*/Media/
    const mediaLogos = await glob(`${languagesDir}/*/Media/*.{png,svg,jpg}`);
    for (const p of mediaLogos) {
        const parts = p.split(path.sep);
        const mediaIdx = parts.indexOf('Media');
        if (mediaIdx > 0) {
            const langDir = parts[mediaIdx - 1];
            const langName = langDir.toLowerCase();
            if (!logoMap.has(langName)) {
                const filename = path.basename(p);
                logoMap.set(langName, `Algorithms/BruteForce/${langDir}/Media/${filename}`);
            }
        }
    }
    console.log(`Total logos available: ${logoMap.size}`);

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

    // Sort matrix files numerically (1.matrix, 2.matrix, etc.)
    matrixFiles.sort((a, b) => {
        const numA = parseInt(a.match(/(\d+)\.matrix/)?.[1] || '0', 10);
        const numB = parseInt(b.match(/(\d+)\.matrix/)?.[1] || '0', 10);
        return numA - numB;
    });

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

    // Find C baselines for each algorithm type
    const cMetricsByAlgorithm = new Map<string, SolverMetrics>();
    metrics.forEach(m => {
        if (m.solver === 'C' && m.algorithmType) {
            cMetricsByAlgorithm.set(m.algorithmType, m);
        }
    });

    // Fallback: if no algorithmType is set, use BruteForce as default C baseline
    const cMetrics = cMetricsByAlgorithm.get('BruteForce') || metrics.find(m => m.solver === 'C');
    const cTimes = cMetrics ? cMetrics.results.map(r => r.time) : [];
    const cTotalIters = cMetrics ? cMetrics.results.reduce((a, b) => a + b.iterations, 0) : 0;


    // C Baselines for Composite Score (BruteForce baseline)
    const cTotalTime = cTimes.reduce((a, b) => a + b, 0);
    const cTotalMem = cMetrics ? Math.max(...cMetrics.results.map(r => r.memory)) : 1; // Max RSS
    const cTotalCpu = cMetrics ? cMetrics.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0) : 1;

    // Compute scoring analysis data
    let scoringAnalysisData: {
        sensitivity: Array<{ language: string; scenarios: Array<{ scenario: string; score: number; rank: number }> }>;
        stability: Array<{ language: string; maxSwing: number; bestRank: number; worstRank: number; currentRank: number }>;
        correlation: { rValue: number; rSquared: number; interpretation: string };
        outliers: Array<{ language: string; metric: string; value: number; threshold: number; direction: string; explanation: string }>;
        percentiles: { p25: number; p50: number; p75: number; p90: number; p99: number };
    } = {
        sensitivity: [],
        stability: [],
        correlation: { rValue: 0, rSquared: 0, interpretation: 'N/A' },
        outliers: [],
        percentiles: { p25: 0, p50: 0, p75: 0, p90: 0, p99: 0 }
    };

    if (cMetrics && cMetrics.results.length > 0) {
        // Filter metrics to only those with successful results
        const validMetrics = metrics.filter(m =>
            m.results.some(r => r.status === 'success' && r.time > 0)
        );

        if (validMetrics.length > 0) {
            const sensitivityMap = calculateSensitivityScores(validMetrics, cMetrics.results);
            scoringAnalysisData.sensitivity = sensitivityMapToArray(sensitivityMap);
            scoringAnalysisData.stability = calculateRankStability(sensitivityMap);
            scoringAnalysisData.correlation = computeCorrelation(validMetrics);
            scoringAnalysisData.outliers = identifyOutliers(validMetrics);
            scoringAnalysisData.percentiles = getScorePercentiles(validMetrics);
        }
    }

    const scoringAnalysisJson = safeJSON(scoringAnalysisData);

    // Calculate mismatch count
    // Calculate mismatch count (Per-matrix logic, verified against algorithm-specific C baseline)
    let mismatchCount = 0;

    mismatchCount = metrics.filter(m => {
        if (m.solver === 'C') return false;

        // Constraint Propagation (CP) uses heuristics that vary by implementation.
        // Iteration counts are NOT expected to match the C baseline.
        // Therefore, we do not flag CP differences as mismatches.
        if (m.algorithmType === 'CP') return false;

        // Get the C baseline for this algorithm type
        const algoType = m.algorithmType || 'BruteForce';
        const cBaselineForAlgo = cMetricsByAlgorithm.get(algoType) || cMetrics;

        if (!cBaselineForAlgo) return false;

        // Create map for C baseline
        const cMap = new Map<string, number>();
        cBaselineForAlgo.results.forEach(r => cMap.set(normalizeMatrix(r.matrix), r.iterations));

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

    const diagnostics: any = {
        env_error: { count: 0, languages: [] },
        timeout: { count: 0, languages: [] },
        error: { count: 0, languages: [] },
        missing: { count: 0, languages: [] }
    };

    const languagesWithResults = new Set(metrics.filter(m => m.solver).map(m => m.solver.replace(/ \((AI)\)$/, '')));

    metrics.forEach(m => {
        const lang = m.solver;
        // Filter out "N/A" matrix entries - those are toolchain checks, not actual matrix failures
        const realResults = m.results.filter(r => r.matrix !== 'N/A');
        const hasSuccessfulResults = realResults.some(r => r.status === 'success');

        // Only report env_errors if there are no successful results (toolchain completely unavailable)
        const envErrors = realResults.filter(r => r.status === 'env_error').map(r => r.matrix);
        const timeouts = realResults.filter(r => r.status === 'timeout').map(r => r.matrix);
        const errors = realResults.filter(r => r.status === 'error').map(r => r.matrix);

        // Only count env_error if the language has NO successful results
        if (envErrors.length > 0 && !hasSuccessfulResults) {
            diagnostics.env_error.count++;
            diagnostics.env_error.languages.push({ language: lang, matrices: envErrors });
        }
        if (timeouts.length > 0) {
            diagnostics.timeout.count++;
            diagnostics.timeout.languages.push({ language: lang, matrices: timeouts });
        }
        if (errors.length > 0) {
            diagnostics.error.count++;
            diagnostics.error.languages.push({ language: lang, matrices: errors });
        }
    });

    if (languageMetadata) {
        Object.keys(languageMetadata).forEach(lang => {
            if (!languagesWithResults.has(lang)) {
                diagnostics.missing.count++;
                diagnostics.missing.languages.push({ language: lang, matrices: [] });
            }
        });
    }

    const metricsJson = safeJSON(sortedMetrics);
    const historyJson = safeJSON(history);
    const personalitiesJson = safeJSON(personalities);
    const metadataJson = safeJSON(languageMetadata);
    // methodologies is small text, likely safe
    const methodologiesJson = safeJSON(methodologyTexts);

    // Calculate total planned metrics based on languages * matrices
    const totalPlanned = languageMetadata ? Object.keys(languageMetadata).length * (Math.min(matrixFiles.length, 6)) : 0;

    // Source Code Data generation removed - handled dynamically by server
    const sourceCodeData = {}; // Empty placeholder for client script

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
        if (window.undoZoom) window.undoZoom();
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
        ${SharedStyles}
    </style>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <!-- Debug Overlay (Hidden by default, shown with ?debug=1) -->
    <div id="debug-overlay" style="display: none; position: fixed; top: 10px; left: 10px; background: rgba(0, 0, 0, 0.8); color: white; padding: 8px 12px; border-radius: 4px; font-family: 'JetBrains Mono', monospace; font-size: 11px; z-index: 99999; border: 1px solid rgba(255, 255, 255, 0.2); line-height: 1.4;">
        <div style="font-weight: bold; margin-bottom: 2px;">🔍 Debug Info</div>
        <div>Branch: <span id="debug-branch">loading...</span></div>
        <div>Commit: <span id="debug-hash">loading...</span></div>
        <div>Built: <span id="debug-time">${new Date().toLocaleString()}</span></div>
    </div>
    <script>
        // Show debug overlay only when ?debug=1 is in URL
        (function() {
            const urlParams = new URLSearchParams(window.location.search);
            if (urlParams.get('debug') === '1') {
                document.getElementById('debug-overlay').style.display = 'block';

            // Populate git info from backend
            fetch('/api/git-info').then(r => r.json()).then(data => {
                document.getElementById('debug-branch').textContent = data.branch || 'unknown';
                document.getElementById('debug-hash').textContent = data.hash || 'unknown';
            }).catch(() => {
                document.getElementById('debug-branch').textContent = 'offline';
                document.getElementById('debug-hash').textContent = 'offline';
            });
            }
        })();
    </script>

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

                <div style="display: flex; gap: 20px;">
                    <!-- Left Sidebar: Logo + Authors -->
                    <div style="display: flex; flex-direction: column; align-items: center; gap: 15px; width: 170px; flex-shrink: 0;">
                        <div class="sidebar-box" id="logoBox">
                            <div class="sidebar-img-container" id="modalImgContainer">
                                <img id="modalImg" class="sidebar-img" src="" alt="Language Logo">
                                <div class="edit-only" style="position: absolute; bottom: 0; left: 0; right: 0; background: rgba(0,0,0,0.7); text-align: center; padding: 5px; font-size: 0.8em; cursor: pointer;" onclick="handleLogoChange(event)">
                                    Change
                                </div>
                                <input type="file" id="logoInput" style="display: none" accept="image/*" onchange="uploadLogo(this)">
                                <input type="file" id="authorFileInput" style="display: none" accept="image/*" onchange="uploadAuthorLogo(this)">
                            </div>
                        </div>
                        
                        <!-- Authors Vertical Stack -->
                        <div id="authorList" class="author-list">
                            <!-- Dynamic authors -->
                        </div>
                        <button class="btn edit-only" style="font-size: 0.7em; width: 100%;" onclick="addAuthorField()">+ Add Author</button>
                    </div>

                    <!-- Main Content: Metadata + Desc -->
                    <div style="flex: 1; min-width: 0;">
                        <input type="text" id="editInputs-title" class="modal-edit-input edit-only" placeholder="Language Name" style="font-size: 1.2em; font-weight: bold;">
                        <h2 id="modalTitle" class="view-only" style="margin: 0 0 10px 0; color: #7aa2f7;"></h2>

                        <!-- View Only: Two Column Grid -->
                        <div class="view-only" style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px; font-size: 0.9em; margin-bottom: 15px;">
                             <!-- Left Column -->
                             <div style="display: flex; flex-direction: column; gap: 6px;">
                                  <div><span style="color: #565f89;">Created:</span> <span id="modalDate" style="color: #c0caf5;"></span> <span style="color: #414868;">•</span> <span id="modalCreator" style="color: #c0caf5;"></span></div>
                                  <div><span style="color: #565f89;">Location:</span> <span id="modalLocation" style="color: #c0caf5;"></span></div>
                                  <div style="margin-top: 4px;"><span id="modalBenefits" style="color: #bb9af7; font-style: italic;"></span></div>
                             </div>
                             <!-- Right Column -->
                             <div style="display: flex; flex-direction: column; gap: 6px;">
                                  <div><span style="color: #565f89;">Paradigm:</span> <span id="modalParadigm" style="color: #7aa2f7;"></span></div>
                                  <div><span style="color: #565f89;">Type Sys:</span> <span id="modalTypeSystem" style="color: #7aa2f7;"></span></div>
                                  <div><span style="color: #565f89;">Related:</span> <span id="modalRelated" style="color: #c0caf5;"></span></div>
                             </div>
                        </div>

                        <!-- Edit inputs -->
                        <div class="edit-only" style="display: grid; grid-template-columns: 1fr 1fr; gap: 6px; margin-top: 8px; margin-bottom: 15px;">
                            <input type="text" id="editInputs-creator" class="modal-edit-input" placeholder="Creator">
                            <input type="text" id="editInputs-date" class="modal-edit-input" placeholder="Date">
                            <input type="text" id="editInputs-location" class="modal-edit-input" placeholder="Location">
                            <input type="text" id="editInputs-website" class="modal-edit-input" placeholder="Website URL">
                            <input type="text" id="editInputs-benefits" class="modal-edit-input" placeholder="Benefits">
                            <input type="text" id="editInputs-related" class="modal-edit-input" placeholder="Related Languages">
                            <input type="text" id="editInputs-paradigm" class="modal-edit-input" placeholder="Paradigm">
                            <input type="text" id="editInputs-typeSystem" class="modal-edit-input" placeholder="Type System">
                            <input type="text" id="editInputs-image" class="modal-edit-input" placeholder="Image URL" style="grid-column: 1 / -1;">
                        </div>

                        <!-- External Links -->
                        <div class="view-only" style="margin-bottom: 15px; display: flex; gap: 8px; flex-wrap: wrap;">
                             <a class="btn" id="btn-website" href="#" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: inline-block;">Website</a>
                             <a class="btn" id="btn-grokipedia" href="#" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: inline-block;">Grokipedia</a>
                             <a class="btn" id="btn-wikipedia" href="#" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: inline-block;">Wikipedia</a>
                             ${!staticMode ? `<a class="btn" href="#" onclick="viewSourceCode(); return false;" style="text-decoration: none; display: inline-block; background: #ff9e64; color: #1a1b26;">View Source</a>` : ''}
                        </div>
                        
                        <!-- Description Area -->
                        <h3 style="color: #7aa2f7; font-size: 1.1em; border-bottom: 1px solid #414868; padding-bottom: 5px;">Description</h3>
                        <textarea id="editInputs-desc" class="modal-edit-textarea edit-only" placeholder="Description"></textarea>
                        <div id="modalDesc" class="view-only" style="line-height: 1.6; color: #c0caf5;"></div>

                        <div id="historySection" class="view-only" style="margin-top: 20px;">
                            <h3 style="color: #7aa2f7; font-size: 1.1em; border-bottom: 1px solid #414868; padding-bottom: 5px;">Historical Context</h3>
                            <div id="modalHistory" style="line-height: 1.6; color: #9aa5ce; font-style: italic; background: rgba(0,0,0,0.2); padding: 15px; border-radius: 8px; border-left: 3px solid #7aa2f7;"></div>
                        </div>
                        <div class="edit-only" style="margin-top: 20px;">
                            <h3 style="color: #7aa2f7; font-size: 1.1em; border-bottom: 1px solid #414868; padding-bottom: 5px;">History</h3>
                            <textarea id="editInputs-history" class="modal-edit-textarea" placeholder="Historical context..."></textarea>
                        </div>
                    </div>
                </div>
            </div>
            
            <div class="modal-body" style="padding-top: 0;">
                 <!-- Body content moved up to header/flex layout. Keeping this empty or for extra tools if needed. -->
                 <div class="edit-only" style="margin-top: 20px; padding: 10px; background: #24283b; border-radius: 8px;">
                    <h4>Tools</h4>
                    <button class="btn" onclick="openGoogleImageSearch()">Search Google Images</button>
                    <p style="font-size: 0.8em; color: #787c99; margin-top: 5px;">Tip: Paste an image (Ctrl+V) anywhere in this modal to upload it as the main logo.</p>
                </div>
            </div>
            
            <!-- Footer: Edit/Save Buttons -->
            <div class="modal-footer" style="padding: 15px 20px; border-top: 1px solid #414868; display: flex; justify-content: flex-end;">
                 ${!staticMode ? `<button class="btn" onclick="toggleEditMode(event)" id="editBtn" style="min-width: 80px;">Edit</button>
                 <button class="btn edit-only" style="background: #4caf50;" onclick="saveLanguageDetails(event)">Save</button>` : ''}
            </div>
        </div>
    </div>

    <!-- Source Code Modal -->
    ${!staticMode ? `<div id="sourceModal" class="modal" style="display: none;" onclick="closeSourceModal(event)">
        <div class="modal-content" id="sourceModalContent" onclick="event.stopPropagation()" style="max-width: 800px; max-height: 90vh;">
            <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;">
                <h3 id="sourceModalTitle" style="margin: 0; color: #7aa2f7; flex: 1;"></h3>
                <div style="display: flex; gap: 10px; align-items: center;">
                    <button class="btn" onclick="copySourceCode()" style="font-size: 0.8em;">Copy</button>
                    <span class="modal-close" onclick="closeSourceModal(event)" style="position: static; font-size: 1.5em;">&times;</span>
                </div>
            </div>
            <pre id="sourceCodeContent" style="background: #16161e; padding: 15px; border-radius: 8px; overflow: auto; max-height: 70vh; font-family: 'JetBrains Mono', monospace; font-size: 0.85em; line-height: 1.4; color: #c0caf5; white-space: pre-wrap; word-wrap: break-word; border: 1px solid #414868;"></pre>
        </div>
    </div>` : ''}

    <!-- Score Modal -->
    <div id="scoreModal" class="modal" style="display: none;" onclick="closeScoreModal(event)">
        <div class="modal-content score-modal-content" onclick="event.stopPropagation()">
            <span class="modal-close" onclick="closeScoreModal(event)">&times;</span>

            <!-- Header -->
            <div class="score-modal-header">
                <div class="score-modal-title">
                    <img id="scoreModalImg" class="score-modal-logo" src="" alt="Language Logo">
                    <div>
                        <h2 id="scoreModalTitle" style="margin: 0; color: #7aa2f7;"></h2>
                        <p id="scoreModalSubtitle" style="margin: 5px 0 0 0; color: #565f89; font-size: 0.9em;"></p>
                        <!-- Variant Selector (US-007) -->
                        <select id="scoreVariantSelector" onchange="onVariantSelect(this.value)" style="margin-top: 8px; padding: 4px 8px; background: #24283b; color: #c0caf5; border: 1px solid #414868; border-radius: 4px; font-size: 0.85em; display: none;">
                        </select>
                        <!-- Admin Controls -->
                        <div id="scoreAdminControls" style="margin-top: 10px; display: flex; gap: 10px; align-items: center; opacity: 0.5; transition: opacity 0.2s;" onmouseover="this.style.opacity=1" onmouseout="this.style.opacity=0.5">
                            <span style="font-size: 0.7em; color: #444;">WEIGHTS:</span>
                            <input type="number" id="weight-time" step="0.1" style="width: 50px; background: #1a1b26; color: #7aa2f7; border: 1px solid #333; font-size: 0.8em;" placeholder="Time">
                            <input type="number" id="weight-mem" step="0.1" style="width: 50px; background: #1a1b26; color: #7aa2f7; border: 1px solid #333; font-size: 0.8em;" placeholder="Mem">
                            <input type="number" id="weight-cpu" step="0.1" style="width: 50px; background: #1a1b26; color: #7aa2f7; border: 1px solid #333; font-size: 0.8em;" placeholder="CPU">
                            <button onclick="saveWeights()" class="btn" style="padding: 2px 8px; font-size: 0.7em; background: rgba(76, 175, 80, 0.2); color: #4caf50;">Save</button>
                        </div>
                    </div>
                </div>
                <div id="scoreModalTierBadge" class="tier-badge" style="font-size: 1.2em; width: 40px; height: 40px;"></div>
            </div>

            <!-- Two-column layout -->
            <div class="score-modal-body">
                <!-- Left column: Score breakdown -->
                <div class="score-modal-left">
                    <h3 style="color: #7aa2f7; margin: 0 0 15px 0; font-size: 1em; border-bottom: 1px solid #414868; padding-bottom: 8px;">Composite Score</h3>
                    <div id="scoreModalValue" class="score-modal-value"></div>
                    <div class="score-breakdown">
                        <div class="breakdown-item">
                            <span class="breakdown-label">Time Ratio</span>
                            <span id="scoreTimeRatio" class="breakdown-value"></span>
                        </div>
                        <div class="breakdown-item">
                            <span class="breakdown-label">Memory Ratio</span>
                            <span id="scoreMemRatio" class="breakdown-value"></span>
                        </div>
                        <div class="breakdown-item">
                            <span class="breakdown-label">CPU Ratio</span>
                            <span id="scoreCpuRatio" class="breakdown-value"></span>
                        </div>

                    </div>
                </div>

                <!-- Right column: Radar chart -->
                <div class="score-modal-right">
                    <h3 style="color: #7aa2f7; margin: 0 0 15px 0; font-size: 1em; border-bottom: 1px solid #414868; padding-bottom: 8px;">Performance Profile</h3>
                    <canvas id="scoreRadarChart" width="280" height="280"></canvas>
                    <div class="radar-legend">
                        <div class="legend-item">
                            <span class="legend-swatch" id="legendLangSwatch" style="background: #00ff9d;"></span>
                            <span id="legendLangName">This Language</span>
                        </div>
                        <div class="legend-item">
                            <span class="legend-swatch" style="background: rgba(122, 162, 247, 0.5);"></span>
                            <span>C Baseline</span>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Matrix Results Table -->
            <div class="score-modal-matrix">
                <h3 style="color: #7aa2f7; margin: 0 0 15px 0; font-size: 1em; border-bottom: 1px solid #414868; padding-bottom: 8px;">Matrix Results</h3>
                <table id="scoreMatrixTable" class="score-matrix-table">
                    <thead>
                        <tr>
                            <th>Matrix</th>
                            <th>Time</th>
                            <th>C Time</th>
                            <th>Ratio</th>
                            <th>Iterations</th>
                            <th>Memory</th>
                        </tr>
                    </thead>
                    <tbody id="scoreMatrixBody">
                    </tbody>
                </table>
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
                <p>The <strong>Composite Score</strong> (&Psi;) uses the <strong>Weighted Geometric Mean</strong> of performance ratios, prioritizing execution speed while respecting memory efficiency.</p>

                <h3 style="color: var(--secondary);">The Baseline: C</h3>
                <p style="font-family: 'JetBrains Mono', monospace;">&Psi;<sub>C</sub> = 1.0 (Reference Point)</p>

                <h3 style="color: var(--secondary);">The Formula</h3>
                <div style="background: rgba(0, 20, 0, 0.8); padding: 20px; border: 1px solid #00ff9d; box-shadow: 0 0 15px rgba(0, 255, 157, 0.2); border-radius: 4px; text-align: center; font-family: 'Times New Roman', serif; margin: 20px 0; color: #fff;">
                    <div style="font-size: 1.3em; letter-spacing: 1px;">
                        &Psi; = (&rho;<sub>time</sub><sup>0.8</sup> &times; &rho;<sub>mem</sub><sup>0.2</sup>)
                    </div>
                </div>
                <p style="font-size: 0.9em; text-align: center; color: var(--muted); font-family: 'JetBrains Mono', monospace;">
                    where &rho;<sub>x</sub> = Value<sub>solver</sub> / Value<sub>C</sub>
                </p>

                <h3 style="color: var(--secondary);">Metrics & Weights</h3>
                <ul style="list-style: none; padding: 0; font-size: 0.95em;">
                    <li style="margin-bottom: 6px;"><strong>&rho;<sub>time</sub></strong> : Total execution time ratio (80% Weight)</li>
                    <li style="margin-bottom: 6px;"><strong>&rho;<sub>mem</sub></strong> : Peak memory usage ratio (20% Weight)</li>
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


    <!-- Goals Modal -->
    <div id="goalsModal" class="modal-overlay" onclick="closeGoals(event)">
        <div class="modal-content" style="text-align: left;">
            <span class="modal-close" onclick="closeGoals(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Goals</div>
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

    <!-- Diagnostics Modal -->
    <div id="diagnosticsModal" class="modal-overlay" onclick="closeDiagnostics(event)">
        <div class="modal-content" style="text-align: left; width: 80%; max-width: 1000px;">
            <span class="modal-close" onclick="closeDiagnostics(event)">&times;</span>
            <div class="modal-title" style="text-align: center;">Language Diagnostics</div>
            <div class="modal-desc" id="diagnosticsContent">
                <!-- Populated by JS -->
            </div>
        </div>
    </div>

    <!-- Validation Diagnostics Modal -->
    <div id="validation-diagnostics-modal-overlay" class="diagnostics-modal-overlay" onclick="if(event.target === this) window.hideDiagnosticsModal()">
        <div class="diagnostics-modal">
            <h3 id="validation-diagnostics-modal-title">Validation Issues</h3>
            <div id="validation-diagnostics-modal-content"></div>
            <button class="close-btn" onclick="window.hideDiagnosticsModal()">Close</button>
        </div>
    </div>

    <!-- Fullscreen header overlay - matches main page header -->
    <div id="fullscreen-header">
        <div class="header-counters">
            <div id="solver-box" class="solver-counter" style="position: relative; display: flex; flex-direction: column; align-items: center; line-height: 1; min-height: 40px; padding-bottom: 20px;">
                <span id="solver-text">${metrics.length} WORKING IMPLEMENTATIONS</span>
                <div id="matrix-timer" class="matrix-timer" style="display: none;"></div>
                <div class="mismatch-counter">MISMATCHES: ${mismatchCount}</div>
                <div id="diagnostics-status" class="diagnostics-status" style="margin-top: 8px; font-size: 0.7em; font-family: 'JetBrains Mono', monospace; display: flex; gap: 15px; flex-wrap: wrap; justify-content: center;">
                    <span style="color: #ff9e64 !important;">ENV: ${diagnostics.env_error.count}</span>
                    <span style="color: #f7768e !important;">TIMEOUT: ${diagnostics.timeout.count}</span>
                    <span style="color: #ff0055 !important;">ERROR: ${diagnostics.error.count}</span>
                    <span style="color: #bb9af7 !important;">MISSING: ${diagnostics.missing.count}</span>
                </div>
                <div id="riddle-container" class="riddle-text" style="font-size: 0.8em; color: var(--secondary); margin-top: 5px; position: relative; bottom: auto;"></div>
            </div>
        </div>
    </div>

    
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
                <option value="algorithm">Algorithm Comparison</option>
                <option value="heatmap">Heatmap</option>
                <option value="jockey">Horse Race</option>
                <option value="iterations">Iteration Counts</option>
                <option value="language">Language</option>
                <option value="line">Line</option>
                <option value="race">Matrix Race</option>
                <option value="histogram">Score Distribution</option>
                <option value="scatter">Time vs Memory</option>
            </select>
        </div>
                <div id="d3-chart-container" style="width: 100%; height: 100%; position: relative;">
            <div id="d3-chart" style="width: 100%; height: 100%;"></div>
                </div>

    </div>
    
    <div class="top-bar">
        <!-- Old solver-stat removed -->
        <div id="personality-intro" class="personality-intro" style="text-align: center; margin: 0 auto 20px auto; max-width: 800px; padding: 15px; background: rgba(0, 255, 157, 0.05); border: 1px solid rgba(0, 255, 157, 0.1); border-radius: 8px; box-shadow: 0 0 15px rgba(0, 255, 157, 0.05); color: var(--text); line-height: 1.6;">
            Welcome to the Polyglot Sudoku Benchmark. Click on any language name for creator details. Use the controls to sort data and analyze performance metrics across different languages.
        </div>

        <div class="controls">

            

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
            <button class="btn" id="toggleMismatchesBtn" onclick="toggleMismatches()" title="Toggle visibility of languages with iteration counts that don't match the C reference">
                <span>Hide Mismatches</span>
            </button>
            <button class="btn" id="toggleFailedBtn" onclick="toggleFailed()">Hide Failed</button>
            <button class="btn" onclick="showDiagnostics()">Diagnostics</button>
            <div class="dropdown">
                <button class="btn">Info ▾</button>
                <div class="dropdown-content">
                    <a onclick="showMethodology()">Methodology</a>
                    <a onclick="showGoals()">Goals</a>
                    <a onclick="showWhy()">Why???</a>
                    <a onclick="showScoringInsights()">Insights</a>
                    <a onclick="launchInteractiveSolver()">Solver</a>
                </div>
            </div>
            <div style="position: relative; display: inline-block;">
                <button class="btn" onclick="toggleLanguageSelector()" id="langSelectorBtn">${languagesWithResults.size} LANGUAGES ▾</button>
                <div id="language-selector-dropdown" style="display: none; position: absolute; top: 100%; left: 0; background: #1a1b26; border: 1px solid var(--primary); padding: 10px; z-index: 1000; min-width: 150px; max-height: 300px; overflow-y: auto;">
                    <!-- Populated by JS -->
                </div>
            </div>
            <div class="dropdown">
                <button class="btn" id="algorithmSelectorBtn">ALGORITHM ▾</button>
                <div class="dropdown-content">
                    <a onclick="filterByAlgorithm('all')">All Algorithms</a>
                    <a onclick="filterByAlgorithm('BruteForce')" class="active">Brute Force</a>
                    <a onclick="filterByAlgorithm('DLX')">Dancing Links</a>
                    <a onclick="filterByAlgorithm('CP')">Constraint Propagation</a>
                </div>
            </div>

            <!-- Search Removed -->
        </div>
    </div>

    <!-- Tier Legend -->
    <div class="legend-container" style="display: flex; justify-content: center; gap: 20px; margin-bottom: 30px; font-size: 0.85em; flex-wrap: wrap; color: var(--muted);">
        <div style="display: flex; align-items: center; gap: 8px;">
            <div class="tier-badge tier-s" style="font-size: 0.8em; width: 20px; height: 20px;">S</div> <span style="color: #ffd700;">&lt; 0.95 (Godlike)</span>
        </div>
        <div style="display: flex; align-items: center; gap: 8px;">
            <div class="tier-badge tier-a" style="font-size: 0.8em; width: 20px; height: 20px;">A</div> <span style="color: #00ff9d;">0.95 - 1.05 (Baseline)</span>
        </div>
        <div style="display: flex; align-items: center; gap: 8px;">
            <div class="tier-badge tier-b" style="font-size: 0.8em; width: 20px; height: 20px;">B</div> <span style="color: #00b8ff;">1.05 - 1.50 (Efficient)</span>
        </div>
        <div style="display: flex; align-items: center; gap: 8px;">
            <div class="tier-badge tier-c" style="font-size: 0.8em; width: 20px; height: 20px;">C</div> <span style="color: #e0e0e0;">1.50 - 3.00 (Acceptable)</span>
        </div>
        <div style="display: flex; align-items: center; gap: 8px;">
            <div class="tier-badge tier-d" style="font-size: 0.8em; width: 20px; height: 20px;">D</div> <span style="color: #ffaa00;">3.00 - 10.00 (Slow)</span>
        </div>
        <div style="display: flex; align-items: center; gap: 8px;">
            <div class="tier-badge tier-f" style="font-size: 0.8em; width: 20px; height: 20px;">F</div> <span style="color: #ff0055;">&gt; 10.00 (Glacial)</span>
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
                    <th class="lang-col-header sortable-header" onclick="sortRows('lang', this)" style="cursor: pointer;" title="Sort by Name" data-sort="lang">
                        Language <span class="sort-arrow">▲</span>
                    </th>
                    <th class="sortable-header" onclick="sortRows('score', this)" style="cursor: pointer;" title="Sort by Score" data-sort="score">
                        <span id="header-score">Score</span> <span class="sort-arrow">▲</span>
                    </th>
                    <th class="sortable-header" onclick="sortRows('timestamp', this)" style="cursor: pointer;" title="Sort by Age" data-sort="timestamp">
                        Updated <span class="sort-arrow">▲</span>
                    </th>`;

    for (let i = 0; i < maxMatrices; i++) {
        html += `<th class="sortable-header" onclick="sortMatrix(${i}, 'time', this)" style="cursor: pointer;" title="Sort by Matrix ${i + 1} Time" data-sort="matrix-${i}">
            Matrix ${i + 1} <span class="sort-arrow">▲</span>
        </th>`;
    }

    html += `<th class="sortable-header" onclick="sortRows('time', this)" style="cursor: pointer;" title="Sort by Total Time" data-sort="time">
        <span id="header-time">Total Time (ms)</span> <span class="sort-arrow">▲</span>
    </th>
    </tr></thead><tbody id="mainTableBody">`;

    sortedMetrics.forEach((m, rankIndex) => {
        // Skip metrics with undefined solver (data integrity issue)
        if (!m.solver) {
            console.warn(`Skipping metric with undefined solver:`, m);
            return;
        }
        const lang = m.solver;
        const rank = rankIndex + 1;
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
        // Ψ = (ρ_time · ρ_mem · ρ_cpu)^(1/3)
        // Compare only against C's results for the SAME matrices this language ran
        // Floor values at 0.001 to avoid zero/negative issues

        const totalCpu = m.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0);

        // Calculate C baseline only for matrices this language ran
        // Use algorithm-specific C baseline for comparison
        const algorithmType = m.algorithmType || 'BruteForce';
        const cBaselineForAlgo = cMetricsByAlgorithm.get(algorithmType) || cMetrics;

        let cMatchedTime = 0;
        let cMatchedMem = 0;
        let cMatchedCpu = 0;
        if (cBaselineForAlgo) {
            m.results.forEach(r => {
                const cRes = cBaselineForAlgo.results.find(cr => normalizeMatrix(cr.matrix) === normalizeMatrix(r.matrix));
                if (cRes) {
                    cMatchedTime += cRes.time;
                    cMatchedMem = Math.max(cMatchedMem, cRes.memory || 0);
                    cMatchedCpu += cRes.cpu_user + cRes.cpu_sys;
                }
            });
        }

        const timeRatio = Math.max(0.001, (cMatchedTime > 0) ? (totalTime / cMatchedTime) : 1);
        const memRatio = Math.max(0.001, (cMatchedMem > 0) ? (maxMem / cMatchedMem) : 1);
        const cpuRatio = Math.max(0.001, (cMatchedCpu > 0) ? (totalCpu / cMatchedCpu) : 1);

        // Weighted Geometric Mean: (Time^0.8 * Mem^0.2)
        let normalizedScore = Math.pow(timeRatio, 0.8) * Math.pow(memRatio, 0.2);

        // Tier Calculation
        let tier = 'F';
        let tierClass = 'tier-f';
        if (normalizedScore < 0.95) { tier = 'S'; tierClass = 'tier-s'; }
        else if (normalizedScore < 1.05) { tier = 'A'; tierClass = 'tier-a'; }
        else if (normalizedScore < 1.50) { tier = 'B'; tierClass = 'tier-b'; }
        else if (normalizedScore < 3.00) { tier = 'C'; tierClass = 'tier-c'; }
        else if (normalizedScore < 10.00) { tier = 'D'; tierClass = 'tier-d'; }

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
        // Calculate expected iterations based on the matrices this solver successfully ran
        // Use algorithm-specific C baseline for comparison
        const algoType = m.algorithmType || 'BruteForce';
        const cBaselineForMismatch = cMetricsByAlgorithm.get(algoType) || cMetrics;

        let expectedIters = 0;
        const mismatchDetails: Array<{ matrix: string, actual: number, expected: number }> = [];

        // Skip mismatch check for CP
        if (cBaselineForMismatch && algoType !== 'CP') {
            m.results.forEach(r => {
                const cRes = cBaselineForMismatch.results.find(cm => normalizeMatrix(cm.matrix) === normalizeMatrix(r.matrix));
                if (cRes) {
                    const expected = Number(cRes.iterations);
                    const actual = Number(r.iterations);
                    expectedIters += expected;
                    if (actual !== expected) {
                        mismatchDetails.push({ matrix: normalizeMatrix(r.matrix), actual, expected });
                    }
                }
            });
        }

        // Baseline is C (Local)
        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
        const isMismatch = !isBaseline && mismatchDetails.length > 0;

        let rowClass = "";
        if (isSuspect) rowClass += " suspect";
        if (isMismatch) rowClass += " mismatch-iterations";

        // Check if language is locked (from session_state.json)
        const isLocked = benchmarkConfig?.lockedLanguages?.includes(lang) || false;

        const baseLang = lang; // lang is already clean now
        const runType = m.runType || 'Local';

        // Determine failure status classes
        if (m.failed || (m.results.length === 0 && m.runType !== 'Init')) {
            // Try to find specific failure reason from metrics
            const hasEnvError = m.results.some(r => r.status === 'env_error') || (m.results.length === 0);
            const hasTimeout = m.results.some(r => r.status === 'timeout');
            const hasError = m.results.some(r => r.status === 'error');

            if (hasEnvError) rowClass += " status-env_error";
            if (hasTimeout) rowClass += " status-timeout";
            if (hasError) rowClass += " status-error";
            // Fallback
            if (!hasEnvError && !hasTimeout && !hasError) rowClass += " status-failure";
        }
        const quote = (personalities['Standard'] as any)[baseLang] || (personalities['Standard'] as any)[lang] || "A mystery wrapped in code.";
        const safeQuote = quote.replace(/'/g, "&apos;") + ` Efficiency: ${efficiencyScore.toFixed(2)} MB/s`;

        // Metadata
        const meta = languageMetadata[baseLang] || languageMetadata[lang] || {};
        const year = meta.date || "0000";
        const displayNameRaw = lang === "C_Sharp" ? "C#" : (lang === "F_Sharp" ? "F#" : lang);
        let displayName = displayNameRaw;

        // Add algorithm badge for non-BruteForce algorithms
        const badgeAlgoType = m.algorithmType || 'BruteForce';
        let algoBadge = '';

        if (badgeAlgoType === 'DLX') {
            algoBadge = '<span style="margin-left: 6px; padding: 2px 6px; background: rgba(122, 162, 247, 0.2); border: 1px solid #7aa2f7; border-radius: 3px; font-size: 0.75em; color: #7aa2f7; font-weight: bold;" title="Dancing Links (Algorithm X)">DLX</span>';
        } else if (badgeAlgoType === 'CP') {
            algoBadge = '<span style="margin-left: 6px; padding: 2px 6px; background: rgba(187, 154, 247, 0.2); border: 1px solid #bb9af7; border-radius: 3px; font-size: 0.75em; color: #bb9af7; font-weight: bold;" title="Constraint Propagation">CP</span>';
        } else if (badgeAlgoType === 'BruteForce') {
            algoBadge = '<span style="margin-left: 6px; padding: 2px 6px; background: rgba(125, 207, 255, 0.2); border: 1px solid #7dcfff; border-radius: 3px; font-size: 0.75em; color: #7dcfff; font-weight: bold;" title="Brute Force (Backtracking)">BF</span>';
        }



        let typeIcon = '';
        if (runType === 'Docker') {
            typeIcon = '<span title="Docker Container" style="margin-left:5px; font-size: 0.8em;">🐳</span>';
        } else if (runType === 'AI') {
            typeIcon = '<span class="ai-tag" title="AI Generated">(AI)</span>';
        }

        const historyText = (meta.history || languageHistories[baseLang] || languageHistories[lang] || "Unknown.").replace(/'/g, "&apos;");

        // Logo Lookup with Special Handling
        let lookupKey = baseLang.toLowerCase();
        if (lookupKey === 'c#') lookupKey = 'c_sharp';
        if (lookupKey === 'f#') lookupKey = 'f_sharp';
        const localLogo = logoMap.get(lookupKey);
        const logoUrl = localLogo || meta.logo || meta.image;

        // Apply tailoring filters if configured
        const tailoring = (tailoringConfig as any)[baseLang] || (tailoringConfig as any)[lang];
        let filterStyle = "";
        if (tailoring?.invert) {
            filterStyle = "filter: url(#filter-invert);";
        } else if (tailoring?.transparent_white) {
            filterStyle = "filter: url(#filter-transparent-white);";
        }

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

        // Check for time unit based on timestamp (New runs after 2025-12-30 are in ms)
        // Heuristic: If timestamp is >= 2025-12-30, it's ms. Else, s.
        // Actually, let's be safe and check the value magnitude too? No, "0.0001" seconds is possible.
        // Let's rely on the date.
        const msCutoff = new Date('2025-12-30T00:00:00Z').getTime();
        const runTime = new Date(m.timestamp).getTime();
        const isMs = runTime >= msCutoff;
        const timeUnit = isMs ? 'ms' : 's';

        // Check if language failed (explicit flag or empty results for non-placeholder)
        const isFailed = m.failed === true || (m.results.length === 0 && m.runType !== 'Init');

        // Override Score/Tier if Failed
        let displayScore = normalizedScore.toFixed(2);
        if (isFailed) {
            normalizedScore = 999; // Force to bottom
            displayScore = "FAIL";
            tier = 'F';
            tierClass = 'tier-f';
        }

        // Check for failure status
        let failedBadge = '';
        if (m.failed) {
            const reason = (m.failureReason || 'Benchmark Failed').replace(/"/g, '&quot;');
            failedBadge = `<span style="margin-left: 6px; padding: 2px 6px; background: rgba(255, 0, 85, 0.2); border: 1px solid #ff0055; border-radius: 3px; font-size: 0.75em; color: #ff0055; font-weight: bold;" title="${reason}">⚠ FAILED</span>`;
        }

        // Validation badge
        let validationBadge = '';
        const langValidationIssues = validationIssues.filter(i => i.language === lang);
        if (langValidationIssues.length > 0) {
            const hasCritical = langValidationIssues.some(i => i.severity === 'CRITICAL');
            const severityClass = hasCritical ? 'critical' : 'warning';
            const icon = hasCritical
                ? '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><line x1="12" y1="8" x2="12" y2="12"/><line x1="12" y1="16" x2="12.01" y2="16"/></svg>'
                : '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"/><line x1="12" y1="9" x2="12" y2="13"/><line x1="12" y1="17" x2="12.01" y2="17"/></svg>';
            validationBadge = `<span class="validation-badge ${severityClass}" onclick="event.stopPropagation(); showDiagnosticsModal('${lang}')" title="${langValidationIssues.length} validation issue(s)">${icon}${langValidationIssues.length}</span>`;
        }

        const totalDisplayTime = isFailed ? "FAILED" : (isMs ? `${totalTime.toFixed(2)} ms` : `${totalTime.toFixed(3)} s`);
        const mismatchStyle = isMismatch ? 'cursor: pointer; background: rgba(255,0,85,0.1);' : '';
        const mismatchOnclick = isMismatch ? `onclick="showMismatchModal(this.closest('tr'))"` : '';
        const mismatchTitle = isMismatch ? `title="Click to see mismatch details"` : '';

        const safeId = lang.replace(/[^a-zA-Z0-9]/g, '_');
        html += `<tr class="main-row ${rowClass} ${isFastest ? 'fastest-row' : ''} ${isSlowest ? 'slowest-row' : ''}"
            onclick="toggleRow('expand-${safeId}')"
            data-lang="${lang}"
            data-algorithm-type="${m.algorithmType || 'BruteForce'}"
            data-timestamp="${new Date(m.timestamp).getTime()}"
            data-year="${year}"
            data-time="${isFailed ? 999999 : (totalTime < 0.0001 && totalTime > 0 ? totalTime.toExponential(2) : totalTime.toFixed(6))}"
            data-time-unit="${timeUnit}"
            data-iters="${totalIters}"
            data-mem="${maxMem}"
            data-memory="${maxMem}"
            data-compiler="${rowCompilerInfo}"
            data-score="${isFailed ? 999 : normalizedScore.toFixed(2)}"
            data-tier="${tier}"
            data-score-breakdown="Time: ${timeRatio.toFixed(2)}x | Mem: ${memRatio.toFixed(2)}x | CPU: ${cpuRatio.toFixed(2)}x"
            data-mismatch-details='${JSON.stringify(mismatchDetails).replace(/'/g, "&apos;")}'
            data-expected-iters="${expectedIters}"
            data-quote="${quote}" data-history='${historyText}' ${matrixDataAttrs}>
            <td class='lang-col' onclick="window.showLanguageDetails('${lang}', event.clientX, event.clientY); event.stopPropagation();" style="cursor: pointer;">
                ${logoUrl ? `<img src="${logoUrl}" alt="${displayNameRaw}" class="lang-logo" style="${filterStyle}">` : ''}
                <div style="display: inline-block; vertical-align: middle;">
                    <div>
                        ${displayName}${algoBadge}${typeIcon}${validationBadge}
                        ${isFailed ? '<span class="status-badge status-failure" style="font-size: 0.7em; margin-left: 5px; color: #ff0055; border: 1px solid #ff0055; padding: 1px 4px; border-radius: 3px;">FAILED</span>' : ''}
                    </div>
                    <div class='lang-year'>${year}</div>
                </div>
            </td>
            <td class="score-col">
                <div class="score-container" style="display: flex; align-items: center; justify-content: center; gap: 8px;">
                    <div class="tier-badge ${tierClass}" title="Tier ${tier}">${tier}</div>
                    ${isFailed ?
                        `<div class="total-score" style="color: #ff0055; font-size: 0.9em;">${displayScore}</div>` :
                        `<div class="score-decomposition"
                             onmouseenter="showScoreTooltip(event, '${lang.replace(/'/g, "\\'")}')"
                             onmouseleave="hideScoreTooltip()">
                            <div class="stacked-bar">
                                <div class="bar-segment bar-time" style="width: 80%"></div>
                                <div class="bar-segment bar-memory" style="width: 20%"></div>
                            </div>
                            <span class="score-value">${displayScore}</span>
                        </div>`
                    }
                </div>
            </td>
            <td class="updated-cell">
                <div style="text-align: center; color: var(--secondary); font-size: 0.9em;">
                    <span class="relative-time" data-ts="${new Date(m.timestamp).getTime()}"></span>
                </div>
            </td>`;

        for (let i = 0; i < maxMatrices; i++) {
            const r = m.results.find((res: any) => normalizeMatrix(res.matrix) === String(i + 1));

            if (isFailed) {
                // Explicit Failure Cell
                html += `<td class="matrix-cell" data-matrix-index="${i}">
                    <div style="color: #ff0055; font-size: 0.8em; font-weight: bold; opacity: 0.7;">FAILED</div>
                 </td>`;
            } else if (r) {
                const memMb = r.memory / 1024 / 1024;

                let scoreDisplay = "";
                if (cTimes[i] && cTimes[i] > 0) {
                    const scoreVal = r.time / cTimes[i];
                    scoreDisplay = `${scoreVal.toFixed(2)}x`;
                }

                const displayTime = isMs ? `${r.time.toFixed(2)} ms` : `${r.time.toFixed(4)} s`;

                html += `<td class="matrix-cell" data-matrix-index="${i}">
                    <div class="cell-content">
                        <div class="cell-header">
                             <div class="time" title="Wall Clock Time">${displayTime}</div>
                        </div>
                        <div class="meta">
                            ${(() => {
                        const cRes = cMetrics?.results.find(res => normalizeMatrix(res.matrix) === normalizeMatrix(r.matrix));
                        const cIterations = cRes ? cRes.iterations : null;
                        // Ensure strict number comparison and handle potential type issues if JSON parsing was loose
                        const rIter = Number(r.iterations);
                        const cIter = Number(cIterations);
                        const isBaseline = m.solver === 'C' && (m.runType === 'Local' || !m.runType);
                        const isMismatch = !isBaseline && cIterations !== null && rIter !== cIter && m.algorithmType !== 'CP';

                        return `<span title="Iterations: ${rIter} vs C: ${cIter}" class="${isMismatch ? 'mismatch' : ''}">#${r.iterations}</span>`;
                    })()}
                            <span title="Memory">${memMb.toFixed(1)}M</span>
                        </div>
                    </div>
                </td>`;
            } else {
                // Empty cell - show run button for unlocked languages
                html += `<td class="matrix-cell" data-matrix-index="${i}">
                    <span style="color: #333">-</span>
                </td>`;
            }
        }

        html += `<td class='total-time' style="${mismatchStyle}" ${mismatchOnclick} ${mismatchTitle}><div style='display:flex;flex-direction:column;align-items:center;'><div>${totalDisplayTime}</div><div style='font-size:0.6em;color:${isMismatch ? '#ff0055' : '#5c5c66'};'>${totalIters.toLocaleString()} iters${isMismatch ? ' ⚠' : ''}</div></div></td></tr>`;
    });

    html += `
        </tbody></table></div>

        <script>
            // Static Data
            window.personalities = ${safeJSON(personalities)};
            window.narratorIntros = ${safeJSON(narratorIntros)};
            window.languageMetadata = ${safeJSON(languageMetadata)};
            window.methodologyTexts = ${safeJSON(methodologyTexts)};
            window.mismatchLabels = ${safeJSON(mismatchLabels)};
            window.iterationLabels = ${safeJSON(iterationLabels)};
            window.timeLabels = ${safeJSON(timeLabels)};
            window.memoryLabels = ${safeJSON(memoryLabels)};
            window.scoreLabels = ${safeJSON(scoreLabels)};
            window.diagnosticsData = ${safeJSON(diagnostics)};
            window.sourceCodeData = ${safeJSON(sourceCodeData)};
            window.validationIssues = ${safeJSON(validationIssues)};

            // Validation helper function
            window.getValidationIssues = function(language) {
                return window.validationIssues.filter(i => i.language === language);
            };

            // Dynamic Data
            window.referenceOutputs = ${safeJSON(referenceOutputs)};
            window.benchmarkConfig = ${safeJSON(benchmarkConfig)};
            window.tailoring = ${safeJSON(tailoringConfig)};
            window.scoringAnalysisData = ${scoringAnalysisJson};
            window.metricsData = ${safeJSON(metrics.filter(m => m.solver).map(m => {
        const baseLang = m.solver.replace(/ \((AI)\)$/, '');

        // Logo Lookup with Special Handling
        let lookupKey = baseLang.toLowerCase();
        if (lookupKey === 'c#') lookupKey = 'c_sharp';
        if (lookupKey === 'f#') lookupKey = 'f_sharp';
        const localLogo = logoMap.get(lookupKey);

        const meta = languageMetadata[baseLang] || languageMetadata[m.solver] || {};

        // Calculate Score & Tier for Client Use
        // Compare only against C's results for the SAME matrices this language ran
        const times = m.results.map(r => r.time);
        const mems = m.results.map(r => r.memory || 0).filter(m => !isNaN(m));
        const totalTime = times.reduce((a, b) => a + b, 0);
        const maxMem = mems.length > 0 ? Math.max(...mems) : 0;
        const totalCpu = m.results.reduce((a, b) => a + b.cpu_user + b.cpu_sys, 0);

        // Calculate C baseline only for matrices this language ran
        // Use algorithm-specific C baseline for comparison
        const algorithmType = m.algorithmType || 'BruteForce';
        const cBaselineForAlgo = cMetricsByAlgorithm.get(algorithmType) || cMetrics;

        let cMatchedTime = 0;
        let cMatchedMem = 0;
        let cMatchedCpu = 0;
        if (cBaselineForAlgo) {
            m.results.forEach(r => {
                const cRes = cBaselineForAlgo.results.find(cr => normalizeMatrix(cr.matrix) === normalizeMatrix(r.matrix));
                if (cRes) {
                    cMatchedTime += cRes.time;
                    cMatchedMem = Math.max(cMatchedMem, cRes.memory || 0);
                    cMatchedCpu += cRes.cpu_user + cRes.cpu_sys;
                }
            });
        }

        const timeRatio = Math.max(0.001, (cMatchedTime > 0) ? (totalTime / cMatchedTime) : 1);
        const memRatio = Math.max(0.001, (cMatchedMem > 0) ? (maxMem / cMatchedMem) : 1);
        const cpuRatio = Math.max(0.001, (cMatchedCpu > 0) ? (totalCpu / cMatchedCpu) : 1);

        // Weighted Geometric Mean: (Time^0.8 * Mem^0.2)
        const score = Math.pow(timeRatio, 0.8) * Math.pow(memRatio, 0.2);

        let tier = 'F';
        if (score < 0.95) tier = 'S';
        else if (score < 1.05) tier = 'A';
        else if (score < 1.50) tier = 'B';
        else if (score < 3.00) tier = 'C';
        else if (score < 10.00) tier = 'D';

        return {
            ...m,
            score: score,
            tier: tier,
            logo: localLogo || meta.logo || meta.image || "https://upload.wikimedia.org/wikipedia/commons/thumb/d/db/Alchemist_symbol_for_process_2.svg/120px-Alchemist_symbol_for_process_2.svg.png"
        };
    }))};
            window.matrixPuzzles = ${safeJSON(matrixContents)};
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

    // Read and inline report_client.js to avoid file:// loading issues
    let clientScript = '';
    try {
        clientScript = fsSync.readFileSync(path.join(__dirname, 'report_client.js'), 'utf8');
    } catch (e) {
        console.error('Warning: Could not read report_client.js, falling back to external script');
    }

    // Verify C Baselines integrity before generation
    console.log(`Injecting C_Baselines with ${Object.keys(C_Baselines).length} algos`);
    if (Object.keys(C_Baselines).length === 0) console.warn("WARNING: C_Baselines object is empty!");

    // Inject Data SCRIPTS FIRST (so they are available to report_client.js if needed immediately)
    html += `
    <script>
        window.cBaselines = ${safeJSON(C_Baselines)};
        window.currentAlgorithm = 'all';
    </script>
    `;

    html += `
    <footer style="text-align: right; padding: 20px 40px; color: #666; font-size: 12px; border-top: 1px solid #333; margin-top: 40px;">
        Report generated: ${generatedAt} CET
    </footer>
    ${clientScript ? `<script>\n${clientScript}\n</script>` : '<script src="./Metrics/report_client.js"></script>'}
    
    <script>
    // Other initialization that depends on client script being loaded
    (function() {
        // Verify injection on client side
        console.log("Client-side C Baselines:", window.cBaselines);
    })();

    // Scoring Analysis Interactive Functions
    function toggleRow(rowId) {
        const row = document.getElementById(rowId);
        if (!row) return;
        const mainRow = row.previousElementSibling;

        if (row.style.display === 'none') {
            row.style.display = 'table-row';
            if (mainRow) mainRow.classList.add('expanded');
            populateSensitivityRow(rowId);
        } else {
            row.style.display = 'none';
            if (mainRow) mainRow.classList.remove('expanded');
        }
    }

    function populateSensitivityRow(rowId) {
        if (!window.scoringAnalysisData) return;

        const language = rowId.replace('expand-', '').replace(/_/g, ' ');
        // Handle special cases like C_Sharp -> C#
        let langLookup = language;
        if (language === 'C Sharp') langLookup = 'C#';
        if (language === 'F Sharp') langLookup = 'F#';
        if (language === 'C  ') langLookup = 'C++';

        const langData = window.scoringAnalysisData.sensitivity.find(s =>
            s.language === langLookup || s.language === language
        );
        const stabilityData = window.scoringAnalysisData.stability.find(s =>
            s.language === langLookup || s.language === language
        );

        if (!langData) return;

        const chartEl = document.getElementById('chart-' + rowId.replace('expand-', ''));
        const gaugesEl = document.getElementById('gauges-' + rowId.replace('expand-', ''));
        const swingEl = document.getElementById('swing-' + rowId.replace('expand-', ''));

        // Render horizontal bar chart for rank sensitivity
        if (chartEl) {
            const maxRank = Math.max(...langData.scenarios.map(s => s.rank));
            chartEl.innerHTML = langData.scenarios.map(s => {
                const barWidth = (s.rank / maxRank) * 100;
                const isCurrent = s.scenario === 'Current (80/20)';
                return \`
                    <div class="sensitivity-bar-row \${isCurrent ? 'current-scenario' : ''}">
                        <div class="bar-label">
                            <span class="scenario-name">\${s.scenario}</span>
                            <span class="scenario-rank">#\${s.rank}</span>
                        </div>
                        <div class="bar-container">
                            <div class="bar-fill" style="width: \${barWidth}%">
                                <span class="bar-value">\${s.score.toFixed(2)}</span>
                            </div>
                        </div>
                    </div>
                \`;
            }).join('');
        }

        // Render metric gauges
        if (gaugesEl) {
            const metric = window.allMetrics.find(m =>
                m.solver === langLookup || m.solver === language
            );

            if (metric && metric.results && metric.results.length > 0) {
                const avgTime = metric.results.reduce((sum, r) => sum + (r.seconds || 0), 0) / metric.results.length;
                const avgMem = metric.results.reduce((sum, r) => sum + (r.memoryKB || 0), 0) / metric.results.length;
                const avgIter = metric.results.reduce((sum, r) => sum + (r.iterations || 0), 0) / metric.results.length;

                gaugesEl.innerHTML = \`
                    <div class="gauge-row">
                        <div class="gauge">
                            <div class="gauge-label">Avg Time</div>
                            <div class="gauge-display">\${avgTime.toFixed(3)}s</div>
                            <div class="gauge-meter">
                                <div class="gauge-fill" style="width: \${Math.min(100, (avgTime / 10) * 100)}%"></div>
                            </div>
                        </div>
                        <div class="gauge">
                            <div class="gauge-label">Avg Memory</div>
                            <div class="gauge-display">\${(avgMem / 1024).toFixed(1)} MB</div>
                            <div class="gauge-meter">
                                <div class="gauge-fill" style="width: \${Math.min(100, (avgMem / 50000) * 100)}%"></div>
                            </div>
                        </div>
                        <div class="gauge">
                            <div class="gauge-label">Avg Iterations</div>
                            <div class="gauge-display">\${avgIter.toLocaleString()}</div>
                            <div class="gauge-meter">
                                <div class="gauge-fill" style="width: \${Math.min(100, (avgIter / 500000) * 100)}%"></div>
                            </div>
                        </div>
                    </div>
                \`;
            }
        }

        if (swingEl && stabilityData) {
            swingEl.textContent = stabilityData.maxSwing;
        }
    }

    let tooltipEl = null;

    function showScoreTooltip(event, language) {
        if (!window.scoringAnalysisData) return;

        if (!tooltipEl) {
            tooltipEl = document.createElement('div');
            tooltipEl.className = 'score-tooltip';
            document.body.appendChild(tooltipEl);
        }

        // Handle special character replacements
        let langLookup = language;
        if (language === 'C_Sharp') langLookup = 'C#';
        if (language === 'F_Sharp') langLookup = 'F#';

        const langData = window.scoringAnalysisData.sensitivity.find(s =>
            s.language === langLookup || s.language === language ||
            s.language.replace(/[#\\+]/g, '_') === language.replace(/[#\\+]/g, '_')
        );
        const currentScenario = langData?.scenarios.find(s => s.scenario === 'Current (80/20)');

        if (currentScenario) {
            const timeComponent = currentScenario.score * 0.8;
            const memoryComponent = currentScenario.score * 0.2;
            tooltipEl.innerHTML = \`
                <div class="tooltip-row">
                    <span class="label">Time (80%):</span>
                    <span class="value time">\${timeComponent.toFixed(2)}</span>
                </div>
                <div class="tooltip-row">
                    <span class="label">Memory (20%):</span>
                    <span class="value memory">\${memoryComponent.toFixed(2)}</span>
                </div>
                <div class="tooltip-row">
                    <span class="label">Total:</span>
                    <span class="value">\${currentScenario.score.toFixed(2)}</span>
                </div>
            \`;
        }

        tooltipEl.style.display = 'block';
        tooltipEl.style.left = (event.pageX + 10) + 'px';
        tooltipEl.style.top = (event.pageY - 10) + 'px';
    }

    function hideScoreTooltip() {
        if (tooltipEl) {
            tooltipEl.style.display = 'none';
        }
    }

    // Populate Scoring Insights section
    function populateScoringInsights() {
        const data = window.scoringAnalysisData;
        if (!data) return;

        // Correlation - Enhanced
        const r2El = document.getElementById('correlation-r2');
        const interpEl = document.getElementById('correlation-interpretation');
        const barEl = document.getElementById('correlation-bar');
        if (r2El && interpEl && data.correlation) {
            const r2Value = data.correlation.rSquared;
            r2El.textContent = r2Value.toFixed(3);
            interpEl.textContent = data.correlation.interpretation;

            // Animate the bar
            if (barEl) {
                setTimeout(() => {
                    barEl.style.width = (r2Value * 100) + '%';
                }, 100);
            }
        }

        // Rank Stability - Enhanced with visualization
        const stableEnhancedEl = document.getElementById('stable-languages-enhanced');
        const unstableEnhancedEl = document.getElementById('unstable-languages-enhanced');
        if (stableEnhancedEl && unstableEnhancedEl && data.stability) {
            // Most stable (lowest swing)
            const sortedByStability = [...data.stability].sort((a, b) => a.maxSwing - b.maxSwing);
            const mostStable = sortedByStability.slice(0, 5);
            stableEnhancedEl.innerHTML = mostStable.map((s, index) =>
                \`<div class="stability-item" style="animation-delay: \${index * 0.1}s">
                    <span class="stability-item-name">\${s.language}</span>
                    <span class="stability-item-value">±\${s.maxSwing}</span>
                </div>\`
            ).join('');

            // Most unstable (highest swing)
            const mostUnstable = sortedByStability.slice(-5).reverse();
            unstableEnhancedEl.innerHTML = mostUnstable.map((s, index) =>
                \`<div class="stability-item" style="animation-delay: \${index * 0.1}s">
                    <span class="stability-item-name">\${s.language}</span>
                    <span class="stability-item-value">±\${s.maxSwing}</span>
                </div>\`
            ).join('');

            // Draw visualization
            setTimeout(drawStabilityVisualization, 200);
        }

        // Outliers - Enhanced
        const outlierEnhancedEl = document.getElementById('outlier-list-enhanced');
        const noOutliersEnhancedEl = document.getElementById('no-outliers-enhanced');
        if (outlierEnhancedEl && noOutliersEnhancedEl && data.outliers) {
            if (data.outliers.length === 0) {
                outlierEnhancedEl.style.display = 'none';
                noOutliersEnhancedEl.style.display = 'flex';
            } else {
                const colors = [
                    '#fa709a', '#fee140', '#30cfd0', '#a8edea', '#ff6b6b', '#4ecdc4'
                ];
                outlierEnhancedEl.innerHTML = data.outliers.map((o, index) =>
                    \`<div class="outlier-item" style="border-color: \${colors[index % colors.length]}; animation: sectionFadeIn 0.6s ease forwards; animation-delay: \${index * 0.1}s; opacity: 0;">
                        <div class="outlier-item-header">
                            <span class="outlier-item-name">\${o.language}</span>
                            <span class="outlier-badge" style="background: \${colors[index % colors.length]}22; color: \${colors[index % colors.length]};">\${o.metric}</span>
                        </div>
                        <p class="outlier-item-desc">\${o.explanation}</p>
                    </div>\`
                ).join('');
            }
        }
    }

    // Call on page load
    document.addEventListener('DOMContentLoaded', populateScoringInsights);

    // Validation styling helper for charts
    window.hasValidationIssues = function(solver) {
        return window.validationIssues && window.validationIssues.some(i => i.language === solver);
    };

    // Scoring Insights Modal Functions
    function showScoringInsights() {
        // Scroll page to top before showing modal
        window.scrollTo(0, 0);
        document.getElementById('scoringInsightsModal').classList.add('visible');
    }

    function closeScoringInsights(event) {
        if (event.target.id === 'scoringInsightsModal' ||
            event.target.classList.contains('modal-close') ||
            event.target.classList.contains('insights-close-btn')) {
            document.getElementById('scoringInsightsModal').classList.remove('visible');
        }
    }

    // Draw rank stability visualization
    function drawStabilityVisualization() {
        const canvas = document.getElementById('stabilityVizCanvas');
        if (!canvas) return;

        const data = window.scoringAnalysisData;
        if (!data || !data.stability) return;

        const ctx = canvas.getContext('2d');
        const width = canvas.width;
        const height = canvas.height;
        const padding = 60;
        const chartWidth = width - 2 * padding;
        const chartHeight = height - 2 * padding;

        // Clear canvas
        ctx.clearRect(0, 0, width, height);

        // Get top 10 languages sorted by max swing
        const sorted = [...data.stability].sort((a, b) => b.maxSwing - a.maxSwing);
        const top10 = sorted.slice(0, 10);

        const barHeight = chartHeight / top10.length;
        const maxSwing = Math.max(...top10.map(d => d.maxSwing));

        // Draw background grid
        ctx.strokeStyle = 'rgba(255, 255, 255, 0.05)';
        ctx.lineWidth = 1;
        for (let i = 0; i <= 5; i++) {
            const x = padding + (chartWidth / 5) * i;
            ctx.beginPath();
            ctx.moveTo(x, padding);
            ctx.lineTo(x, height - padding);
            ctx.stroke();
        }

        // Draw bars
        top10.forEach((lang, index) => {
            const y = padding + index * barHeight;
            const barWidth = (lang.maxSwing / maxSwing) * chartWidth;

            // Gradient color based on volatility
            const gradient = ctx.createLinearGradient(padding, 0, padding + barWidth, 0);
            const hue = 120 - (lang.maxSwing / maxSwing) * 120; // Green to Red
            gradient.addColorStop(0, \`hsla(\${hue}, 70%, 50%, 0.3)\`);
            gradient.addColorStop(1, \`hsla(\${hue}, 70%, 50%, 0.8)\`);

            // Draw bar
            ctx.fillStyle = gradient;
            ctx.fillRect(padding, y + barHeight * 0.2, barWidth, barHeight * 0.6);

            // Draw border
            ctx.strokeStyle = \`hsla(\${hue}, 70%, 50%, 1)\`;
            ctx.lineWidth = 2;
            ctx.strokeRect(padding, y + barHeight * 0.2, barWidth, barHeight * 0.6);

            // Draw language name
            ctx.fillStyle = '#fff';
            ctx.font = '12px JetBrains Mono, monospace';
            ctx.textAlign = 'right';
            ctx.fillText(lang.language, padding - 10, y + barHeight * 0.5 + 4);

            // Draw value
            ctx.textAlign = 'left';
            ctx.fillStyle = '#fff';
            ctx.fillText(\`±\${lang.maxSwing}\`, padding + barWidth + 10, y + barHeight * 0.5 + 4);
        });

        // Draw title
        ctx.fillStyle = '#888';
        ctx.font = '14px JetBrains Mono, monospace';
        ctx.textAlign = 'center';
        ctx.fillText('Rank Volatility (Max Position Swing)', width / 2, padding - 20);

        // Draw axis label
        ctx.fillText('Position Changes', width / 2, height - padding + 35);
    }

    // Validation Diagnostics Modal Functions
    window.showDiagnosticsModal = function(language) {
        const issues = window.getValidationIssues(language);
        const modal = document.getElementById('validation-diagnostics-modal-overlay');
        const title = document.getElementById('validation-diagnostics-modal-title');
        const content = document.getElementById('validation-diagnostics-modal-content');

        // Normalize language name for display
        const displayName = language.replace('_Sharp', '#').replace(/_Plus/g, '+');
        title.textContent = displayName + ' - Validation Issues';

        if (issues.length === 0) {
            content.innerHTML = '<p style="color: var(--primary);">No validation issues found.</p>';
        } else {
            content.innerHTML = issues.map(issue => {
                const severityClass = issue.severity.toLowerCase();
                return \`
                    <div class="issue-item">
                        <span class="issue-severity \${severityClass}">\${issue.severity}</span>
                        <span style="margin-left: 8px; color: #aaa;">Matrix \${issue.matrix}</span>
                        <p style="margin: 8px 0 0 0; color: var(--text);">\${issue.message}</p>
                        \${issue.failure_type === 'iteration_mismatch' ? \`
                            <div class="iteration-compare">
                                <div class="expected">
                                    <label>Expected</label>
                                    <div class="value">\${issue.expected_iterations.toLocaleString()}</div>
                                </div>
                                <div class="actual">
                                    <label>Actual</label>
                                    <div class="value">\${issue.actual_iterations.toLocaleString()}</div>
                                </div>
                            </div>
                        \` : ''}
                    </div>
                \`;
            }).join('');
        }

        modal.classList.add('visible');
    };

    window.hideDiagnosticsModal = function() {
        document.getElementById('validation-diagnostics-modal-overlay').classList.remove('visible');
    };

    // Close on ESC key
    document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
            window.hideDiagnosticsModal();
        }
    });
    </script>
    `;

    // Verify C Baselines injection (Server-side log, visible in generation output)
    console.log("Static C Baselines injected.");

    html += `
    <script>
    // Filter table rows by algorithm
function filterByAlgorithm(algo) {
    const rows = document.querySelectorAll('table tbody tr');
    const links = document.querySelectorAll('#algorithmSelectorBtn + .dropdown-content a');

    // Update active link
    links.forEach(link => {
        link.classList.remove('active');
        const linkAlgo = link.getAttribute('onclick').match(/'([^']+)'/)[1];
        if (linkAlgo === algo) {
            link.classList.add('active');
        }
    });

    // Filter rows
    rows.forEach(row => {
        const langCell = row.querySelector('td:first-child');
        if (!langCell) return;

        const language = langCell.textContent.trim();
        const rowAlgo = row.getAttribute('data-algorithm-type') || 'BruteForce'; // Default to BruteForce

        if (algo === 'all' || rowAlgo === algo) {
            row.style.display = '';
        } else {
            row.style.display = 'none';
        }
    });

    // Save to localStorage
    try {
        localStorage.setItem('sudoku-benchmark-algorithm', algo);
    } catch (e) { /* ignore */ }

    // Update button text
    const btn = document.getElementById('algorithmSelectorBtn');
    if (btn) {
        const algoNames = {
            'all': 'All Algorithms',
            'BruteForce': 'Brute Force',
            'DLX': 'Dancing Links',
            'CP': 'Constraint Propagation'
        };
        btn.textContent = (algoNames[algo] || 'ALGORITHM') + ' ▾';
    }
}

    // Initialize on load - restore from localStorage or default to all
document.addEventListener('DOMContentLoaded', function () {
    let savedAlgo = 'all';
    try {
        const stored = localStorage.getItem('sudoku-benchmark-algorithm');
        if (stored && ['all', 'BruteForce', 'DLX', 'CP'].includes(stored)) {
            savedAlgo = stored;
        }
    } catch (e) { /* localStorage unavailable */ }

    filterByAlgorithm(savedAlgo);
});
</script>

    <!-- Scoring Insights Modal -->
    <div id="scoringInsightsModal" class="modal-overlay" onclick="closeScoringInsights(event)">
        <div class="insights-modal-container" onclick="event.stopPropagation()">
            <button class="insights-close-btn" onclick="event.stopPropagation(); closeScoringInsights(event);">&times;</button>

            <!-- Hero Header -->
            <div class="insights-hero">
                <div class="insights-hero-bg"></div>
                <div class="insights-hero-content">
                    <div class="insights-icon">
                        <svg width="64" height="64" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                            <path d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/>
                            <path d="M10 7v6l4 2"/>
                        </svg>
                    </div>
                    <h1 class="insights-title">Scoring Insights</h1>
                    <p class="insights-subtitle">Deep dive into performance metrics and ranking behavior</p>
                </div>
            </div>

            <!-- Insights Content -->
            <div class="insights-content">
                <!-- Correlation Section -->
                <section class="insights-section">
                    <div class="section-header">
                        <div class="section-icon" style="background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);">
                            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <line x1="18" y1="20" x2="18" y2="10"/>
                                <line x1="12" y1="20" x2="12" y2="4"/>
                                <line x1="6" y1="20" x2="6" y2="14"/>
                            </svg>
                        </div>
                        <div>
                            <h2 class="section-title">Time vs Memory Correlation</h2>
                            <p class="section-desc">Statistical relationship between execution time and memory usage</p>
                        </div>
                    </div>
                    <div class="insights-card correlation-card">
                        <div class="correlation-display">
                            <div class="correlation-value">
                                <div class="value-label">R² Coefficient</div>
                                <div class="value-number" id="correlation-r2">--</div>
                                <div class="value-bar">
                                    <div class="value-bar-fill" id="correlation-bar"></div>
                                </div>
                            </div>
                            <div class="correlation-interpretation">
                                <div class="interpretation-icon">💡</div>
                                <p id="correlation-interpretation">Loading analysis...</p>
                            </div>
                        </div>
                    </div>
                </section>

                <!-- Rank Stability Section with Visualization -->
                <section class="insights-section">
                    <div class="section-header">
                        <div class="section-icon" style="background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);">
                            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/>
                            </svg>
                        </div>
                        <div>
                            <h2 class="section-title">Rank Stability Analysis</h2>
                            <p class="section-desc">How language rankings shift across different weighting scenarios</p>
                        </div>
                    </div>
                    <div class="insights-card stability-card">
                        <!-- Visualization Canvas -->
                        <div class="stability-viz-container">
                            <canvas id="stabilityVizCanvas" width="800" height="300"></canvas>
                        </div>
                        <div class="stability-grid">
                            <div class="stability-column stable">
                                <div class="column-header">
                                    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                        <polyline points="20 6 9 17 4 12"/>
                                    </svg>
                                    <span>Most Stable</span>
                                </div>
                                <div class="stability-list" id="stable-languages-enhanced"></div>
                            </div>
                            <div class="stability-divider"></div>
                            <div class="stability-column volatile">
                                <div class="column-header">
                                    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                        <path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/>
                                    </svg>
                                    <span>Most Volatile</span>
                                </div>
                                <div class="stability-list" id="unstable-languages-enhanced"></div>
                            </div>
                        </div>
                    </div>
                </section>

                <!-- Outliers Section -->
                <section class="insights-section">
                    <div class="section-header">
                        <div class="section-icon" style="background: linear-gradient(135deg, #fa709a 0%, #fee140 100%);">
                            <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <circle cx="12" cy="12" r="10"/>
                                <path d="M12 16v-4"/>
                                <path d="M12 8h.01"/>
                            </svg>
                        </div>
                        <div>
                            <h2 class="section-title">Statistical Outliers</h2>
                            <p class="section-desc">Languages with exceptional performance characteristics (IQR method)</p>
                        </div>
                    </div>
                    <div class="insights-card outliers-card">
                        <div class="outliers-list" id="outlier-list-enhanced"></div>
                        <div class="no-outliers" id="no-outliers-enhanced" style="display: none;">
                            <svg width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                                <circle cx="12" cy="12" r="10"/>
                                <line x1="15" y1="9" x2="9" y2="15"/>
                                <line x1="9" y1="9" x2="15" y2="15"/>
                            </svg>
                            <p>No statistical outliers detected</p>
                        </div>
                    </div>
                </section>
            </div>
        </div>
    </div>

    <!-- Interactive Solver Modal -->
    <div id="solver-modal" class="modal-overlay">
        <div class="solver-modal-content" style="max-width: 1200px; width: 95%; max-height: 95vh; overflow-y: auto; background: #0a0a0f; border-radius: 8px; border: 1px solid rgba(0, 255, 157, 0.3); position: relative; padding: 2rem;">
            <button class="modal-close" onclick="closeInteractiveSolver()" style="position: absolute; right: 15px; top: 15px; font-size: 24px; background: none; border: none; color: #888; cursor: pointer; width: 32px; height: 32px; display: flex; align-items: center; justify-content: center; border-radius: 4px; transition: all 0.2s;">&times;</button>
            <div id="interactive-solver-section">
                <!-- Content populated by interactive-solver.js -->
            </div>
        </div>
    </div>

    <!-- Interactive Solver Module -->
    <script type="module">
        import { initInteractiveSolver } from './Metrics/modules/interactive-solver.js';

        // Store solver instance globally for modal lifecycle
        window.solverInstance = null;

        // Launch modal
        window.launchInteractiveSolver = function() {
            const modal = document.getElementById('solver-modal');
            const container = document.getElementById('interactive-solver-section');

            if (!modal || !container) {
                console.error('[Solver Modal] Modal or container not found', { modal: !!modal, container: !!container });
                return;
            }

            console.log('[Solver Modal] Launching solver modal');

            // Show modal using visible class (like other modals)
            modal.classList.add('visible');
            document.body.style.overflow = 'hidden';

            // Scroll page to top
            window.scrollTo(0, 0);

            // Scroll modal content to top
            const modalContent = modal.querySelector('.solver-modal-content');
            if (modalContent) {
                modalContent.scrollTop = 0;
            }

            // Initialize solver if not already done
            if (!window.solverInstance) {
                console.log('[Solver Modal] Initializing solver instance');
                window.solverInstance = initInteractiveSolver();
            }
        };

        // Close modal
        window.closeInteractiveSolver = function() {
            const modal = document.getElementById('solver-modal');
            if (!modal) return;

            console.log('[Solver Modal] Closing solver modal');

            // Hide modal using visible class
            modal.classList.remove('visible');
            document.body.style.overflow = '';

            // Cleanup solver instance if it exists
            if (window.solverInstance && typeof window.solverInstance.cleanup === 'function') {
                window.solverInstance.cleanup();
                window.solverInstance = null;
            }
        };

        // Close on backdrop click
        document.addEventListener('click', function(e) {
            const modal = document.getElementById('solver-modal');
            if (e.target === modal) {
                window.closeInteractiveSolver();
            }
        });

        // Close on Escape key
        document.addEventListener('keydown', function(e) {
            if (e.key === 'Escape') {
                const modal = document.getElementById('solver-modal');
                if (modal && modal.style.display === 'flex') {
                    window.closeInteractiveSolver();
                }
            }
        });
    </script>

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
    < !DOCTYPE html >
        <html lang="en" >
            <head>
            <meta charset="UTF-8" >
                <title>Benchmark History </title>
                    < link href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap" rel = "stylesheet" >
                        <link rel="stylesheet" href = "./Metrics/index.css" >
                            <style>
                            ${SharedStyles}
            /* History-specific overrides */
            body { padding: 40px; }
            .history - container { max - width: 1400px; margin: 0 auto; }
            .status - success { color: #00ff9d; font - weight: bold; }
            .status - failure, .status - error, .status - timeout { color: #ff0055; font - weight: bold; }
            .time - val { font - family: 'JetBrains Mono', monospace; color: #fff; }
</style>
    <script>
function sortTable(n) {
    var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
    table = document.getElementById("historyTable");
    switching = true;
    dir = "asc";
    while (switching) {
        switching = false;
        rows = table.rows;
        // Start loop at 1 to skip header
        for (i = 1; i < (rows.length - 1); i++) {
            shouldSwitch = false;
            x = rows[i].getElementsByTagName("TD")[n];
            y = rows[i + 1].getElementsByTagName("TD")[n];
            var xContent = x.innerText.toLowerCase();
            var yContent = y.innerText.toLowerCase();

            // Numeric sort for Time (3) and Iterations (4)
            if (n === 3 || n === 4) {
                xContent = parseFloat(xContent.replace(/[^0-9.]/g, '')) || 0;
                yContent = parseFloat(yContent.replace(/[^0-9.]/g, '')) || 0;
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
            switchcount++;
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
    < body >
    <div class="history-container" >
        <h1>Benchmark History </h1>

            < div style = "margin-bottom: 20px; text-align: right;" >
                <a href="benchmark_report.html" class="btn" > View Latest Report </a>
                    </div>

                    < div class="container" style = "width: 100%; padding: 0;" >
                        <table id="historyTable" >
                            <thead>
                            <tr>
                            <th onclick="sortTable(0)" style = "cursor: pointer;" > Timestamp ↕</th>
                                < th onclick = "sortTable(1)" style = "cursor: pointer;" > Solver ↕</th>
                                    < th onclick = "sortTable(2)" style = "cursor: pointer;" > Matrix ↕</th>
                                        < th onclick = "sortTable(3)" style = "cursor: pointer;" > Time ↕</th>
                                            < th onclick = "sortTable(4)" style = "cursor: pointer;" > Iterations ↕</th>
                                                < th onclick = "sortTable(5)" style = "cursor: pointer;" > Status ↕</th>
                                                    </tr>
                                                    </thead>
                                                    <tbody>
                        ${rows.map(r => {
        const statusLower = (r.status || 'unknown').toLowerCase();
        // Heuristic for time unit: after Dec 30 2025 is ms
        const isMs = new Date(r.timestamp).getTime() >= new Date('2025-12-30').getTime();
        const timeDisplay = isMs ? `${(r.time ?? 0).toFixed(2)} ms` : `${(r.time ?? 0).toFixed(4)} s`;

        return `
                            <tr>
                                <td style="color: var(--muted); font-size: 0.9em;">${new Date(r.timestamp).toLocaleString()}</td>
                                <td style="color: var(--primary); font-weight: bold;">${r.solver}</td>
                                <td>${r.matrix}</td>
                                <td class="time-val">${timeDisplay}</td>
                                <td>${r.iterations?.toLocaleString() ?? '-'}</td>
                                <td class="status-${statusLower}">${r.status || '-'}</td>
                            </tr>
                            `;
    }).join('')
        }
</tbody>
    </table>
    </div>
    </div>
    </body>
    </html>
        `;
}
