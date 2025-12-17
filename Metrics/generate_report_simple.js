#!/usr/bin/env node
/**
 * Simplified JavaScript Report Generator
 * Generates benchmark report without TypeScript dependencies
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { glob } from 'glob';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');

async function generateSimpleReport() {
    try {
        console.log('Starting simple report generation...');

        // Load metrics from Languages directory
        const metricsPattern = path.join(rootDir, 'Languages', '*', 'metrics.json');
        const metricFiles = await glob(metricsPattern);
        console.log(`Found ${metricFiles.length} metric files`);

        const allMetrics = [];
        const knownSolvers = new Set();

        for (const file of metricFiles) {
            try {
                const content = await fs.promises.readFile(file, 'utf-8');
                let parsedMetrics = JSON.parse(content);

                // Extract solver name from path
                const pathParts = file.split(path.sep);
                const langIndex = pathParts.lastIndexOf('Languages');
                const solverName = (langIndex !== -1 && pathParts[langIndex + 1])
                    ? pathParts[langIndex + 1]
                    : "Unknown";

                if (!knownSolvers.has(solverName)) {
                    const stats = await fs.promises.stat(file);

                    // Check for run_type
                    let runType = 'Local';
                    try {
                        const runTypePath = path.join(path.dirname(file), 'run_type');
                        runType = (await fs.promises.readFile(runTypePath, 'utf-8')).trim();
                    } catch (e) {
                        // ignore
                    }

                    const metricEntry = {
                        solver: solverName,
                        timestamp: stats.mtimeMs,
                        runType: runType,
                        results: Array.isArray(parsedMetrics) ? parsedMetrics : [parsedMetrics]
                    };

                    allMetrics.push(metricEntry);
                    knownSolvers.add(solverName);
                }
            } catch (e) {
                console.warn(`Error reading ${file}:`, e.message);
            }
        }

        console.log(`Loaded ${allMetrics.length} metrics`);

        // Load metadata overrides if available
        let metadataOverrides = {};
        const metadataPath = path.join(rootDir, 'Languages', 'metadata.json');
        if (fs.existsSync(metadataPath)) {
            try {
                metadataOverrides = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
                console.log('Loaded metadata overrides');
            } catch (e) {
                console.warn('Could not load metadata overrides:', e.message);
            }
        }

        // Generate simple HTML report
        const reportHtml = generateSimpleHTML(allMetrics, metadataOverrides);

        // Write report
        const htmlFile = path.join(rootDir, 'benchmark_report.html');
        const reportFile = path.join(rootDir, '_report.html');

        await fs.promises.writeFile(htmlFile, reportHtml);
        await fs.promises.writeFile(reportFile, reportHtml);

        // Write timestamp
        const timestampFile = path.join(rootDir, 'timestamp.js');
        await fs.promises.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()};`);

        console.log(`✅ Successfully generated ${htmlFile}`);
        console.log(`✅ Successfully generated ${reportFile}`);
        console.log(`✅ Report available at http://localhost:9001/`);

        return true;
    } catch (error) {
        console.error('❌ Error generating report:', error);
        throw error;
    }
}

function generateSimpleHTML(metrics, metadataOverrides) {
    const timestamp = new Date().toISOString();

    // Create results table
    const rows = metrics.map(m => {
        const results = m.results || [];
        const matrices = results.map(r => {
            const matrix = r.matrix || 'Unknown';
            const iterations = r.iterations || 'N/A';
            const time = r.time_seconds ? `${r.time_seconds.toFixed(3)}s` : 'N/A';
            const status = r.status || 'Unknown';
            return `
                <tr>
                    <td>${m.solver}</td>
                    <td>${matrix}</td>
                    <td>${iterations}</td>
                    <td>${time}</td>
                    <td>${status}</td>
                </tr>
            `;
        }).join('');

        return matrices || `
            <tr>
                <td>${m.solver}</td>
                <td colspan="4">No results available</td>
            </tr>
        `;
    }).join('');

    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Sudoku Benchmark Report</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
            background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%);
            color: #e2e8f0;
            min-height: 100vh;
            padding: 2rem;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: rgba(30, 41, 59, 0.8);
            border-radius: 16px;
            padding: 2rem;
            box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
        }

        header {
            text-align: center;
            margin-bottom: 3rem;
            padding-bottom: 2rem;
            border-bottom: 2px solid #334155;
        }

        h1 {
            font-size: 3rem;
            background: linear-gradient(135deg, #38bdf8 0%, #818cf8 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            margin-bottom: 0.5rem;
        }

        .subtitle {
            color: #94a3b8;
            font-size: 1.1rem;
        }

        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 1.5rem;
            margin-bottom: 2rem;
        }

        .stat-card {
            background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%);
            padding: 1.5rem;
            border-radius: 12px;
            border: 1px solid #334155;
            text-align: center;
        }

        .stat-label {
            color: #64748b;
            font-size: 0.875rem;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            margin-bottom: 0.5rem;
        }

        .stat-value {
            font-size: 2rem;
            font-weight: bold;
            background: linear-gradient(135deg, #38bdf8 0%, #818cf8 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
        }

        table {
            width: 100%;
            border-collapse: collapse;
            background: rgba(15, 23, 42, 0.6);
            border-radius: 12px;
            overflow: hidden;
        }

        thead {
            background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
        }

        th {
            padding: 1rem;
            text-align: left;
            font-weight: 600;
            color: #38bdf8;
            text-transform: uppercase;
            font-size: 0.875rem;
            letter-spacing: 0.05em;
        }

        td {
            padding: 1rem;
            border-top: 1px solid #334155;
        }

        tbody tr:hover {
            background: rgba(56, 189, 248, 0.05);
        }

        .status-success {
            color: #10b981;
        }

        .status-error {
            color: #ef4444;
        }

        .footer {
            text-align: center;
            margin-top: 3rem;
            padding-top: 2rem;
            border-top: 2px solid #334155;
            color: #64748b;
            font-size: 0.875rem;
        }

        .note {
            background: rgba(56, 189, 248, 0.1);
            border-left: 4px solid #38bdf8;
            padding: 1rem;
            margin: 2rem 0;
            border-radius: 8px;
            color: #94a3b8;
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>🧩 Sudoku Benchmark Report</h1>
            <div class="subtitle">Polyglot Brute-Force Solver Performance Analysis</div>
            <div class="subtitle" style="margin-top: 0.5rem; font-size: 0.9rem;">
                Generated: ${timestamp}
            </div>
        </header>

        <div class="stats-grid">
            <div class="stat-card">
                <div class="stat-label">Languages Tested</div>
                <div class="stat-value">${metrics.length}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Total Runs</div>
                <div class="stat-value">${metrics.reduce((sum, m) => sum + (m.results?.length || 0), 0)}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Baseline</div>
                <div class="stat-value">C</div>
            </div>
        </div>

        <div class="note">
            <strong>Note:</strong> This is a simplified report view. For full interactive visualizations,
            charts, and detailed analysis, the TypeScript report generator provides enhanced features.
            This simplified version shows core benchmark data for quick reference.
        </div>

        <table>
            <thead>
                <tr>
                    <th>Language</th>
                    <th>Matrix</th>
                    <th>Iterations</th>
                    <th>Time</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
                ${rows}
            </tbody>
        </table>

        <div class="footer">
            <p>Polyglot Sudoku Solver Benchmark Framework</p>
            <p style="margin-top: 0.5rem;">
                <a href="/runner" style="color: #38bdf8; text-decoration: none;">Matrix Runner</a> |
                <a href="https://github.com/anthropics/claude-code" style="color: #38bdf8; text-decoration: none;">Built with Claude Code</a>
            </p>
        </div>
    </div>
</body>
</html>`;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
    generateSimpleReport()
        .then(() => process.exit(0))
        .catch(err => {
            console.error(err);
            process.exit(1);
        });
}

export { generateSimpleReport };
