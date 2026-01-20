const fs = require('fs');
const path = require('path');
const { globSync } = require('glob');

const baselines = {};

// Load Baselines
['BruteForce', 'DLX', 'CP'].forEach(algo => {
    try {
        const p = `Algorithms/${algo}/C/metrics.json`;
        if (fs.existsSync(p)) {
            const data = JSON.parse(fs.readFileSync(p, 'utf8'));
            if (data[0] && data[0].results) {
                baselines[algo] = {};
                data[0].results.forEach(r => {
                    baselines[algo][r.matrix] = r.iterations;
                });
            }
        }
    } catch(e) {
        console.warn(`No baseline for ${algo}`);
    }
});

const files = globSync('Algorithms/*/*/metrics.json');
const mismatches = [];

files.forEach(f => {
    try {
        const parts = f.split(path.sep);
        const algo = parts[1]; // Algorithms/Algo/Lang/...
        const lang = parts[2];
        
        if (lang === 'C') return; // Skip baseline itself

        const data = JSON.parse(fs.readFileSync(f, 'utf8'));
        const run = data[0]; // Assuming latest run is first or wrapper
        // Handle array of runs - take last
        const latestRun = Array.isArray(data) ? data[data.length-1] : data;
        
        if (latestRun && latestRun.results) {
            latestRun.results.forEach(r => {
                if (r.status === 'success') {
                    const expected = baselines[algo] ? baselines[algo][r.matrix] : null;
                    if (expected !== undefined && expected !== null) {
                        if (r.iterations !== expected) {
                            mismatches.push({
                                solver: lang,
                                algorithm: algo,
                                matrix: r.matrix,
                                actual: r.iterations,
                                expected: expected,
                                diff: r.iterations - expected
                            });
                        }
                    }
                }
            });
        }
    } catch(e) {}
});

// Group by Solver
const grouped = {};
mismatches.forEach(m => {
    const key = `${m.algorithm}/${m.solver}`;
    if (!grouped[key]) grouped[key] = [];
    grouped[key].push(m);
});

console.log("=== Mismatch Report ===");
Object.keys(grouped).sort().forEach(k => {
    console.log(`\n${k}:`);
    grouped[k].sort((a,b) => parseInt(a.matrix) - parseInt(b.matrix)).forEach(m => {
        console.log(`  Matrix ${m.matrix}: ${m.actual} (Expected: ${m.expected})`);
    });
});
