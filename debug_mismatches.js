const fs = require('fs');
const path = require('path');

async function check() {
    const root = path.join(process.cwd(), 'Languages');

    // Manual directory list
    let files = [];
    if (!fs.existsSync(root)) {
        console.error("Languages directory not found at:", root);
        return;
    }
    const langs = fs.readdirSync(root);
    for (const lang of langs) {
        if (lang.startsWith('.')) continue;
        const p = path.join(root, lang, 'metrics.json');
        if (fs.existsSync(p)) {
            files.push(p);
        }
    }

    let cMetrics = null;
    let allMetrics = [];

    // Helper to normalize matrix name
    const normalize = (name) => String(name).replace('.matrix', '');

    // Load all
    for (const f of files) {
        const data = JSON.parse(fs.readFileSync(f, 'utf8'));
        const solver = path.basename(path.dirname(f));

        let results = [];
        if (Array.isArray(data)) {
            // It's likely [ {matrix:1}, {matrix:2} ... ]
            // But verify if it's array of runs or array of results
            if (data.length > 0 && data[0].matrix) {
                results = data;
            } else if (data.length > 0) {
                // Array of runs? Take last
                const last = data[data.length - 1];
                results = last.results || (Array.isArray(last) ? last : []);
            }
        } else {
            results = data.results || [];
        }

        if (!Array.isArray(results)) {
            console.warn(`Warning: Invalid results format for ${solver}`, results);
            results = [];
        }

        const entry = {
            solver: solver,
            results: results
        };

        if (solver === 'C') {
            cMetrics = entry;
        }
        allMetrics.push(entry);
    }

    if (!cMetrics) {
        console.log("No C metrics found!");
        return;
    }

    // Create a map for C metrics for easier lookup
    const cMap = new Map();
    cMetrics.results.forEach(r => cMap.set(normalize(r.matrix), r.iterations));

    const cTotal = cMetrics.results.reduce((a, b) => a + b.iterations, 0);
    console.log(`C Total Iters: ${cTotal}`);
    cMetrics.results.forEach(r => console.log(`  Matrix ${normalize(r.matrix)}: ${r.iterations}`));

    console.log("\n--- Checking Mismatches ---");
    let headerCount = 0;

    for (const m of allMetrics) {
        if (m.solver === 'C') continue;

        let tableMismatchCount = 0;
        let diffDetails = [];
        let totalIters = 0;

        // Check per matrix based on what the solver actually ran
        for (const r of m.results) {
            const matName = normalize(r.matrix);
            const expected = cMap.get(matName);
            const actual = r.iterations;
            totalIters += actual;

            if (expected !== undefined) {
                if (actual !== expected) {
                    tableMismatchCount++;
                    diffDetails.push(`${matName}: ${actual} vs ${expected}`);
                }
            } else {
                // Matrix not in C baseline?
                // diffDetails.push(`${matName}: Unknown baseline`);
            }
        }

        if (tableMismatchCount > 0) {
            headerCount++;
            console.log(`Solver: ${m.solver}`);
            console.log(`  Table Mismatches: ${tableMismatchCount}`);
            console.log(`  Details: ${diffDetails.join(', ')}`);
            console.log('---');
        }
    }

    console.log(`\nFinal Header Count: ${headerCount}`);
}

check();
