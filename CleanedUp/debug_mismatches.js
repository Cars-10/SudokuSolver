const fs = require('fs');
const path = require('path');

async function check() {
    const root = '/Users/cars10/GIT/SudokuSolver/CleanedUp/Languages';

    // Manual directory list
    let files = [];
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
            } else {
                // Array of runs? Take last
                const last = data[data.length - 1];
                results = last.results || last;
            }
        } else {
            results = data.results || [];
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

    const cTotal = cMetrics.results.reduce((a, b) => a + b.iterations, 0);
    console.log(`C Total Iters: ${cTotal}`);
    cMetrics.results.forEach(r => console.log(`  Matrix ${r.matrix}: ${r.iterations}`));

    console.log("\n--- Checking Mismatches ---");
    let headerCount = 0;

    for (const m of allMetrics) {
        if (m.solver === 'C') continue;

        const total = m.results.reduce((a, b) => a + (b.iterations || 0), 0);
        const isHeaderMismatch = total !== cTotal;

        let tableMismatchCount = 0;
        let diffDetails = [];

        // Check per matrix
        const matrices = ["1.matrix", "2.matrix", "3.matrix", "4.matrix", "5.matrix", "6.matrix"];
        for (const mat of matrices) {
            const r = m.results.find(res => res.matrix === mat);
            const c = cMetrics.results.find(res => res.matrix === mat);

            if (!r) {
                // Missing result (Timeout?)
                // Table says "-"
                diffDetails.push(`${mat}: MISSING`);
            } else if (c && r.iterations !== c.iterations) {
                tableMismatchCount++;
                diffDetails.push(`${mat}: ${r.iterations} vs ${c.iterations}`);
            }
        }

        if (isHeaderMismatch) {
            headerCount++;
            console.log(`Solver: ${m.solver}`);
            console.log(`  Header says MISMATCH (Total: ${total} vs ${cTotal})`);
            console.log(`  Table Mismatches: ${tableMismatchCount}`);
            if (diffDetails.length > 0) {
                console.log(`  Details: ${diffDetails.join(', ')}`);
            } else {
                console.log(`  Details: Matches per matrix but sum differs? (Missing matrix?)`);
            }
            console.log('---');
        }
    }

    console.log(`\nFinal Header Count: ${headerCount}`);
}

check();
