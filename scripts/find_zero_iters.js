const fs = require('fs');
const { globSync } = require('glob');

const files = globSync('Algorithms/*/*/metrics.json');
let zeroIters = [];

files.forEach(f => {
    try {
        const data = JSON.parse(fs.readFileSync(f, 'utf8'));
        data.forEach(run => {
            if (run.results) {
                run.results.forEach(r => {
                    if (r.status === 'success' && r.iterations === 0) {
                        zeroIters.push({ file: f, solver: run.solver, matrix: r.matrix });
                    }
                });
            }
        });
    } catch(e) {}
});

console.log(JSON.stringify(zeroIters, null, 2));
