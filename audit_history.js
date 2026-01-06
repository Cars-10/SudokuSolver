const fs = require('fs');
const path = require('path');

const HISTORY_FILE = 'benchmark_history.json';
const REFERENCE_ITERATIONS = {
    "1": 656,
    "1.matrix": 656,
    "2": 439269,
    "2.matrix": 439269,
    "3": 98847,
    "3.matrix": 98847,
    "4": 9085,
    "4.matrix": 9085,
    "5": 445778,
    "5.matrix": 445778,
    "6": 622577597,
    "6.matrix": 622577597
};

function audit() {
    if (!fs.existsSync(HISTORY_FILE)) {
        console.error("History file not found");
        return;
    }

    const history = JSON.parse(fs.readFileSync(HISTORY_FILE, 'utf8'));
    let changed = false;
    let stats = {
        total: history.length,
        predictions: 0,
        mismatches: 0,
        fixed: 0
    };

    const auditedHistory = history.map(entry => {
        if (entry.runType === 'Prediction') {
            stats.predictions++;
            // We keep predictions for now but maybe we should flag them?
            return entry;
        }

        let entryMismatched = false;
        const auditedResults = entry.results.map(res => {
            const expected = REFERENCE_ITERATIONS[res.matrix];
            if (expected !== undefined && res.iterations !== expected) {
                stats.mismatches++;
                entryMismatched = true;
                // If it's a known solver and iteration count is just slightly off? 
                // No, SS requires EXACT match.
                return { ...res, status: 'mismatch', expectedIterations: expected };
            }
            return res;
        });

        if (entryMismatched) {
            changed = true;
            return { ...entry, results: auditedResults, status: 'mismatch' };
        }
        return entry;
    });

    console.log("Audit Stats:");
    console.log(`- Total Records: ${stats.total}`);
    console.log(`- Prediction Records: ${stats.predictions}`);
    console.log(`- Iteration Mismatches Found: ${stats.mismatches}`);

    if (changed) {
        // fs.writeFileSync('benchmark_history_audited.json', JSON.stringify(auditedHistory, null, 2));
        // console.log("Audited history written to benchmark_history_audited.json");
    }
}

audit();
