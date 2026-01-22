// check_invalid.mjs - Find invalid metrics entries
import fs from 'fs';
import { glob } from 'glob';

const files = await glob('./Algorithms/BruteForce/*/metrics.json');
const invalid = [];

for (const file of files) {
    try {
        const data = JSON.parse(fs.readFileSync(file, 'utf8'));

        if (Array.isArray(data)) {
            for (const entry of data) {
                // Check for invalid entries
                if (!entry.results || entry.results.length === 0) {
                    invalid.push({ file, reason: 'No results array' });
                    break;
                }

                for (const r of entry.results) {
                    // Check for N/A matrix
                    if (r.matrix === 'N/A' || r.matrix === null || r.matrix === undefined) {
                        invalid.push({ file, reason: 'Invalid matrix: N/A', status: r.status });
                    }
                    // Check for env_error status
                    if (r.status === 'env_error' || r.status === 'error') {
                        invalid.push({ file, reason: 'Error status: ' + r.status, matrix: r.matrix });
                    }
                }
            }
        }
    } catch (e) {
        invalid.push({ file, reason: 'Parse error: ' + e.message });
    }
}

// Group by reason
const grouped = {};
for (const i of invalid) {
    const key = i.reason;
    if (!grouped[key]) grouped[key] = [];
    grouped[key].push(i.file.replace('./Algorithms/BruteForce/', '').replace('/metrics.json', ''));
}

for (const [reason, files] of Object.entries(grouped)) {
    console.log(reason + ':');
    console.log('  ' + [...new Set(files)].join(', '));
    console.log('');
}

console.log('Total invalid entries:', invalid.length);
