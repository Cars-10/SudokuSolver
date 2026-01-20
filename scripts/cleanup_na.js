const fs = require('fs');
const path = require('path');
const { globSync } = require('glob');

function clean() {
    // Find all metrics.json files
    // Patterns: Algorithms/*/*/metrics.json and Languages/*/metrics.json
    const patterns = ['Algorithms/*/*/metrics.json', 'Languages/*/metrics.json'];
    let files = [];
    for (const p of patterns) {
        files = files.concat(globSync(p));
    }
    
    let totalRemoved = 0;
    let filesChanged = 0;

    for (const file of files) {
        try {
            const content = fs.readFileSync(file, 'utf8');
            let data;
            try {
                data = JSON.parse(content);
            } catch (e) {
                console.warn(`Skipping invalid JSON in ${file}`);
                continue;
            }

            if (!Array.isArray(data)) {
                continue;
            }

            let modified = false;

            // Filter out runs or results
            const newData = data.map(run => {
                if (run.results && Array.isArray(run.results)) {
                    const originalLength = run.results.length;
                    const newResults = run.results.filter(r => r.matrix !== "N/A");
                    if (newResults.length !== originalLength) {
                        modified = true;
                        totalRemoved += (originalLength - newResults.length);
                        return { ...run, results: newResults };
                    }
                }
                return run;
            });

            if (modified) {
                fs.writeFileSync(file, JSON.stringify(newData, null, 2));
                console.log(`Cleaned ${file}`);
                filesChanged++;
            }
        } catch (e) {
            console.error(`Error processing ${file}:`, e);
        }
    }
    console.log(`Cleanup complete. Modified ${filesChanged} files, removed ${totalRemoved} N/A entries.`);
}

clean();
