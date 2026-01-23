const fs = require('fs');
const content = fs.readFileSync('index.html', 'utf-8');

const bfMatches = (content.match(/>BF<\/span>/g) || []).length;
const dlxMatches = (content.match(/>DLX<\/span>/g) || []).length;
const cpMatches = (content.match(/>CP<\/span>/g) || []).length;

console.log(`BF Badges: ${bfMatches}`);
console.log(`DLX Badges: ${dlxMatches}`);
console.log(`CP Badges: ${cpMatches}`);

// Extract Ada rows
const adaRows = content.match(/<tr[^>]*data-lang="Ada"[^>]*>[\s\S]*?<\/tr>/g);
console.log(`Ada Rows found: ${adaRows ? adaRows.length : 0}`);
if (adaRows) {
    adaRows.forEach((row, i) => {
        const hasBF = row.includes('>BF</span>');
        const hasDLX = row.includes('>DLX</span>');
        const hasCP = row.includes('>CP</span>');
        const algoTypeMatch = row.match(/data-algorithm-type="([^"]*)"/);
        const algoType = algoTypeMatch ? algoTypeMatch[1] : 'UNKNOWN';

        console.log(`Ada Row ${i}: Algo=${algoType}, HasBF=${hasBF}, HasDLX=${hasDLX}, HasCP=${hasCP}`);
    });
}

// Extract Python rows
const pythonRows = content.match(/<tr[^>]*data-lang="Python"[^>]*>[\s\S]*?<\/tr>/g);
console.log(`Python Rows found: ${pythonRows ? pythonRows.length : 0}`);
if (pythonRows) {
    pythonRows.forEach((row, i) => {
        const hasBF = row.includes('>BF</span>');
        const hasDLX = row.includes('>DLX</span>');
        const hasCP = row.includes('>CP</span>');
        const algoTypeMatch = row.match(/data-algorithm-type="([^"]*)"/);
        const algoType = algoTypeMatch ? algoTypeMatch[1] : 'UNKNOWN';

        console.log(`Python Row ${i}: Algo=${algoType}, HasBF=${hasBF}, HasDLX=${hasDLX}, HasCP=${hasCP}`);
    });
}

// Extract Fennel rows
const fennelRows = content.match(/<tr[^>]*data-lang="Fennel"[^>]*>[\s\S]*?<\/tr>/g);
console.log(`Fennel Rows found: ${fennelRows ? fennelRows.length : 0}`);
if (fennelRows) {
    fennelRows.forEach((row, i) => {
        const hasBF = row.includes('>BF</span>');
        const hasDLX = row.includes('>DLX</span>');
        const hasCP = row.includes('>CP</span>');
        const algoTypeMatch = row.match(/data-algorithm-type="([^"]*)"/);
        const algoType = algoTypeMatch ? algoTypeMatch[1] : 'UNKNOWN';

        console.log(`Fennel Row ${i}: Algo=${algoType}, HasBF=${hasBF}, HasDLX=${hasDLX}, HasCP=${hasCP}`);
    });
}
