
const fs = require('fs');
const path = require('path');

const modules = [
    'globals.js',
    'utils.js',
    'ui-persistence.js',
    'personality.js',
    'modal-operations.js',
    'table-operations.js',
    'screensaver.js',
    'd3-chart.js', // Added D3 Chart module
];

const entry = 'report_client_modular.js';
const output = 'report_client.js';

let content = '';

const modulePath = path.join(__dirname, 'modules');

// Helper to strip imports/exports
function processFile(filename, isEntry = false) {
    try {
        let code = fs.readFileSync(path.join(isEntry ? __dirname : modulePath, filename), 'utf8');

        // Remove imports
        // Remove imports (handling multi-line)
        code = code.replace(/import\s+(?:\{[\s\S]*?\}|[\w\s,]*)\s+from\s+['"].*?['"];?/gm, '');

        // Remove export keywords (but keep the definitions)
        code = code.replace(/^export\s+/gm, '');

        return `// --- ${filename} ---\n${code}\n\n`;
    } catch (e) {
        console.error(`Error processing ${filename}:`, e.message);
        return `// ERROR loading ${filename}\n`;
    }
}

console.log('Building report_client.js...');

modules.forEach(m => {
    content += processFile(m);
});

content += processFile(entry, true);

fs.writeFileSync(path.join(__dirname, output), content);
console.log(`Success! Wrote to ${output}`);
