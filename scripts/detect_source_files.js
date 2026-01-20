const fs = require('fs');
const path = require('path');
const { globSync } = require('glob');

const algorithms = ['BruteForce', 'DLX', 'CP'];
const sourceMapping = {};

const validExtensions = [
    '.c', '.cpp', '.cc', '.h', '.hpp',
    '.java', '.kt', '.scala', '.groovy', '.clj',
    '.py', '.rb', '.pl', '.php', '.js', '.ts', '.coffee', '.dart',
    '.go', '.rs', '.swift', '.nim', '.cr', '.zig', '.v', '.vala',
    '.pas', '.adb', '.ads', '.f90', '.f', '.m', '.mm',
    '.lisp', '.scm', '.rkt', '.el', '.fs', '.fsi',
    '.sh', '.bash', '.ksh', '.zsh', '.fish', '.tcsh', '.dash',
    '.rexx', '.awk', '.sed', '.m4', '.ps', '.gp', '.jq', '.xslt',
    '.asm', '.s', '.bas', '.vb', '.st', '.pl', '.red', '.pike',
    '.bf', '.janet', '.wren', '.ipynb'
];

function detectSource(algo, lang) {
    const dir = path.join('Algorithms', algo, lang);
    if (!fs.existsSync(dir)) return null;

    const files = fs.readdirSync(dir);
    
    // 1. Try common prefixes
    const prefixes = ['sudoku.', 'dlx.', 'cp.', 'solver.', 'main.'];
    for (const p of prefixes) {
        const match = files.find(f => {
            const lower = f.toLowerCase();
            if (lower.startsWith(p)) {
                const ext = path.extname(f).toLowerCase();
                return validExtensions.includes(ext) && !lower.includes('test');
            }
            return false;
        });
        if (match) return match;
    }

    // 2. Try algorithm names anywhere in file
    const keywords = [algo.toLowerCase(), 'sudoku'];
    for (const k of keywords) {
        const match = files.find(f => {
            const lower = f.toLowerCase();
            if (lower.includes(k)) {
                const ext = path.extname(f).toLowerCase();
                return validExtensions.includes(ext) && !lower.includes('test') && !lower.includes('readme');
            }
            return false;
        });
        if (match) return match;
    }

    // 3. Fallback to any source file
    const fallback = files.find(f => {
        const ext = path.extname(f).toLowerCase();
        return validExtensions.includes(ext) && !f.toLowerCase().includes('test') && !f.toLowerCase().includes('readme');
    });

    return fallback || null;
}

algorithms.forEach(algo => {
    sourceMapping[algo] = {};
    const algoDir = path.join('Algorithms', algo);
    if (!fs.existsSync(algoDir)) return;

    const langs = fs.readdirSync(algoDir).filter(f => fs.statSync(path.join(algoDir, f)).isDirectory());
    
    langs.forEach(lang => {
        if (lang === 'Media') return;
        const source = detectSource(algo, lang);
        if (source) {
            sourceMapping[algo][lang] = source;
        }
    });
});

console.log(JSON.stringify(sourceMapping, null, 2));

// Save to config
const configPath = 'benchmark_config.json';
let config = {};
if (fs.existsSync(configPath)) {
    config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
}

config.source_files = sourceMapping;
fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
console.log('\nSaved source mapping to benchmark_config.json');
