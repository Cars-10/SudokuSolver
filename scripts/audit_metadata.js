const fs = require('fs');
const path = require('path');

const languagesDir = path.join(__dirname, '../Languages');
const metadataPath = path.join(languagesDir, 'metadata.json');

// Read metadata
const metadataRaw = fs.readFileSync(metadataPath, 'utf8');
const metadata = JSON.parse(metadataRaw).languageMetadata;

// Get language directories
const entries = fs.readdirSync(languagesDir, { withFileTypes: true });
const languages = entries
    .filter(dirent => dirent.isDirectory() && !dirent.name.startsWith('.') && !['Media', 'node_modules'].includes(dirent.name))
    .map(dirent => dirent.name);

const report = {
    missingInJson: [],
    missingFields: {},
    longHistory: []
};

languages.forEach(lang => {
    if (!metadata[lang]) {
        report.missingInJson.push(lang);
    } else {
        const data = metadata[lang];
        const missing = [];
        if (!data.history) missing.push('history');
        if (!data.paradigm) missing.push('paradigm');
        if (!data.typeSystem) missing.push('typeSystem');
        // description is usually present, but good to check
        if (!data.description) missing.push('description');

        if (missing.length > 0) {
            report.missingFields[lang] = missing;
        }

        if (data.history && data.history.length > 50) {
            report.longHistory.push({ lang, length: data.history.length, text: data.history });
        }
    }
});

// Output report
console.log('# Metadata Audit Report\n');

if (report.missingInJson.length > 0) {
    console.log('## Missing in JSON');
    report.missingInJson.forEach(l => console.log(`- ${l}`));
    console.log('');
}

if (Object.keys(report.missingFields).length > 0) {
    console.log('## Missing Fields');
    Object.keys(report.missingFields).forEach(l => {
        console.log(`- **${l}**: ${report.missingFields[l].join(', ')}`);
    });
    console.log('');
}

if (report.longHistory.length > 0) {
    console.log('## Long History (>50 chars)');
    report.longHistory.forEach(item => {
        console.log(`- **${item.lang}** (${item.length} chars): ${item.text}`);
    });
    console.log('');
}

if (report.missingInJson.length === 0 && Object.keys(report.missingFields).length === 0 && report.longHistory.length === 0) {
    console.log('All checks passed!');
}
