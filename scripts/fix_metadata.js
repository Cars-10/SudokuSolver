const fs = require('fs');
const path = require('path');

const languagesDir = path.join(__dirname, '../Languages');
const metadataPath = path.join(languagesDir, 'metadata.json');

let metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
const languageMetadata = metadata.languageMetadata;

// Get all directories
const entries = fs.readdirSync(languagesDir, { withFileTypes: true });
const languages = entries
    .filter(dirent => dirent.isDirectory() && !dirent.name.startsWith('.') && !['Media', 'node_modules'].includes(dirent.name))
    .map(dirent => dirent.name);

languages.forEach(lang => {
    if (!languageMetadata[lang]) {
        languageMetadata[lang] = {};
    }
    const entry = languageMetadata[lang];

    // Ensure fields exist
    if (!entry.history) entry.history = "";
    if (!entry.paradigm) entry.paradigm = "";
    if (!entry.typeSystem) entry.typeSystem = "";
    if (!entry.description) entry.description = "";

    // Fix long history
    if (entry.history.length > 50) {
        // Append to description if not already contained
        if (!entry.description.includes(entry.history)) {
             entry.description = (entry.description + "\n\n" + entry.history).trim();
        }
        // Clear history for manual filling
        entry.history = "";
    }
});

fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, 4));
console.log("Metadata schema standardized.");
