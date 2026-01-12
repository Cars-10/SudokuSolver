const fs = require('fs');
const path = require('path');

const targetFile = path.join(__dirname, 'LanguagesMetadata.ts');
const newHistories = require('./new_histories.cjs');

let content = fs.readFileSync(targetFile, 'utf8');

// Construct the new object string
let newHistoriesStr = 'export const languageHistories: Record<string, string> = {\n';
const entries = Object.entries(newHistories);
entries.forEach(([key, value], index) => {
    // Escape backticks or quotes if needed?
    // Using JSON.stringify ensures safe string representation, then we remove the outer curlies?
    // No, simple key: "value" format.
    // Use JSON.stringify for the value to handle newlines/quotes safely.
    newHistoriesStr += `    \"${key}\": ${JSON.stringify(value)}`;
    if (index < entries.length - 1) newHistoriesStr += ',';
    newHistoriesStr += '\n';
});
newHistoriesStr += '};';

// Regex to replace the existing block
// Matches "export const languageHistories... = { ... }"
const regex = /export const languageHistories: Record<string, string> = \{[\s\S]*?\};/m;

if (regex.test(content)) {
    const newContent = content.replace(regex, newHistoriesStr);
    fs.writeFileSync(targetFile, newContent);
    console.log(`Successfully updated ${targetFile}`);
} else {
    console.error("Could not find languageHistories block in target file.");
    // Fallback: Append it? No, that would duplicate.
    process.exit(1);
}
