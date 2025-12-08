
import { languageMetadata, personalities, narratorIntros, methodologyTexts, mismatchLabels, iterationLabels, timeLabels, memoryLabels, scoreLabels } from '../Metrics/LanguagesMetadata';
import * as fs from 'fs';
import * as path from 'path';

const outputDir = path.join(__dirname, '../CleanedUp/Languages');
if (!fs.existsSync(outputDir)) {
    console.log("Creating directory: " + outputDir);
    fs.mkdirSync(outputDir, { recursive: true });
}

const metadata = {
    languageMetadata,
    personalities,
    narratorIntros,
    methodologyTexts,
    mismatchLabels,
    iterationLabels,
    timeLabels,
    memoryLabels,
    scoreLabels
};

const outputPath = path.join(outputDir, 'metadata.json');
fs.writeFileSync(outputPath, JSON.stringify(metadata, null, 4));

console.log(`Metadata migrated to ${outputPath}`);
