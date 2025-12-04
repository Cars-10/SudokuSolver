import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';
import {
    generateHtml,
    personalities,
    languageMetadata,
    methodologyTexts
} from './gather_metrics.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '..');
const metricsFile = path.join(rootDir, 'metrics.json');
const htmlFile = path.join(rootDir, 'benchmark_report.html');

async function run() {
    try {
        const data = await fs.readFile(metricsFile, 'utf-8');
        const allMetrics = JSON.parse(data);
        console.log(`Loaded ${allMetrics.length} metrics.`);

        // Read allowed matrices from TypeScript metrics
        const typeScriptMetricsFile = path.join(rootDir, 'Manual', 'TypeScript', 'metrics.json');
        let allowedMatrices: string[] = [];
        try {
            const tsData = await fs.readFile(typeScriptMetricsFile, 'utf-8');
            const tsMetrics = JSON.parse(tsData);
            allowedMatrices = [...new Set(tsMetrics.map((m: any) => m.matrix))] as string[];
            console.log(`Filtering report to ${allowedMatrices.length} matrices from TypeScript metrics.`);
        } catch (e) {
            console.warn("Could not read Manual/TypeScript/metrics.json, defaulting to all.", e);
        }

        const htmlContent = await generateHtml(allMetrics, [], personalities, languageMetadata, methodologyTexts, {}, allowedMatrices);
        await fs.writeFile(htmlFile, htmlContent);

        // Write timestamp file for smart refresh
        const timestampFile = path.join(rootDir, 'timestamp.js');
        await fs.writeFile(timestampFile, `window.latestTimestamp = ${Date.now()};`);

        console.log(`Successfully generated ${htmlFile}`);
    } catch (e) {
        console.error("Error:", e);
    }
}

run();
