import * as fs from 'fs/promises';
import * as path from 'path';
import type { SolverMetrics } from './types.ts';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function seedHistory() {
    console.log("Seeding benchmark history...");

    const rootDir = path.resolve(__dirname, '..');
    const historyFile = path.join(rootDir, 'benchmark_history.json');

    const languages = [
        { name: 'C', baseTime: 0.05, variance: 0.01 },
        { name: 'Rust', baseTime: 0.06, variance: 0.01 },
        { name: 'Go', baseTime: 0.08, variance: 0.02 },
        { name: 'Java', baseTime: 0.12, variance: 0.03 },
        { name: 'Python', baseTime: 1.5, variance: 0.2 }
    ];

    const history: SolverMetrics[] = [];
    const now = new Date();

    // Create 10 data points for each language over the last 10 days
    for (let i = 9; i >= 0; i--) {
        const date = new Date(now);
        date.setDate(date.getDate() - i);

        for (const lang of languages) {
            // Add some random variance
            const time = lang.baseTime + (Math.random() * lang.variance * 2 - lang.variance);

            history.push({
                solver: lang.name,
                runType: 'Prediction', // Label as prediction/guess
                timestamp: date.toISOString(),
                results: [{
                    matrix: '1.matrix',
                    time: parseFloat(time.toFixed(4)),
                    iterations: 100, // Dummy
                    memory: 1024 * 1024 * (Math.random() * 10 + 1),
                    cpu_user: time * 0.8,
                    cpu_sys: time * 0.2,
                    status: 'pass'
                }]
            });
        }
    }

    try {
        await fs.writeFile(historyFile, JSON.stringify(history, null, 2));
        console.log(`Successfully seeded ${history.length} records to ${historyFile}`);
    } catch (e) {
        console.error("Failed to write history file:", e);
    }
}

seedHistory();
