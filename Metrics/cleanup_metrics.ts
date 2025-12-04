import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

interface MetricResult {
    matrix: string;
    time: number;
    iterations: number;
    memory: number;
    cpu_user: number;
    cpu_sys: number;
    status: string;
    output?: string;
}

interface SolverMetrics {
    solver: string;
    timestamp: string;
    results: MetricResult[];
}

async function cleanupMetrics() {
    const rootDir = path.resolve(__dirname, '..');
    const metricsFile = path.join(rootDir, 'metrics.json');

    try {
        const data = await fs.readFile(metricsFile, 'utf-8');
        let metrics: SolverMetrics[] = JSON.parse(data);
        console.log(`Loaded ${metrics.length} metrics.`);

        const originalCount = metrics.length;
        const solversMap = new Map<string, SolverMetrics>();

        // First pass: Index all solvers
        for (const m of metrics) {
            solversMap.set(m.solver, m);
        }

        const toRemove = new Set<string>();
        const toRename = new Map<string, string>();

        for (const m of metrics) {
            // Check if it's a legacy name (no parenthesis)
            if (!m.solver.includes('(')) {
                const manualName = `${m.solver} (Manual)`;
                const aiName = `${m.solver} (AI)`;

                if (solversMap.has(manualName)) {
                    console.log(`Found duplicate: "${m.solver}" and "${manualName}". Marking "${m.solver}" for removal.`);
                    toRemove.add(m.solver);
                } else if (solversMap.has(aiName)) {
                    console.log(`Found duplicate: "${m.solver}" and "${aiName}". Marking "${m.solver}" for removal.`);
                    toRemove.add(m.solver);
                } else {
                    console.log(`Found legacy name: "${m.solver}". Marking for rename to "${manualName}".`);
                    toRename.set(m.solver, manualName);
                }
            }
        }

        // Apply removals
        metrics = metrics.filter(m => !toRemove.has(m.solver));

        // Apply renames
        for (const m of metrics) {
            if (toRename.has(m.solver)) {
                m.solver = toRename.get(m.solver)!;
            }
        }

        console.log(`Removed ${toRemove.size} duplicates.`);
        console.log(`Renamed ${toRename.size} legacy entries.`);
        console.log(`Final count: ${metrics.length} (was ${originalCount})`);

        await fs.writeFile(metricsFile, JSON.stringify(metrics, null, 2));
        console.log(`Saved cleaned metrics to ${metricsFile}`);

    } catch (e) {
        console.error("Error cleaning metrics:", e);
    }
}

cleanupMetrics().catch(console.error);
