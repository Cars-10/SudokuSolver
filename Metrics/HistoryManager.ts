import * as fs from 'fs/promises';
import * as path from 'path';
import type { SolverMetrics } from './types.ts';

export class HistoryManager {
    private historyFile: string;

    constructor(rootDir: string) {
        this.historyFile = path.join(rootDir, 'benchmark_history.json');
    }

    async appendRecord(record: SolverMetrics) {
        let history: SolverMetrics[] = [];
        try {
            const data = await fs.readFile(this.historyFile, 'utf-8');
            history = JSON.parse(data);
        } catch (e) {
            // File might not exist or be empty, start fresh
            history = [];
        }

        history.push(record);

        // Optional: Prune history if it gets too large?
        // For now, keep all.

        await fs.writeFile(this.historyFile, JSON.stringify(history, null, 2));
    }

    async getHistory(): Promise<SolverMetrics[]> {
        try {
            const data = await fs.readFile(this.historyFile, 'utf-8');
            return JSON.parse(data);
        } catch (e) {
            return [];
        }
    }
}
