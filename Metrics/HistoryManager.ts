import * as fs from 'fs/promises';
import * as fsSync from 'fs';
import * as path from 'path';
import type { SolverMetrics } from './types.ts';

/**
 * HistoryManager - Manages benchmark history storage
 *
 * Supports both SQLite (primary) and JSON (fallback) storage.
 * The server uses SQLite directly via server/database.js.
 * This class is used by report generation and can read from either source.
 */
export class HistoryManager {
    private historyFile: string;
    private dbFile: string;
    private rootDir: string;

    constructor(rootDir: string) {
        this.rootDir = rootDir;
        this.historyFile = path.join(rootDir, 'benchmark_history.json');
        this.dbFile = path.join(rootDir, 'benchmark_history.db');
    }

    /**
     * Append a record to history (JSON fallback - server uses SQLite directly)
     */
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
        await fs.writeFile(this.historyFile, JSON.stringify(history, null, 2));
    }

    /**
     * Get history - tries SQLite first, falls back to JSON
     */
    async getHistory(): Promise<SolverMetrics[]> {
        // Try SQLite first if database exists
        if (fsSync.existsSync(this.dbFile)) {
            try {
                const history = await this.getHistoryFromSqlite();
                if (history.length > 0) {
                    return history;
                }
            } catch (e) {
                console.warn('SQLite read failed, falling back to JSON:', (e as Error).message);
            }
        }

        // Fallback to JSON
        return this.getHistoryFromJson();
    }

    /**
     * Get history from JSON file
     */
    private async getHistoryFromJson(): Promise<SolverMetrics[]> {
        try {
            const data = await fs.readFile(this.historyFile, 'utf-8');
            return JSON.parse(data);
        } catch (e) {
            return [];
        }
    }

    /**
     * Get history from SQLite database
     */
    private async getHistoryFromSqlite(): Promise<SolverMetrics[]> {
        // Dynamic import to avoid issues if better-sqlite3 isn't installed
        const Database = require('better-sqlite3');
        const db = new Database(this.dbFile, { readonly: true });

        try {
            const rows = db.prepare(`
                SELECT
                    r.id, r.solver, r.run_type as runType, r.variant, r.timestamp,
                    m.matrix, m.time, m.iterations, m.memory,
                    m.cpu_user, m.cpu_sys, m.status
                FROM benchmark_runs r
                LEFT JOIN matrix_results m ON r.id = m.run_id
                ORDER BY r.timestamp DESC, r.id DESC, m.matrix ASC
            `).all();

            // Group rows by run ID
            const runsMap = new Map<number, SolverMetrics>();
            for (const row of rows as any[]) {
                if (!runsMap.has(row.id)) {
                    runsMap.set(row.id, {
                        solver: row.solver,
                        runType: row.runType,
                        timestamp: row.timestamp,
                        results: []
                    });
                }

                if (row.matrix) {
                    runsMap.get(row.id)!.results.push({
                        matrix: row.matrix,
                        time: row.time,
                        iterations: row.iterations,
                        memory: row.memory,
                        cpu_user: row.cpu_user,
                        cpu_sys: row.cpu_sys,
                        status: row.status
                    });
                }
            }

            return Array.from(runsMap.values());
        } finally {
            db.close();
        }
    }
}
