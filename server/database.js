/**
 * SQLite Database Module for Benchmark History
 * Replaces benchmark_history.json with structured database storage
 */

const Database = require('better-sqlite3');
const path = require('path');
const fs = require('fs');

const DB_PATH = path.join(__dirname, '..', 'benchmark_history.db');

let db = null;

/**
 * Initialize the database and create tables if they don't exist
 */
function initDatabase() {
    if (db) return db;

    db = new Database(DB_PATH);

    // Enable WAL mode for better concurrent access
    db.pragma('journal_mode = WAL');

    // Create tables
    db.exec(`
        -- Benchmark runs table (one row per benchmark execution)
        CREATE TABLE IF NOT EXISTS benchmark_runs (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            solver TEXT NOT NULL,
            run_type TEXT NOT NULL DEFAULT 'Local',
            variant TEXT DEFAULT 'default',
            timestamp TEXT NOT NULL,
            created_at TEXT DEFAULT CURRENT_TIMESTAMP
        );

        -- Matrix results table (one row per matrix result)
        CREATE TABLE IF NOT EXISTS matrix_results (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            run_id INTEGER NOT NULL,
            matrix TEXT NOT NULL,
            time REAL,
            iterations INTEGER,
            memory REAL,
            cpu_user REAL,
            cpu_sys REAL,
            page_faults_major INTEGER,
            page_faults_minor INTEGER,
            context_switches_voluntary INTEGER,
            context_switches_involuntary INTEGER,
            io_inputs INTEGER,
            io_outputs INTEGER,
            status TEXT,
            output TEXT,
            run_timestamp TEXT,
            FOREIGN KEY (run_id) REFERENCES benchmark_runs(id) ON DELETE CASCADE
        );

        -- Indexes for common queries
        CREATE INDEX IF NOT EXISTS idx_runs_solver ON benchmark_runs(solver);
        CREATE INDEX IF NOT EXISTS idx_runs_timestamp ON benchmark_runs(timestamp);
        CREATE INDEX IF NOT EXISTS idx_runs_solver_variant ON benchmark_runs(solver, variant);
        CREATE INDEX IF NOT EXISTS idx_results_run_id ON matrix_results(run_id);
        CREATE INDEX IF NOT EXISTS idx_results_matrix ON matrix_results(matrix);
    `);

    console.log('Database initialized at:', DB_PATH);
    return db;
}

/**
 * Insert a benchmark run with its matrix results
 * @param {Object} record - The benchmark record
 * @param {string} record.solver - Language/solver name
 * @param {string} record.runType - Type of run (Local, Docker, AI, etc.)
 * @param {string} record.timestamp - ISO timestamp
 * @param {string} [record.variant] - Compiler variant (optional)
 * @param {Array} record.results - Array of matrix results
 * @returns {number} The inserted run ID
 */
function insertBenchmarkRun(record) {
    const database = initDatabase();

    const insertRun = database.prepare(`
        INSERT INTO benchmark_runs (solver, run_type, variant, timestamp)
        VALUES (@solver, @runType, @variant, @timestamp)
    `);

    const insertResult = database.prepare(`
        INSERT INTO matrix_results (
            run_id, matrix, time, iterations, memory,
            cpu_user, cpu_sys, page_faults_major, page_faults_minor,
            context_switches_voluntary, context_switches_involuntary,
            io_inputs, io_outputs, status, output, run_timestamp
        ) VALUES (
            @run_id, @matrix, @time, @iterations, @memory,
            @cpu_user, @cpu_sys, @page_faults_major, @page_faults_minor,
            @context_switches_voluntary, @context_switches_involuntary,
            @io_inputs, @io_outputs, @status, @output, @run_timestamp
        )
    `);

    const transaction = database.transaction((record) => {
        const runInfo = insertRun.run({
            solver: record.solver,
            runType: record.runType || 'Local',
            variant: record.variant || 'default',
            timestamp: record.timestamp
        });

        const runId = runInfo.lastInsertRowid;

        if (record.results && Array.isArray(record.results)) {
            for (const result of record.results) {
                insertResult.run({
                    run_id: runId,
                    matrix: String(result.matrix).replace('.matrix', ''),
                    time: result.time || null,
                    iterations: result.iterations || null,
                    memory: result.memory || null,
                    cpu_user: result.cpu_user || null,
                    cpu_sys: result.cpu_sys || null,
                    page_faults_major: result.page_faults_major || null,
                    page_faults_minor: result.page_faults_minor || null,
                    context_switches_voluntary: result.context_switches_voluntary || null,
                    context_switches_involuntary: result.context_switches_involuntary || null,
                    io_inputs: result.io_inputs || null,
                    io_outputs: result.io_outputs || null,
                    status: result.status || null,
                    output: result.output || null,
                    run_timestamp: result.run_timestamp || record.timestamp
                });
            }
        }

        return runId;
    });

    return transaction(record);
}

/**
 * Get all benchmark history (for backwards compatibility with JSON format)
 * @param {Object} options - Query options
 * @param {number} [options.limit] - Maximum number of runs to return
 * @param {string} [options.solver] - Filter by solver name
 * @param {string} [options.since] - Only runs after this timestamp
 * @returns {Array} Array of benchmark records in JSON-compatible format
 */
function getHistory(options = {}) {
    const database = initDatabase();

    let query = `
        SELECT
            r.id, r.solver, r.run_type as runType, r.variant, r.timestamp,
            m.matrix, m.time, m.iterations, m.memory,
            m.cpu_user, m.cpu_sys, m.page_faults_major, m.page_faults_minor,
            m.context_switches_voluntary, m.context_switches_involuntary,
            m.io_inputs, m.io_outputs, m.status, m.output, m.run_timestamp
        FROM benchmark_runs r
        LEFT JOIN matrix_results m ON r.id = m.run_id
        WHERE 1=1
    `;

    const params = {};

    if (options.solver) {
        query += ' AND r.solver = @solver';
        params.solver = options.solver;
    }

    if (options.since) {
        query += ' AND r.timestamp >= @since';
        params.since = options.since;
    }

    query += ' ORDER BY r.timestamp DESC, r.id DESC, m.matrix ASC';

    if (options.limit) {
        // Limit by unique runs, not total rows
        query = `
            WITH limited_runs AS (
                SELECT DISTINCT id FROM benchmark_runs
                WHERE 1=1
                ${options.solver ? ' AND solver = @solver' : ''}
                ${options.since ? ' AND timestamp >= @since' : ''}
                ORDER BY timestamp DESC
                LIMIT @limit
            )
            SELECT
                r.id, r.solver, r.run_type as runType, r.variant, r.timestamp,
                m.matrix, m.time, m.iterations, m.memory,
                m.cpu_user, m.cpu_sys, m.page_faults_major, m.page_faults_minor,
                m.context_switches_voluntary, m.context_switches_involuntary,
                m.io_inputs, m.io_outputs, m.status, m.output, m.run_timestamp
            FROM benchmark_runs r
            INNER JOIN limited_runs lr ON r.id = lr.id
            LEFT JOIN matrix_results m ON r.id = m.run_id
            ORDER BY r.timestamp DESC, r.id DESC, m.matrix ASC
        `;
        params.limit = options.limit;
    }

    const rows = database.prepare(query).all(params);

    // Group rows by run ID to reconstruct the JSON structure
    const runsMap = new Map();
    for (const row of rows) {
        if (!runsMap.has(row.id)) {
            runsMap.set(row.id, {
                solver: row.solver,
                runType: row.runType,
                variant: row.variant,
                timestamp: row.timestamp,
                results: []
            });
        }

        if (row.matrix) {
            runsMap.get(row.id).results.push({
                matrix: row.matrix,
                time: row.time,
                iterations: row.iterations,
                memory: row.memory,
                cpu_user: row.cpu_user,
                cpu_sys: row.cpu_sys,
                page_faults_major: row.page_faults_major,
                page_faults_minor: row.page_faults_minor,
                context_switches_voluntary: row.context_switches_voluntary,
                context_switches_involuntary: row.context_switches_involuntary,
                io_inputs: row.io_inputs,
                io_outputs: row.io_outputs,
                status: row.status,
                run_timestamp: row.run_timestamp
            });
        }
    }

    return Array.from(runsMap.values());
}

/**
 * Get history for a specific solver
 * @param {string} solver - Solver name
 * @param {number} [limit=100] - Maximum number of runs
 * @returns {Array} Array of benchmark records
 */
function getHistoryBySolver(solver, limit = 100) {
    return getHistory({ solver, limit });
}

/**
 * Get aggregate statistics for a solver
 * @param {string} solver - Solver name
 * @returns {Object} Statistics object
 */
function getSolverStats(solver) {
    const database = initDatabase();

    const stats = database.prepare(`
        SELECT
            COUNT(DISTINCT r.id) as total_runs,
            COUNT(m.id) as total_results,
            MIN(m.time) as best_time,
            AVG(m.time) as avg_time,
            MAX(m.time) as worst_time,
            MIN(r.timestamp) as first_run,
            MAX(r.timestamp) as last_run
        FROM benchmark_runs r
        LEFT JOIN matrix_results m ON r.id = m.run_id
        WHERE r.solver = ?
    `).get(solver);

    return stats;
}

/**
 * Get recent activity across all solvers
 * @param {number} [hours=24] - Look back this many hours
 * @returns {Array} Recent benchmark records
 */
function getRecentActivity(hours = 24) {
    const since = new Date(Date.now() - hours * 60 * 60 * 1000).toISOString();
    return getHistory({ since });
}

/**
 * Migrate data from benchmark_history.json to SQLite
 * @param {string} jsonPath - Path to the JSON file
 * @returns {Object} Migration result with counts
 */
function migrateFromJson(jsonPath) {
    const database = initDatabase();

    if (!fs.existsSync(jsonPath)) {
        return { success: false, error: 'JSON file not found', runsImported: 0 };
    }

    try {
        const jsonData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));

        if (!Array.isArray(jsonData)) {
            return { success: false, error: 'Invalid JSON format', runsImported: 0 };
        }

        let runsImported = 0;
        let resultsImported = 0;

        for (const record of jsonData) {
            try {
                insertBenchmarkRun(record);
                runsImported++;
                resultsImported += (record.results || []).length;
            } catch (e) {
                console.error('Error importing record:', e.message);
            }
        }

        return {
            success: true,
            runsImported,
            resultsImported,
            message: `Imported ${runsImported} runs with ${resultsImported} results`
        };
    } catch (e) {
        return { success: false, error: e.message, runsImported: 0 };
    }
}

/**
 * Close the database connection
 */
function closeDatabase() {
    if (db) {
        db.close();
        db = null;
    }
}

module.exports = {
    initDatabase,
    insertBenchmarkRun,
    getHistory,
    getHistoryBySolver,
    getSolverStats,
    getRecentActivity,
    migrateFromJson,
    closeDatabase
};
