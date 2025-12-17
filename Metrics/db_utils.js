/**
 * Database Utility Functions
 * Provides helpers for inserting/querying benchmark data
 */

const Database = require('better-sqlite3');
const path = require('path');

const DB_PATH = path.join(__dirname, 'benchmarks.db');

/**
 * Get database connection
 * @returns {Database} SQLite database instance
 */
function getDatabase() {
    const db = new Database(DB_PATH);
    db.pragma('foreign_keys = ON');
    return db;
}

/**
 * Insert a benchmark run into the database
 * @param {Object} run - Run data object
 * @returns {Object} Insert result with lastInsertRowid
 */
function insertRun(run) {
    const db = getDatabase();

    const stmt = db.prepare(`
        INSERT INTO runs (
            timestamp, language, matrix, iterations, time_seconds,
            memory_kb, cpu_user, cpu_sys, status, output,
            compiler_variant, toolchain_version
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `);

    const result = stmt.run(
        run.timestamp,
        run.language,
        run.matrix,
        run.iterations || null,
        run.time_seconds || null,
        run.memory_kb || null,
        run.cpu_user || null,
        run.cpu_sys || null,
        run.status,
        run.output || null,
        run.compiler_variant || 'default',
        run.toolchain_version || null
    );

    db.close();
    return result;
}

/**
 * Insert multiple runs in a transaction
 * @param {Array} runs - Array of run objects
 * @returns {Number} Number of rows inserted
 */
function insertRuns(runs) {
    const db = getDatabase();

    const stmt = db.prepare(`
        INSERT INTO runs (
            timestamp, language, matrix, iterations, time_seconds,
            memory_kb, cpu_user, cpu_sys, status, output,
            compiler_variant, toolchain_version
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `);

    const insertMany = db.transaction((runs) => {
        for (const run of runs) {
            stmt.run(
                run.timestamp,
                run.language,
                run.matrix,
                run.iterations || null,
                run.time_seconds || null,
                run.memory_kb || null,
                run.cpu_user || null,
                run.cpu_sys || null,
                run.status,
                run.output || null,
                run.compiler_variant || 'default',
                run.toolchain_version || null
            );
        }
    });

    insertMany(runs);
    db.close();
    return runs.length;
}

/**
 * Query runs with filters
 * @param {Object} filters - Filter options (language, matrix, status, limit)
 * @returns {Array} Array of run objects
 */
function queryRuns(filters = {}) {
    const db = getDatabase();

    let query = 'SELECT * FROM runs WHERE 1=1';
    const params = [];

    if (filters.language) {
        query += ' AND language = ?';
        params.push(filters.language);
    }

    if (filters.matrix) {
        query += ' AND matrix = ?';
        params.push(filters.matrix);
    }

    if (filters.status) {
        query += ' AND status = ?';
        params.push(filters.status);
    }

    if (filters.compiler_variant) {
        query += ' AND compiler_variant = ?';
        params.push(filters.compiler_variant);
    }

    query += ' ORDER BY timestamp DESC';

    if (filters.limit) {
        query += ' LIMIT ?';
        params.push(filters.limit);
    }

    const stmt = db.prepare(query);
    const results = stmt.all(...params);
    db.close();

    return results;
}

/**
 * Get latest run for each language/matrix combination
 * @returns {Array} Array of latest run objects
 */
function getLatestRuns() {
    const db = getDatabase();
    const results = db.prepare('SELECT * FROM latest_runs').all();
    db.close();
    return results;
}

/**
 * Insert a validation result
 * @param {Object} validation - Validation data
 * @returns {Object} Insert result
 */
function insertValidation(validation) {
    const db = getDatabase();

    const stmt = db.prepare(`
        INSERT INTO validations (
            language, matrix, expected_iterations, actual_iterations,
            valid, error_message, format_valid, format_error
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    `);

    const result = stmt.run(
        validation.language,
        validation.matrix,
        validation.expected,
        validation.actual,
        validation.valid ? 1 : 0,
        validation.error || null,
        validation.format_valid !== undefined ? (validation.format_valid ? 1 : 0) : null,
        validation.format_error || null
    );

    db.close();
    return result;
}

/**
 * Get validation summary by language
 * @returns {Array} Validation summary with pass rates
 */
function getValidationSummary() {
    const db = getDatabase();
    const results = db.prepare('SELECT * FROM validation_summary').all();
    db.close();
    return results;
}

/**
 * Get performance leaderboard
 * @returns {Array} Performance leaderboard data
 */
function getPerformanceLeaderboard() {
    const db = getDatabase();
    const results = db.prepare('SELECT * FROM performance_leaderboard').all();
    db.close();
    return results;
}

/**
 * Get all validations for a language
 * @param {String} language - Language name
 * @returns {Array} Validation results
 */
function getValidationsForLanguage(language) {
    const db = getDatabase();
    const results = db.prepare(`
        SELECT * FROM validations
        WHERE language = ?
        ORDER BY matrix
    `).all(language);
    db.close();
    return results;
}

/**
 * Check if database exists and is initialized
 * @returns {Boolean} True if database exists with tables
 */
function isDatabaseInitialized() {
    const fs = require('fs');
    const dbExists = fs.existsSync(DB_PATH);

    if (!dbExists) {
        return false;
    }

    try {
        const db = getDatabase();
        const tables = db.prepare(`
            SELECT name FROM sqlite_master
            WHERE type='table' AND name='runs'
        `).all();
        db.close();
        return tables.length > 0;
    } catch (error) {
        return false;
    }
}

module.exports = {
    getDatabase,
    insertRun,
    insertRuns,
    queryRuns,
    getLatestRuns,
    insertValidation,
    getValidationSummary,
    getPerformanceLeaderboard,
    getValidationsForLanguage,
    isDatabaseInitialized,
    DB_PATH
};
