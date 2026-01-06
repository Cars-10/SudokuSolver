-- Sudoku Benchmark Database Schema
-- SQLite 3.x
-- Created: 2025-12-16

-- ============================================================================
-- RUNS TABLE - Primary metrics storage for all benchmark executions
-- ============================================================================
CREATE TABLE IF NOT EXISTS runs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    -- Execution context
    timestamp TEXT NOT NULL,              -- ISO 8601 format (YYYY-MM-DDTHH:MM:SSZ)
    language TEXT NOT NULL,               -- Language name (C, Python, Go, etc.)
    matrix INTEGER NOT NULL,              -- Matrix number (1-6)
    runType TEXT,                         -- Local, Docker, etc.

    -- Performance metrics
    iterations INTEGER,                   -- Number of iterations (algorithm fingerprint)
    time_ms REAL,                         -- Wall clock time in milliseconds
    memory_bytes INTEGER,                 -- Peak memory usage in bytes
    cpu_user REAL,                        -- User CPU time in milliseconds
    cpu_sys REAL,                         -- System CPU time in milliseconds

    -- Detailed OS Metrics
    page_faults_major INTEGER,
    page_faults_minor INTEGER,
    context_switches_voluntary INTEGER,
    context_switches_involuntary INTEGER,
    io_inputs INTEGER,
    io_outputs INTEGER,

    -- Execution status
    status TEXT NOT NULL,                 -- success, timeout, error, env_error
    output TEXT,                          -- Full solver output (for format validation)

    -- Toolchain information
    compiler_variant TEXT,                -- Optimization flag (O0, O2, O3, Ofast) or interpreter version
    toolchain_version TEXT,               -- gcc 13.2.0, python 3.12.1, etc.

    -- Metadata
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CHECK (matrix >= 0 AND matrix <= 6),
    CHECK (status IN ('success', 'timeout', 'error', 'env_error'))
);

-- Performance indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_runs_timestamp ON runs(timestamp);
CREATE INDEX IF NOT EXISTS idx_runs_language ON runs(language);
CREATE INDEX IF NOT EXISTS idx_runs_matrix ON runs(matrix);
CREATE INDEX IF NOT EXISTS idx_runs_status ON runs(status);
CREATE INDEX IF NOT EXISTS idx_runs_language_matrix ON runs(language, matrix);
CREATE INDEX IF NOT EXISTS idx_runs_language_timestamp ON runs(language, timestamp);

-- ============================================================================
-- VALIDATIONS TABLE - Iteration count and format validation results
-- ============================================================================
CREATE TABLE IF NOT EXISTS validations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    -- What was validated
    language TEXT NOT NULL,
    matrix INTEGER NOT NULL,

    -- Iteration count validation
    expected_iterations INTEGER NOT NULL,  -- From reference
    actual_iterations INTEGER NOT NULL,    -- From run
    valid INTEGER NOT NULL,                -- 0 = failed, 1 = passed
    error_message TEXT,                    -- Details if validation failed

    -- Output format validation
    format_valid INTEGER,                  -- 0 = failed, 1 = passed, NULL = not checked
    format_error TEXT,                     -- First line where format differs

    -- Metadata
    validated_at DATETIME DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CHECK (matrix >= 0 AND matrix <= 6),
    CHECK (valid IN (0, 1)),
    CHECK (format_valid IS NULL OR format_valid IN (0, 1))
);

-- Indexes for validation queries
CREATE INDEX IF NOT EXISTS idx_validations_language ON validations(language);
CREATE INDEX IF NOT EXISTS idx_validations_valid ON validations(valid);
CREATE INDEX IF NOT EXISTS idx_validations_language_matrix ON validations(language, matrix);

-- ============================================================================
-- LANGUAGES TABLE - Language metadata and configuration
-- ============================================================================
CREATE TABLE IF NOT EXISTS languages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    -- Identity
    name TEXT NOT NULL UNIQUE,            -- C, Python, Go, etc.
    display_name TEXT,                    -- Display name (optional)

    -- Classification
    tier INTEGER DEFAULT 1,               -- 1 = mainstream, 2 = popular, 3 = niche
    paradigm TEXT,                        -- compiled, interpreted, jvm, etc.

    -- Status
    status TEXT DEFAULT 'pending',        -- pending, in_progress, validated, failed
    validated_at DATETIME,                -- When validation passed

    -- Configuration
    enabled INTEGER DEFAULT 1,            -- 0 = disabled, 1 = enabled
    max_matrix INTEGER DEFAULT 5,         -- Maximum matrix to run (1-6)

    -- Metadata
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CHECK (tier IN (1, 2, 3)),
    CHECK (status IN ('pending', 'in_progress', 'validated', 'failed')),
    CHECK (enabled IN (0, 1)),
    CHECK (max_matrix >= 1 AND max_matrix <= 6)
);

-- Index for language lookups
CREATE INDEX IF NOT EXISTS idx_languages_name ON languages(name);
CREATE INDEX IF NOT EXISTS idx_languages_status ON languages(status);

-- ============================================================================
-- COMPILER_VARIANTS TABLE - Track different compilation/runtime options
-- ============================================================================
CREATE TABLE IF NOT EXISTS compiler_variants (
    id INTEGER PRIMARY KEY AUTOINCREMENT,

    -- Which language
    language TEXT NOT NULL,

    -- Variant details
    variant_name TEXT NOT NULL,           -- O0, O2, O3, Ofast, default, etc.
    flags TEXT,                           -- Full compilation flags
    description TEXT,                     -- Human-readable description

    -- Performance designation
    is_default INTEGER DEFAULT 0,         -- 1 if this is the default variant
    is_fastest INTEGER DEFAULT 0,         -- 1 if this is the fastest known variant

    -- Metadata
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,

    -- Constraints
    CHECK (is_default IN (0, 1)),
    CHECK (is_fastest IN (0, 1)),
    UNIQUE (language, variant_name)
);

-- Index for variant lookups
CREATE INDEX IF NOT EXISTS idx_compiler_variants_language ON compiler_variants(language);

-- ============================================================================
-- SEED DATA - Tier 1 languages
-- ============================================================================
INSERT OR IGNORE INTO languages (name, display_name, tier, paradigm, status) VALUES
    ('C', 'C', 1, 'compiled', 'pending'),
    ('C++', 'C++', 1, 'compiled', 'pending'),
    ('Go', 'Go', 1, 'compiled', 'pending'),
    ('Rust', 'Rust', 1, 'compiled', 'pending'),
    ('Python', 'Python', 1, 'interpreted', 'pending'),
    ('JavaScript', 'JavaScript (Node.js)', 1, 'interpreted', 'pending'),
    ('TypeScript', 'TypeScript', 1, 'compiled_to_js', 'pending'),
    ('Java', 'Java', 1, 'jvm', 'pending'),
    ('Kotlin', 'Kotlin', 1, 'jvm', 'pending'),
    ('Scala', 'Scala', 1, 'jvm', 'pending'),
    ('C#', 'C#', 1, 'dotnet', 'pending'),
    ('PHP', 'PHP', 1, 'interpreted', 'pending'),
    ('Ruby', 'Ruby', 1, 'interpreted', 'pending'),
    ('Perl', 'Perl', 1, 'interpreted', 'pending'),
    ('Swift', 'Swift', 1, 'compiled', 'pending');

-- ============================================================================
-- VIEWS - Useful query abstractions
-- ============================================================================

-- Latest run for each language/matrix combination
CREATE VIEW IF NOT EXISTS v_latest_runs AS
SELECT
    r.*
FROM runs r
INNER JOIN (
    SELECT language, matrix, MAX(timestamp) as max_timestamp
    FROM runs
    WHERE status = 'success'
    GROUP BY language, matrix
) latest ON r.language = latest.language
    AND r.matrix = latest.matrix
    AND r.timestamp = latest.max_timestamp;

-- Validation status summary by language
CREATE VIEW IF NOT EXISTS v_validation_summary AS
SELECT
    language,
    COUNT(*) as total_validations,
    SUM(CASE WHEN valid = 1 THEN 1 ELSE 0 END) as passed_validations,
    SUM(CASE WHEN valid = 0 THEN 1 ELSE 0 END) as failed_validations,
    SUM(CASE WHEN format_valid = 1 THEN 1 ELSE 0 END) as format_passed,
    SUM(CASE WHEN format_valid = 0 THEN 1 ELSE 0 END) as format_failed,
    MAX(validated_at) as last_validated
FROM validations
GROUP BY language;

-- Performance leaderboard (fastest time by language/matrix)
CREATE VIEW IF NOT EXISTS v_leaderboard AS
SELECT
    matrix,
    language,
    MIN(time_ms) as best_time_ms,
    iterations,
    compiler_variant,
    timestamp
FROM runs
WHERE status = 'success'
GROUP BY matrix, language
ORDER BY matrix, best_time_ms;

-- ============================================================================
-- SCHEMA VERSION
-- ============================================================================
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

INSERT OR IGNORE INTO schema_version (version) VALUES (1);
