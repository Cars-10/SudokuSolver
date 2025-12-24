# Codebase Concerns

**Analysis Date:** 2025-12-24

## Tech Debt

**TypeScript/JavaScript Duplication:**
- Issue: Compiled .js files committed alongside .ts source files
- Files: `Metrics/HTMLGenerator.ts` + `HTMLGenerator.js`, `Metrics/LanguagesMetadata.ts` + `LanguagesMetadata.js`, `Metrics/PersonaMetadata.ts` + `PersonaMetadata.js`, `Metrics/SolverRunner.ts` + `SolverRunner.js`, `Metrics/HistoryManager.ts` + `HistoryManager.js`, `Metrics/RepositoryAnalyzer.ts` + `RepositoryAnalyzer.js`
- Why: Likely for fallback execution without TypeScript compilation
- Impact: Changes must be applied to both versions, maintenance burden
- Fix approach: Remove .js files, use tsx for direct TS execution

**Overly Large HTML Generator:**
- Issue: Single file handling too many responsibilities
- Files: `Metrics/HTMLGenerator.ts` (2001 lines)
- Why: Organic growth without refactoring
- Impact: Difficult to locate bugs, hard to test, complex maintenance
- Fix approach: Extract into smaller modules (template engine, data transformer, D3 wrapper)

## Known Bugs

**No critical bugs identified**

The codebase appears functionally stable based on analysis.

## Security Considerations

**Path Traversal Risk:**
- Risk: User input used in file paths without validation
- Files: `server/index.js` lines 86, 118, 228, 306, 352
- Current mitigation: `path.join()` normalizes paths somewhat
- Recommendations: Validate `lang` and `language` parameters against allowlist of valid language directories before path operations

**Unsafe Shell Command Execution:**
- Risk: Language parameter flows into exec() command working directory
- Files: `server/index.js` lines 152, 434
- Current mitigation: Script names are hardcoded, only cwd changes
- Recommendations: Validate language parameter against known languages before execution

**Unvalidated File Uploads:**
- Risk: File extension taken from user input without strict validation
- Files: `server/index.js` lines 299-333, 336-365
- Current mitigation: Basic sanitization (`safeExt = ext.replace(/[^a-z0-9.]/gi, '')`)
- Recommendations: Validate against explicit allowlist of file extensions

## Performance Bottlenecks

**Large History File:**
- Problem: Ever-growing benchmark history without pruning
- Files: `benchmark_history.json` (4278 lines currently)
- Measurement: File grows with each benchmark run
- Cause: No pruning strategy implemented
- Improvement path: Add rotation or archival after N records

**No Database Connection Pooling:**
- Problem: Database opened/closed on each operation
- Files: `Metrics/db_utils.js` (all database functions)
- Cause: Simple implementation pattern
- Improvement path: Implement connection pooling or keep-alive

## Fragile Areas

**Database Connection Handling:**
- Why fragile: No try-finally blocks for db.close()
- Files: `Metrics/db_utils.js` lines 134-136
- Common failures: Connection leaks if operation throws between open and close
- Safe modification: Wrap all db operations in try-finally
- Test coverage: No automated tests for database operations

**JSON Parsing Without Validation:**
- Why fragile: JSON.parse() without schema validation
- Files: `Metrics/SolverRunner.ts` lines 46, 134; `Metrics/run_suite.ts` lines 63, 118; `Metrics/HistoryManager.ts` lines 16, 33; `server/index.js` lines 201, 232, 252, 279
- Common failures: Malformed JSON causes cryptic errors deep in processing
- Safe modification: Add Zod schema validation at parse points
- Test coverage: No validation tests for malformed input

**Concurrent File Write Race Condition:**
- Why fragile: Multiple locations write to same files without locking
- Files: `Metrics/run_suite.ts` line 159, `server/index.js` line 190, `Metrics/HistoryManager.ts` line 27
- Common failures: Simultaneous benchmark runs can cause data loss
- Safe modification: Implement file locking or use database for writes
- Test coverage: No concurrency tests

## Scaling Limits

**File-Based State:**
- Current capacity: Works fine for single-user local usage
- Limit: Concurrent benchmark runs may corrupt state
- Symptoms at limit: Metrics overwritten, history lost
- Scaling path: Migrate to SQLite with proper locking

## Dependencies at Risk

**None identified** - Core dependencies (Express, Puppeteer, better-sqlite3) are actively maintained.

## Missing Critical Features

**No Input Sanitization Layer:**
- Problem: Each endpoint implements its own (incomplete) validation
- Current workaround: Implicit trust of input
- Blocks: Cannot safely expose to untrusted networks
- Implementation complexity: Low - add validation middleware

**No .env.example File:**
- Problem: Environment variables not documented
- Current workaround: Read .env directly or guess
- Blocks: Developer onboarding
- Implementation complexity: Low - create example file

## Test Coverage Gaps

**No Automated Tests:**
- What's not tested: All TypeScript and JavaScript code
- Risk: Regressions go unnoticed until manual testing
- Priority: Medium
- Difficulty to test: Low for unit tests, medium for integration

**Database Operations:**
- What's not tested: All db_utils.js functions
- Risk: Connection leaks, data corruption
- Priority: Medium
- Difficulty to test: Medium - requires test database setup

**API Endpoints:**
- What's not tested: All Express routes
- Risk: Breaking changes to API contract
- Priority: High for /api/run endpoint
- Difficulty to test: Low with supertest library

---

*Concerns audit: 2025-12-24*
*Update as issues are fixed or new ones discovered*

## Summary of Severity

| Severity | Count | Areas |
|----------|-------|-------|
| HIGH | 2 | Code duplication, Path traversal risk |
| MEDIUM | 6 | Large generator, Shell execution, File uploads, JSON parsing, Race conditions, DB handling |
| LOW | 3 | No .env.example, Large history file, Hardcoded Puppeteer path |

Most actionable issues: Input validation, code duplication cleanup, database connection management.
