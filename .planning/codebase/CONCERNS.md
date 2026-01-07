# Codebase Concerns

**Analysis Date:** 2026-01-07

## Security Considerations

**Command Injection in API Endpoints:**
- Risk: User input (`matrix`, `lang` parameters) passed to shell commands without sanitization
- Files: `server/index.js` (lines 145-170)
- Current mitigation: None
- Recommendations:
  - Use `execFile()` with array arguments instead of `exec()` with string
  - Validate `lang` against whitelist of known languages
  - Validate `matrix` against known matrix filenames
  - Use path normalization to prevent traversal attacks

**Path Traversal Vulnerability:**
- Risk: `req.params.filename` used directly in file operations
- File: `server/index.js` (line 86)
- Impact: Could expose files outside intended directories via `../../../` sequences
- Recommendations: Use `path.resolve()` and verify result is within allowed directory

**Broad Static File Serving:**
- Risk: Serves entire parent directory via Express static middleware
- File: `server/index.js` (lines 14-17)
- Impact: Could expose `.env`, config files, source code
- Recommendations: Limit static serving to specific directories only

**Missing .env.example:**
- Risk: No documentation of required environment variables
- Impact: New developers may misconfigure deployment
- Recommendations: Create `.env.example` with documented variables

## Tech Debt

**Duplicate TypeScript/JavaScript Files:**
- Issue: Paired TS/JS versions of same modules in `Metrics/`
- Files:
  - `HTMLGenerator.ts` (1996 lines) + `HTMLGenerator.js` (481 lines)
  - `LanguagesMetadata.ts` (840 lines) + `LanguagesMetadata.js` (777 lines)
  - `PersonaMetadata.ts` (688 lines) + `PersonaMetadata.js` (673 lines)
  - `HistoryManager.ts` + `HistoryManager.js`
  - `SolverRunner.ts` + `SolverRunner.js`
  - `RepositoryAnalyzer.ts` + `RepositoryAnalyzer.js`
  - `ScreenshotUtils.ts` + `ScreenshotUtils.js`
  - `generate_report_only.ts` + `generate_report_only.js`
- Impact: Changes must be duplicated, unclear which is authoritative
- Fix approach: Remove JS duplicates, use only TypeScript with tsx runner

**Large Monolithic Files:**
- Issue: Several files exceed maintainable size
- Files:
  - `Metrics/report_client.js`: 3543 lines
  - `Metrics/HTMLGenerator.ts`: 1996 lines
  - `server/index.js`: 500+ lines
- Impact: Hard to navigate, test, and maintain
- Fix approach: Extract into smaller focused modules

**Missing Compiler Variant Tracking:**
- Issue: TODO in sync_db.ts indicates incomplete feature
- File: `Metrics/sync_db.ts` (line 111)
- Code: `compiler_variant: 'default' // TODO: add to JSON if needed`
- Impact: Can't distinguish results from different compiler versions
- Fix approach: Add compiler_variant field to metrics.json schema

## Performance Bottlenecks

**Database Connection Per Query:**
- Problem: New SQLite connection created for every operation
- File: `Metrics/db_utils.js` (lines 20-24)
- Impact: Overhead for connection setup/teardown on each query
- Improvement: Implement connection pooling or singleton pattern

**No Caching for Metadata:**
- Problem: Language metadata loaded fresh on each request
- Files: `Metrics/LanguagesMetadata.ts`, `Metrics/PersonaMetadata.ts`
- Impact: 840+ lines of data parsed repeatedly
- Improvement: Cache parsed metadata, invalidate on file change

**Synchronous Report Generation:**
- Problem: All report content built in memory before output
- File: `Metrics/HTMLGenerator.ts`
- Impact: Memory pressure for large datasets, blocking operation
- Improvement: Stream output, process incrementally

## Fragile Areas

**Metrics JSON Parsing:**
- Why fragile: No schema validation on JSON.parse()
- Files: `server/index.js` (lines 135, 163, 201, 232, 252, 279)
- Common failures: Malformed JSON, missing fields, type mismatches
- Safe modification: Add Zod schema validation before processing
- Test coverage: None

**Report HTML Generation:**
- Why fragile: String concatenation for HTML, complex nested logic
- File: `Metrics/HTMLGenerator.ts` (lines 80-180)
- Common failures: XSS vulnerabilities, broken HTML structure
- Safe modification: Use template engine or JSX
- Test coverage: None

**Shell Script Execution:**
- Why fragile: Relies on specific shell behavior and language runtimes
- Files: `Languages/*/setupAndRunMe.sh`
- Common failures: Missing dependencies, path issues, permission problems
- Safe modification: Add better error reporting, dependency checks
- Test coverage: Manual only

## Test Coverage Gaps

**API Endpoints:**
- What's not tested: All `/api/*` routes in `server/index.js`
- Risk: Breaking changes go unnoticed
- Priority: High
- Difficulty: Medium (mock child_process, database)

**Database Operations:**
- What's not tested: `Metrics/db_utils.js` CRUD operations
- Risk: Data corruption, query failures
- Priority: High
- Difficulty: Low (use in-memory SQLite)

**Report Generation:**
- What's not tested: HTML output correctness
- Risk: Broken reports, invalid HTML
- Priority: Medium
- Difficulty: Medium (snapshot testing)

**Solver Validation:**
- What's not tested: metrics.json structure validation
- Risk: Invalid data stored and processed
- Priority: Medium
- Difficulty: Low (add JSON schema)

## Missing Critical Features

**Input Validation:**
- Problem: No validation on API parameters
- Current workaround: Trust all input
- Blocks: Secure deployment
- Fix: Add Zod/Joi validation middleware

**Error Recovery:**
- Problem: No graceful handling of partial failures
- Current workaround: Manual intervention
- Blocks: Unattended operation
- Fix: Add retry logic, error aggregation

**Authentication:**
- Problem: No access controls
- Current workaround: Local/trusted network only
- Blocks: Public deployment
- Fix: Add basic auth or API keys (if needed)

## Dependencies at Risk

**Puppeteer Version Sprawl:**
- Risk: Multiple versions across packages (21.0.0, 24.31.0, 24.32.0)
- Files: `package.json`, `server/package.json`, `Metrics/package.json`
- Impact: Inconsistent behavior, large node_modules
- Migration: Consolidate to single version in root package.json

**No Lock on Node Version:**
- Risk: Different Node versions may behave differently
- Impact: "Works on my machine" issues
- Migration: Add `.nvmrc` or `engines` field

## Known Issues

**Race Condition in History Append:**
- Symptoms: Potential data loss if multiple benchmarks finish simultaneously
- File: `Metrics/HistoryManager.ts`
- Trigger: Concurrent benchmark execution
- Workaround: Run benchmarks sequentially
- Root cause: No file locking on JSON append

---

*Concerns audit: 2026-01-07*
*Update as issues are fixed or new ones discovered*
