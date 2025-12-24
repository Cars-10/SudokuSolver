# Codebase Concerns

**Analysis Date:** 2025-12-24

## Tech Debt

**Mixed Sync/Async File Operations:**
- Issue: Blocking synchronous I/O in async Express handlers
- Files: `server/index.js:201,232,252,279` - `fs.readFileSync()` in request handlers
- Files: `Metrics/generate_report_only.ts:123-128` - `fs.readFileSync()` during initialization
- Why: Legacy code patterns, incremental development
- Impact: Performance degradation under load, blocked event loop
- Fix approach: Convert to async `fs.readFile()` with proper await

**Large Monolithic HTMLGenerator:**
- Issue: 26,000+ line file mixing concerns (HTML generation, data processing, visualization)
- File: `Metrics/HTMLGenerator.ts`
- Why: Rapid feature addition without refactoring
- Impact: Hard to maintain, test, and modify
- Fix approach: Extract into modules (DataProcessor, ChartGenerator, TemplateEngine)

**Missing .env.example:**
- Issue: No template showing required environment variables
- File: Root directory (missing `.env.example`)
- Why: Oversight during initial development
- Impact: Onboarding friction for new contributors
- Fix approach: Create `.env.example` with `WEBHOST`, `WEBPORT`, `PORT`

**Inconsistent Dependency Versions:**
- Issue: Same packages at different versions across package.json files
- Files: `package.json`, `Metrics/package.json`, `server/package.json`
- Examples: Express 4.18.2 vs 5.2.1, Puppeteer 21.0.0 vs 24.32.0, glob 10.5.0 vs 13.0.0
- Impact: Unpredictable behavior, potential security issues
- Fix approach: Consolidate to single version per package, consider monorepo tooling

## Known Bugs

**Race Condition in History Writes:**
- Symptoms: Concurrent benchmark runs can lose data
- Files: `Metrics/HistoryManager.ts:14-27`, `server/index.js:175-193`
- Trigger: Multiple simultaneous `/api/run` requests
- Workaround: Run benchmarks sequentially
- Root cause: Read-modify-write pattern without file locking
- Fix: Implement file locking or atomic writes

## Security Considerations

**Path Traversal Vulnerability:**
- Risk: User input used in file path construction without validation
- Files:
  - `server/index.js:86` - `path.join(MATRICES_DIR, req.params.filename)`
  - `server/index.js:112-150` - `req.body.language` in path construction
- Current mitigation: None
- Recommendations: Validate inputs against whitelist, use path.resolve and check against base directory

**Command Injection Vulnerability:**
- Risk: Template strings used in shell command construction
- Files:
  - `Metrics/SolverRunner.ts:37` - `` `${globalScript} ${langName} ${matrixName}` ``
  - `Metrics/SolverRunner.ts:112,121` - Docker commands with interpolation
  - `server/index.js:152` - `exec(command, { cwd: langDir })`
- Current mitigation: None
- Recommendations: Use execFile with argument arrays instead of exec with string interpolation

**SSRF Vulnerability:**
- Risk: Server can be used to fetch arbitrary URLs
- Files: `server/index.js:336-364` - Logo download endpoint accepts user URLs
- Current mitigation: Minimal extension sanitization
- Recommendations: Validate URL format, whitelist allowed domains, implement rate limiting

**Missing Input Validation:**
- Risk: Type confusion, injection attacks
- Files:
  - `server/index.js:244` - Language name not validated
  - `server/index.js:405` - URL not validated for format/protocol
  - `Metrics/run_suite.ts:35-45` - Matrix argument not validated
- Recommendations: Add input validation with zod or joi

## Performance Bottlenecks

**Synchronous Report Generation:**
- Problem: Entire HTML generation is synchronous
- File: `Metrics/HTMLGenerator.ts:generateHtml()`
- Measurement: Not benchmarked, but noticeable delay with large datasets
- Cause: Single-threaded string concatenation, large JSON serialization
- Improvement path: Stream-based generation, worker threads for heavy processing

**N+1 Pattern in Metrics Merge:**
- Problem: Individual merge operations per metrics entry
- File: `server/index.js:175-186`
- Cause: Converting Map to array one entry at a time
- Improvement path: Batch operations, use Map directly

## Fragile Areas

**Solver Execution Pipeline:**
- Files: `Metrics/SolverRunner.ts`, `server/index.js:111-220`
- Why fragile: Relies on exact script naming (`runMe.sh`, `setupAndRunMe.sh`)
- Common failures: Missing scripts, permission issues, timeout
- Safe modification: Test with multiple languages after changes
- Test coverage: No automated tests

**History Persistence:**
- File: `Metrics/HistoryManager.ts`
- Why fragile: Silent failures return empty array, masking corruption
- Common failures: JSON parse errors, concurrent write conflicts
- Safe modification: Add validation before returning data
- Test coverage: No automated tests

## Dependencies at Risk

**TypeScript Strict Mode Disabled:**
- File: `Metrics/tsconfig.json` - `"strict": false`
- Risk: Type safety bypassed, bugs not caught at compile time
- Impact: Runtime type errors, harder debugging
- Migration plan: Enable strict mode incrementally

**No Automated Security Audits:**
- Risk: Vulnerable dependencies not detected
- Impact: Security vulnerabilities in production
- Fix: Add `npm audit` to CI pipeline, configure `.npmrc`

## Missing Critical Features

**No Test Framework:**
- Problem: No automated testing infrastructure
- Current workaround: Manual validation scripts (`check_invalid.mjs`)
- Blocks: Confident refactoring, regression detection
- Implementation complexity: Low (vitest setup)

**No CI/CD Pipeline:**
- Problem: No automated testing or deployment
- Current workaround: Manual deployment
- Blocks: Continuous integration, automated quality gates
- Implementation complexity: Low (GitHub Actions)

**No Input Validation Layer:**
- Problem: API endpoints accept arbitrary input
- Current workaround: None
- Blocks: Security hardening
- Implementation complexity: Medium (add validation middleware)

## Test Coverage Gaps

**Solver Execution Logic:**
- What's not tested: `Metrics/SolverRunner.ts` execution flow
- Risk: Timeout handling, error recovery could fail silently
- Priority: High
- Difficulty: Medium (need to mock child_process)

**File Operations:**
- What's not tested: `Metrics/HistoryManager.ts`, `RepositoryAnalyzer.ts`
- Risk: Data corruption, silent failures
- Priority: High
- Difficulty: Low (straightforward file system mocking)

**API Endpoints:**
- What's not tested: All routes in `server/index.js`
- Risk: Input validation gaps, error handling issues
- Priority: Medium
- Difficulty: Low (supertest integration tests)

---

*Concerns audit: 2025-12-24*
*Update as issues are fixed or new ones discovered*
