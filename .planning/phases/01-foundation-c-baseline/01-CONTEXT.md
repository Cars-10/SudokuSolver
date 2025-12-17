# Phase 1: Foundation & C Baseline - Context

**Gathered:** 2025-12-16
**Status:** For planning

<phase_objectives>
## What This Phase Accomplishes

Rebuild all infrastructure on new server and validate C solver as the authoritative reference baseline for algorithmic consistency.

**Primary goal:**
Create production-ready infrastructure (Docker, Content Server, database, validation systems, modular scripts, screenshot automation) and validate that C solver matches reference iteration counts exactly, establishing the pattern for all 14 remaining languages.

**Clarifications:**
- Docker image built with ALL 15 language toolchains upfront (not incremental)
- Content Server fixes include: modal positioning (calculate from mouse event), edit/update logic (metadata, authors, logos), logo system rebuild (Sharp-based, SVG→PNG, tailoring)
- Modular scripts: Languages/common.sh with shared functions, per-language runMe.sh calls common functions with language-specific overrides
- Validation systems: Check iteration count matches reference AND output format matches C exactly (spacing, headers, paths)
- C baseline: Not just "runs" but produces exact reference output (656, 439269, 98847, 9085, 445778 iterations for matrices 1-5)

**Out of scope:**
- Compiler variants for C (Phase 1 uses single variant, variants added later)
- Matrix 6 execution (shown blank in reports)
- Language implementations beyond C (Phases 2-5)
- Performance optimization of Docker image size (build complete image first, optimize later if needed)
- Advanced logo processing (just basic upload, URL fetch, SVG→PNG, invert/transparent_white)

</phase_objectives>

<constraints>
## Constraints

**Technical:**
- Docker base image: ubuntu:24.04 (latest LTS)
- Docker image name: sudoku-benchmark:latest (reuse this name, remove old before creating new)
- External port: 9001 (Content Server must serve on this port)
- Logo formats: PNG only (convert all SVG to PNG using Sharp library)
- Logo tailoring: Support only `invert` and `transparent_white` options from Tailoring.json
- Script architecture: Balance simplicity (easy to understand), reusability (DRY where it makes sense), flexibility (languages can override)
- Timeout enforcement: 5-minute maximum per matrix run, abort process on timeout
- Validation strictness: Zero tolerance - iteration counts must match exactly, output format must match exactly

**Timeline:**
- Critical path dependency: SQLite database must be operational before other components (blocks metrics capture)
- Phase 1 blocks all other phases - nothing can proceed until C baseline validated

**Resources:**
- Docker image size: Estimated ~10GB with 15 toolchains (acceptable, don't optimize prematurely)
- Host system: macOS localhost (Darwin), Docker installed

**Dependencies:**
- New server environment (fresh Docker setup required)
- Reference data: Matrices/ReferenceForAllMatrixRun.txt exists and contains canonical iteration counts
- Existing codebase:
  - Content Server code exists (server/index.js) but needs fixes
  - HTMLGenerator.ts works but may need updates for new features
  - C/setupAndRunMe.sh exists but needs modularization
  - Languages/metadata.json exists for language metadata
  - logos/Tailoring.json exists (Awk: invert, C: transparent_white)

**Other:**
- Must preserve existing metrics.json files for debugging (SQLite is additive, not replacement)
- Must keep benchmark_history.json data (migrate to SQLite as historical import)

</constraints>

<risks>
## Risks and Mitigation

**Risk 1: Docker image size exceeds expectations**
- **Likelihood:** Medium (15 toolchains could be >10GB)
- **Impact:** Low (disk space available, size not blocking)
- **Mitigation:** Build once, test functionality first, defer size optimization. Monitor actual size during build.

**Risk 2: C validation reveals algorithm drift**
- **Likelihood:** Medium (benchmark_history.json shows iteration count of 100, not 656)
- **Impact:** High (would need to fix C implementation before proceeding)
- **Mitigation:** Validate C first before building other infrastructure. If C doesn't match, fix C algorithm before modularizing scripts (don't propagate broken pattern).

**Risk 3: Modal positioning fix more complex than expected**
- **Likelihood:** Low (coordinate calculation is straightforward)
- **Impact:** Medium (UI broken until fixed, affects testing)
- **Mitigation:** Implement basic positioning first (mouse click + offset), add bounds checking second, test on different viewport sizes.

**Risk 4: Logo system reconstruction difficult (code was lost)**
- **Likelihood:** Medium (need to rebuild from scratch)
- **Impact:** Low (not blocking C validation, can iterate)
- **Mitigation:** Start with minimal implementation (upload works, URL fetch works, SVG→PNG works), add tailoring second. Test with existing logos (Awk.png, C.png).

**Risk 5: SQLite schema design inadequate for queries**
- **Likelihood:** Low (schema design well-understood)
- **Impact:** Medium (would require migration later)
- **Mitigation:** Design schema with time-series queries in mind (indexes on timestamp, language, matrix). Test representative queries before declaring complete.

**Risk 6: Modular script pattern doesn't fit all languages**
- **Likelihood:** Low (most languages similar to C)
- **Impact:** Medium (would need refactoring later)
- **Mitigation:** Design common.sh with flexibility in mind (languages can override functions). Test pattern with C, then validate with C++ (similar) before declaring success.

</risks>

<success_indicators>
## Success Indicators

**How we'll know this phase is complete:**

**Functional:**
- [ ] Docker image built (sudoku-benchmark:latest) with all 15 language toolchains installed
- [ ] Docker container runs and serves Content Server on port 9001
- [ ] Content Server modal positioning works (appears near mouse click, not top-left)
- [ ] Language Detail modal edit/update works (can edit metadata, upload logos, save successfully)
- [ ] Logo upload/URL fetch works (PNG uploaded, SVG converted to PNG, cached to logos/)
- [ ] Logo tailoring applied (invert and transparent_white transformations work)
- [ ] Languages/common.sh exists with shared functions (report_env_error, metrics generation, validation)
- [ ] C/runMe.sh uses common.sh (sources common functions, provides language-specific config)
- [ ] SQLite database created (Metrics/benchmarks.db) with full schema
- [ ] Database insert works (C run writes to database with all fields)
- [ ] Iteration count validation works (compares against ReferenceForAllMatrixRun.txt, reports pass/fail)
- [ ] Output format validation works (compares C output to reference, exact match including spacing)
- [ ] Puppeteer screenshot automation works (captures viewport after C run, saves to screenshots/)
- [ ] C solver runs matrices 1-5 successfully
- [ ] C iteration counts match reference EXACTLY (656, 439269, 98847, 9085, 445778)
- [ ] C output format matches reference EXACTLY (spacing, headers, paths)

**Quality:**
- [ ] All validation tests pass (manual and automated)
- [ ] Database schema supports time-series queries (tested with sample queries)
- [ ] Modular script pattern documented (README in Languages/ explaining pattern)
- [ ] No errors in Docker build log
- [ ] Content Server starts without errors
- [ ] Screenshots captured correctly (viewport only, not full page)

**Deployment:**
- [ ] Docker image running on localhost
- [ ] Content Server accessible at localhost:9001
- [ ] Database file created and accessible within Docker

**Documentation:**
- [ ] SQLite schema documented (table structure, indexes, field meanings)
- [ ] Modular script pattern documented (what goes in common.sh, what in runMe.sh)
- [ ] C implementation documented (README.md in Languages/C/)
- [ ] Reference iteration counts documented (clearly stated in validation system)

**User-facing:**
- [ ] Benchmark report displays (HTML generated, D3 charts render)
- [ ] Language modal opens near click location
- [ ] Edit metadata works smoothly
- [ ] Logo appears in report with tailoring applied
- [ ] Screenshot shows current benchmark state

</success_indicators>

<codebase_context>
## Codebase State and Patterns

**Current state:**
Existing codebase with partial infrastructure - Content Server code exists, HTMLGenerator works, language scripts exist but need refactoring. Running on new server (requires fresh Docker setup).

**Relevant files/systems:**
- `server/index.js` - Content Server (Express, handles API for metadata editing, logo uploads)
- `Metrics/report_client.js` - Client-side JS for modal interactions (needs modal positioning fix)
- `Metrics/HTMLGenerator.ts` - Generates benchmark_report.html from metrics data
- `Languages/C/setupAndRunMe.sh` - Current C script (needs modularization into common.sh pattern)
- `Languages/C/Sudoku.c` - C baseline implementation (needs validation)
- `Languages/metadata.json` - Language metadata (creator, date, description, logo path)
- `logos/Tailoring.json` - Logo tailoring config (Awk: invert, C: transparent_white)
- `Matrices/ReferenceForAllMatrixRun.txt` - Reference output from C (canonical truth)
- `benchmark_config.json` - Configuration (baseline: C, completed languages, matrix thresholds)
- `benchmark_history.json` - Historical run data (migrate to SQLite)

**Patterns to follow:**
- Metrics output: JSON array with solver, runType, timestamp, results[] (matrix, time, iterations, memory, cpu_user, cpu_sys, status, output)
- Error handling: report_env_error() function generates error metrics.json
- Output validation: "Solved in Iterations=N" string parsing
- Logo storage: logos/[LanguageName].png or logos/[LanguageName].svg (convert to PNG)
- Docker naming: Reuse sudoku-benchmark:latest (remove old image first)

**External dependencies:**
- Node.js/npm (for Content Server, HTMLGenerator, Puppeteer)
- Sharp (npm package for image processing)
- Puppeteer (npm package for screenshot automation)
- SQLite3 (install in Docker image)
- TypeScript (for HTMLGenerator.ts compilation)
- Express (for Content Server)
- D3.js (already in use for charts)

**Known issues to address:**
- Modal positioning: Fixed position instead of calculated from mouse event
- Edit/update logic: Upload/paste not working, metadata save broken
- Logo system: Code lost, needs rebuild using Sharp
- Iteration counts: benchmark_history.json shows 100 instead of 656 (algorithm drift or test data)
- No database: Using scattered JSON files instead of SQLite

**Prior decisions affecting this phase:**
- From PROJECT.md: Use brute-force algorithm only, validate via iteration count, 5-minute timeout
- From PROJECT.md: Docker kitchen-sink approach (all toolchains in one image)
- From PROJECT.md: Serial language implementation (complete one before next)
- From MANIFESTO.md: Neon theme, pragmatic approach, TDD for languages (Matrix 1 test first)

</codebase_context>

<decisions_needed>
## Decisions That Will Affect Implementation

**Decision 1: SQLite schema field types and indexes**
- **Context:** Need to support time-series queries (performance over time), aggregations (fastest language), filtering (by variant, matrix)
- **Options:**
  - Minimal: Basic fields, no indexes (add later if needed)
  - Standard: Fields from PROJECT.md proposal, indexes on common queries
  - Extended: Add hardware fingerprint, environment snapshot for full reproducibility
- **When to decide:** During planning (affects database setup task)

**Decision 2: Modular script function boundaries**
- **Context:** What goes in common.sh vs runMe.sh? Need clear interface that works for 15 languages
- **Options:**
  - All in common.sh (languages just set variables like LANGUAGE, COMPILE_CMD, RUN_CMD)
  - Split responsibilities (common.sh: metrics/validation, runMe.sh: compile/run)
  - Flexible (common.sh provides defaults, languages override specific functions)
- **When to decide:** During planning (affects script architecture task)

**Decision 3: Puppeteer screenshot timing**
- **Context:** When exactly to trigger screenshot? After HTML generated? After server restart? After specific delay?
- **Options:**
  - After HTMLGenerator.ts completes (immediate)
  - After benchmark_report.html file written + 1 second delay
  - Triggered by Content Server (screenshot endpoint called after report update)
- **When to decide:** During task breakdown for screenshot automation

**Decision 4: C validation failure handling**
- **Context:** If C doesn't match reference (likely based on benchmark_history.json showing 100 iterations), do we fix C first or continue with infrastructure?
- **Options:**
  - Fix C immediately (block everything until reference matches)
  - Build infrastructure first, fix C later (risk propagating broken pattern)
  - Validate C first, halt if broken (recommended - don't build on broken foundation)
- **When to decide:** During planning (affects task order)

</decisions_needed>

<notes>
## Additional Context

[Questions asked during intake:]
- Q: What's the current state of the existing codebase and infrastructure?
- A: All of the above - Content Server exists but needs fixes, HTMLGenerator works, scripts exist but need refactoring

- Q: For the Docker image with 15 toolchains (~10GB), what's the priority?
- A: Build complete image upfront - all 15 toolchains at once

- Q: What's blocking progress if we don't fix everything in Phase 1?
- A: SQLite database - must be operational to capture metrics from C and future languages

- Q: How will we verify each component works correctly?
- A: All verification methods - manual testing, automated tests where critical, end-to-end smoke test

- Q: Are there specific concerns or constraints for the logo processing system?
- A: Just make it work - basic functionality (upload, URL fetch, SVG→PNG, tailoring), optimize later

- Q: For the modular script architecture, what should the design prioritize?
- A: Balance all three - simple to understand, reusable where it makes sense, flexible where languages differ

[Clarifications:]
- Critical path is SQLite database - this unlocks everything else
- Docker image complexity is acceptable - build all toolchains upfront, optimize later if needed
- Logo system should be functional, not perfect - get basic features working, iterate later
- Modular scripts need to balance simplicity, reusability, and flexibility - don't over-engineer

[References:]
- ReferenceForAllMatrixRun.txt - Contains canonical C output for all 6 matrices
- MANIFESTO.md - Project philosophy and conventions
- benchmark_config.json - Current configuration and completed languages

</notes>

---

*Phase: 01-foundation-c-baseline*
*Context gathered: 2025-12-16*
*Ready for planning: yes*
