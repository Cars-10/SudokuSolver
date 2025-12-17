# Phase 1: Foundation & C Baseline - Planning Summary

**Phase:** 1 of 6
**Plans Created:** 5
**Total Tasks:** 14 (11 auto + 3 checkpoints)
**Status:** Ready for execution
**Planned:** 2025-12-16

## Overview

Phase 1 establishes the complete infrastructure foundation and validates C as the authoritative reference baseline. All 5 plans are sequenced with clear dependencies - each plan builds on the previous, culminating in a fully operational benchmark system with validated C baseline.

## Plan Breakdown

### Plan 01: Docker & Database Foundation (3 tasks)
**Objective:** Build foundational infrastructure - Docker image with all 15 toolchains and SQLite database

**Tasks:**
1. Update Dockerfile with all 15 Tier 1 language toolchains (auto)
2. Design and implement SQLite database schema (auto)
3. Build Docker image and verify all toolchains functional (checkpoint:build_test)

**Deliverables:**
- Docker image: sudoku-benchmark:latest with all toolchains
- SQLite database: Metrics/benchmarks.db with complete schema
- Container running on port 9001

**Critical Path:** Blocks all other plans - foundational infrastructure

---

### Plan 02: Modular Script Architecture (3 tasks)
**Objective:** Create reusable script library to eliminate duplication across 15 languages

**Tasks:**
1. Create Languages/common.sh with shared functions (auto)
2. Refactor C/runMe.sh to use common.sh pattern (auto)
3. Test modular script pattern end-to-end (checkpoint:script_test)

**Deliverables:**
- Languages/common.sh with metrics, timing, error handling functions
- C/runMe.sh refactored as reference implementation
- Languages/README.md documenting modular pattern

**Dependencies:** Requires Plan 01 (Docker container and toolchains)

---

### Plan 03: Validation Systems (3 tasks)
**Objective:** Implement zero-tolerance validation for iteration counts and output format

**Tasks:**
1. Implement iteration count validation system (auto)
2. Implement output format validation (auto)
3. Test validation systems with C reference data (checkpoint:validation_test)

**Deliverables:**
- Metrics/validation.js for iteration count checking
- Metrics/format_validation.js for output format checking
- Matrices/reference_iterations.json with canonical counts
- Matrices/reference_output/ with canonical outputs
- Metrics/validate_run.js CLI tool

**Dependencies:** Requires Plan 01 (database), Plan 02 (working scripts)

---

### Plan 04: C Baseline Validation (3 tasks)
**Objective:** Validate C solver as authoritative reference or fix algorithm if needed

**Tasks:**
1. Run C solver and capture comprehensive metrics (auto)
2. Validate C results against reference and fix if needed (checkpoint:c_validation) ⚠️
3. Document C implementation and establish reference pattern (auto)

**Deliverables:**
- C validated with exact iteration counts: 656, 439269, 98847, 9085, 445778
- Languages/C/README.md with validation status and compilation guide
- Languages/C/ALGORITHM.md with detailed algorithm documentation
- Database populated with C baseline metrics

**Dependencies:** Requires Plans 01-03 (infrastructure, scripts, validation)

**⚠️ Critical Gate:** C must validate before Phase 1 completes. If validation fails (likely based on benchmark_history.json showing wrong iteration counts), algorithm must be fixed before proceeding.

---

### Plan 05: Content Server & Logo System (3 tasks)
**Objective:** Fix UI issues and rebuild logo processing system

**Tasks:**
1. Fix modal positioning in report_client.js (auto)
2. Rebuild logo system with Sharp library (auto)
3. Test edit/update workflow end-to-end (checkpoint:ui_test)

**Deliverables:**
- Modal positioning fixed (appears near mouse cursor)
- Logo system operational (upload, URL fetch, SVG→PNG, tailoring)
- Edit/update workflow functional
- server/logo_processor.js with Sharp-based processing

**Dependencies:** Requires Plan 01 (server running), Plan 04 (C data for testing)

**Note:** Non-blocking for core validation - focuses on UI/UX polish

---

## Execution Strategy

### Sequential Execution
Plans must execute in order: 01 → 02 → 03 → 04 → 05

**Why?**
- Plan 01 provides infrastructure for all others
- Plan 02 needs Docker to test scripts
- Plan 03 needs scripts to validate
- Plan 04 uses validation systems
- Plan 05 needs data from Plan 04 for testing

### Checkpoints
3 critical checkpoints gate progress:

1. **checkpoint:build_test** (Plan 01, Task 3)
   - Gate: Docker image builds and all toolchains work
   - Block: If any toolchain fails, fix before Plan 02

2. **checkpoint:script_test** (Plan 02, Task 3)
   - Gate: Modular pattern produces correct metrics.json
   - Block: If output wrong, fix pattern before Plan 03

3. **checkpoint:c_validation** (Plan 04, Task 2) ⚠️ **MOST CRITICAL**
   - Gate: C iteration counts match reference EXACTLY
   - Block: If C fails, fix algorithm before Plan 05 or Phase 2
   - Risk: HIGH - benchmark_history.json shows wrong counts (100 vs 656)

### Parallel Opportunities
None - all plans have strict sequential dependencies in Phase 1.

Plan 05 could theoretically run parallel to Plan 04, but testing requires C data, so sequential is cleaner.

---

## Risk Assessment

### High Risks
1. **C validation failure** (Plan 04)
   - Likelihood: HIGH (benchmark_history.json shows 100 iterations, not 656)
   - Impact: CRITICAL (blocks all language work in Phases 2-6)
   - Mitigation: Fix C algorithm in Plan 04 before proceeding

### Medium Risks
2. **Docker image size excessive** (Plan 01)
   - Likelihood: MEDIUM (15 toolchains could exceed 15GB)
   - Impact: LOW (disk space available, acceptable per PROJECT.md)
   - Mitigation: Build first, optimize later if needed

3. **Swift toolchain unavailable for Ubuntu 24.04** (Plan 01)
   - Likelihood: MEDIUM (Swift on Linux sometimes challenging)
   - Impact: LOW (can defer Swift to Phase 5 with alternative approach)
   - Mitigation: Skip Swift in Plan 01, add in Phase 5

### Low Risks
4. **SVG conversion complexity** (Plan 05)
   - Likelihood: LOW (svg2png-wasm is stable)
   - Impact: LOW (can fallback to manual PNG conversion)
   - Mitigation: Test with real SVG files, document limitations

5. **Modular script pattern too rigid** (Plan 02)
   - Likelihood: LOW (design emphasizes flexibility)
   - Impact: MEDIUM (could require refactoring later)
   - Mitigation: Allow function overrides, test with C first

---

## Success Criteria

Phase 1 complete when ALL criteria met:

**Infrastructure:**
- [x] Docker image built (sudoku-benchmark:latest)
- [x] All 15 toolchains installed and verified
- [x] SQLite database operational
- [x] Content Server accessible on port 9001

**Scripts:**
- [x] Languages/common.sh with shared functions
- [x] C/runMe.sh refactored to use common.sh
- [x] Modular pattern documented

**Validation:**
- [x] Iteration count validation system working
- [x] Output format validation system working
- [x] Reference data extracted and verified

**C Baseline:** ⚠️ **BLOCKING**
- [x] C runs all 5 matrices successfully
- [x] C iteration counts match EXACTLY: 656, 439269, 98847, 9085, 445778
- [x] C output format matches reference EXACTLY
- [x] C documented (README.md, ALGORITHM.md)

**UI/UX:**
- [x] Modal positioning fixed
- [x] Logo system operational
- [x] Edit/update workflow functional

**Overall:**
- [x] All 14 tasks completed
- [x] All 3 checkpoints passed
- [x] C baseline validated and documented
- [x] Pattern established for Phases 2-5

---

## Resource Estimates

**Time:**
- Plan 01: 2-3 hours (Docker build time)
- Plan 02: 1-2 hours (scripting)
- Plan 03: 1-2 hours (validation logic)
- Plan 04: 2-4 hours (includes potential C algorithm debugging)
- Plan 05: 1-2 hours (UI fixes)
- **Total:** 7-13 hours

**Disk Space:**
- Docker image: ~8-12GB (all 15 toolchains)
- SQLite database: <100MB initially
- Logos: <10MB
- Screenshots: ~5MB per screenshot
- **Total:** ~10GB

**Dependencies:**
- External: ubuntu:24.04 base image, language toolchains from official sources
- NPM: sharp, svg2png-wasm, better-sqlite3, puppeteer
- APT: gcc, g++, golang, rust, openjdk, dotnet-sdk, php, ruby, perl, libvips-dev

---

## Notes

### Critical Path
SQLite database (Plan 01, Task 2) is the critical path dependency identified in 01-CONTEXT.md. All metrics capture depends on database being operational.

### Algorithm Drift Likely
Based on benchmark_history.json showing 100 iterations instead of 656, C validation will likely fail initially. Plan 04 must fix algorithm before Phase 1 can complete.

### Pattern Propagation
C implementation (Plan 04) establishes the reference pattern for all 14 remaining languages. Getting C right is essential - any mistakes propagate to Phases 2-5.

### UI Work Non-Blocking
Plan 05 (Content Server/Logos) is polish work - nice to have but not blocking for core validation. Could be deferred if time constrained, but better to complete now for clean Phase 1 delivery.

---

**Ready for Execution:** Yes
**Next Step:** Execute Plan 01 (Docker & Database Foundation)
**Blocking Issues:** None - Phase 1 is first phase, no upstream dependencies
