# Roadmap: Polyglot Sudoku Solver Benchmark

## Overview

Transform an existing framework with 80+ language directories into a validated, production-ready polyglot benchmark system. Starting from infrastructure rebuild and C baseline validation, progressively implement 15 mainstream languages in serial fashion, ensuring each matches the reference algorithm exactly. Culminates with Matrix 6 enablement across all validated languages.

## Domain Expertise

None - This is a benchmarking/systems project without specialized domain requirements.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3, 4, 5, 6): Planned milestone work
- Decimal phases (X.1, X.2): Urgent insertions if needed (marked with INSERTED)

- [ ] **Phase 1: Foundation & C Baseline** - Infrastructure rebuild and reference validation
- [ ] **Phase 2: Compiled Languages Wave** - C++, Go, Rust implementation
- [ ] **Phase 3: Scripting Languages** - Python, Ruby, Perl implementation
- [ ] **Phase 4: JavaScript Ecosystem** - JavaScript, TypeScript implementation
- [ ] **Phase 5: JVM & Modern Languages** - Java, Kotlin, Scala, C#, PHP, Swift implementation
- [ ] **Phase 6: Matrix 6 Enablement** - Enable 622M iteration matrix for all languages

## Phase Details

### Phase 1: Foundation & C Baseline
**Goal**: Rebuild all infrastructure on new server and validate C as the reference baseline

**Depends on**: Nothing (first phase)

**Research**: Likely (Docker multi-language setup, Sharp image processing, Puppeteer automation)

**Research topics**:
- Docker ubuntu:24.04 setup for 15 language toolchains (optimal layering, size optimization)
- Sharp library API for PNG conversion and transformations (invert, transparent_white)
- Puppeteer viewport screenshot automation (timing, error handling)
- SQLite schema best practices for time-series benchmark data

**Plans**: TBD (estimate 4-5 plans)

**Deliverables**:
- Docker image (sudoku-benchmark:latest, ~10GB) with all 15 toolchains installed
- Content Server running on port 9001 with fixed modal positioning
- Language Detail modal edit/update working (metadata, authors, logos)
- Logo system operational (upload, URL fetch, SVG→PNG conversion, tailoring)
- Modular script architecture (Languages/common.sh with shared functions)
- SQLite database (Metrics/benchmarks.db) with full schema
- Puppeteer screenshot automation (viewport-only, after each run)
- Iteration count validation system (against ReferenceForAllMatrixRun.txt)
- Output format validation (exact match to C reference)
- C solver validated: iteration counts match reference (656, 439269, 98847, 9085, 445778)
- C baseline documented with README.md

**Success Criteria**:
- C runs matrices 1-5 successfully with exact iteration count matches
- All infrastructure tests pass (modals work, screenshots capture, database writes)
- Pattern established for remaining 14 languages

### Phase 2: Compiled Languages Wave
**Goal**: Implement C++, Go, Rust following established pattern

**Depends on**: Phase 1

**Research**: Unlikely (similar to C, standard toolchains)

**Plans**: TBD (estimate 3 plans, one per language)

**Deliverables**:
- C++ implementation with exact output/iteration match
- Go implementation with exact output/iteration match
- Rust implementation with exact output/iteration match
- Each with runMe.sh, get_compile_flags(), README.md
- Each validated against reference (matrices 1-5)
- Fastest variants identified for each
- All runs captured in SQLite with timestamps

**Success Criteria**:
- 4 languages total passing validation (C, C++, Go, Rust)
- Benchmark report shows comparative performance
- Screenshots document progress

### Phase 3: Scripting Languages
**Goal**: Implement Python, Ruby, Perl

**Depends on**: Phase 2

**Research**: Unlikely (standard interpreters, established patterns from Phase 2)

**Plans**: TBD (estimate 3 plans, one per language)

**Deliverables**:
- Python implementation with exact output/iteration match
- Ruby implementation with exact output/iteration match
- Perl implementation with exact output/iteration match
- Each with runMe.sh (no compile variants needed), README.md
- Each validated against reference (matrices 1-5)
- 5-minute timeout handling tested (especially Python on Matrix 5)
- All runs captured in SQLite

**Success Criteria**:
- 7 languages total passing validation
- Performance comparison between compiled and interpreted visible
- Timeout handling working correctly

### Phase 4: JavaScript Ecosystem
**Goal**: Implement JavaScript, TypeScript

**Depends on**: Phase 3

**Research**: Unlikely (Node.js standard, established patterns)

**Plans**: TBD (estimate 2 plans, one per language)

**Deliverables**:
- JavaScript (Node.js) implementation with exact output/iteration match
- TypeScript implementation (compiled to JS) with exact output/iteration match
- Each with runMe.sh, compilation setup for TS, README.md
- Each validated against reference (matrices 1-5)
- All runs captured in SQLite

**Success Criteria**:
- 9 languages total passing validation
- TypeScript compilation pipeline working smoothly
- JavaScript ecosystem performance characteristics captured

### Phase 5: JVM & Modern Languages
**Goal**: Implement Java, Kotlin, Scala, C#, PHP, Swift (final 6 languages)

**Depends on**: Phase 4

**Research**: Likely (.NET in Docker, Swift in non-macOS environment)

**Research topics**:
- .NET SDK setup in ubuntu:24.04 Docker (dotnet command, runtime vs SDK)
- Swift compiler in Docker (official Swift images vs manual install)
- JVM warmup considerations for fair benchmarking

**Plans**: TBD (estimate 6 plans, one per language)

**Deliverables**:
- Java implementation with exact output/iteration match (+ JVM warmup handling)
- Kotlin implementation with exact output/iteration match
- Scala implementation with exact output/iteration match
- C# implementation with exact output/iteration match
- PHP implementation with exact output/iteration match
- Swift implementation with exact output/iteration match
- Each with runMe.sh, get_compile_flags() where applicable, README.md
- Each validated against reference (matrices 1-5)
- All runs captured in SQLite

**Success Criteria**:
- 15 languages total passing validation (Tier 1 complete!)
- All languages documented and reproducible
- Database populated with comprehensive benchmark data
- Can query: fastest language, least memory, trends

### Phase 6: Matrix 6 Enablement
**Goal**: Enable the 622M iteration Matrix 6 for all validated languages

**Depends on**: Phase 5

**Research**: Unlikely (configuration change, no new integrations)

**Plans**: TBD (estimate 1-2 plans)

**Deliverables**:
- Matrix 6 execution enabled in benchmark system
- All 15 languages tested on Matrix 6 (expect some to timeout after 5 minutes)
- Timeout handling validated for long-running matrices
- Report updated to show Matrix 6 results (or timeout status)
- Screenshots capture final comprehensive benchmark state

**Success Criteria**:
- Matrix 6 runs successfully for compiled languages (C, C++, Go, Rust, Java, etc.)
- Interpreted languages timeout gracefully (Python, Ruby, Perl likely won't finish)
- Final benchmark report shows complete picture across all matrices
- Project deliverables complete

## Progress

**Execution Order:**
Phases execute sequentially: 1 → 2 → 3 → 4 → 5 → 6

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Foundation & C Baseline | 0/TBD | Not started | - |
| 2. Compiled Languages Wave | 0/TBD | Not started | - |
| 3. Scripting Languages | 0/TBD | Not started | - |
| 4. JavaScript Ecosystem | 0/TBD | Not started | - |
| 5. JVM & Modern Languages | 0/TBD | Not started | - |
| 6. Matrix 6 Enablement | 0/TBD | Not started | - |

## Notes

**Serial Language Implementation**: Within each phase, languages are implemented one at a time (serial), not in parallel. Each must be fully validated before starting the next.

**Validation Requirements**: Every language must:
- Match C output format exactly (spacing, headers, paths)
- Match C iteration counts exactly (zero tolerance)
- Complete matrices 1-5 within 5-minute timeout
- Generate valid metrics captured in SQLite
- Include README.md documentation

**Variant Strategy**: Compiled languages support variants (e.g., C: -O0, -O2, -O3, -Ofast). Fastest variant runs by default. Variants tracked in database for future analysis but not required for Phase 1-5 completion.

**Matrix 6 Strategy**: Shown in reports but not executed until Phase 6. Requires 622,577,597 iterations (extremely compute-intensive). Likely only compiled languages will complete within timeout.
