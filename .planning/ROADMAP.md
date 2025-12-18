# Roadmap: Polyglot Sudoku Solver Benchmark

## Overview

Transform an existing framework with 80+ language directories into a validated, production-ready polyglot benchmark system. Starting from infrastructure rebuild and C baseline validation, progressively implement 15 mainstream languages in serial fashion, ensuring each matches the reference algorithm exactly. Culminates with Matrix 6 enablement across all validated languages.

## Domain Expertise

None - This is a benchmarking/systems project without specialized domain requirements.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3, 4, 5, 6): Planned milestone work
- Decimal phases (X.1, X.2): Urgent insertions if needed (marked with INSERTED)

- [x] **Phase 1: Foundation & C Baseline** - Infrastructure rebuild and reference validation
- [x] **Phase 1.5: UI Polish & Fixes** - INSERTED: Fix modal display, logo serving, edit workflow
- [x] **Phase 1.5.1: Screensaver Issues** - INSERTED: Fix screensaver bounce on auto-trigger
- [x] **Phase 1.5.2: Table Highlighting** - INSERTED: Fix table row/cell highlighting text shift
- [x] **Phase 1.5.3: Comprehensive Scoring System** - INSERTED: Create scoring calculation using all collected metrics
- [x] **Phase 1.5.4: UI Button Handling & Language Filtering** - INSERTED: Manage language lock-in and skip already-run languages
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

### Phase 1.5: UI Polish & Fixes (INSERTED)
**Goal**: Fix modal interactions and display issues

**Depends on**: Phase 1

**Research**: None (known issues, direct fixes)

**Plans**: 4 plans
1. Fix modal CSS & behavior (floating overlay, backdrop, animations)
2. Fix logo serving & display (add /logos route, verify images load)
3. Test & polish edit workflow (metadata save, logo upload, persistence)
4. General UI polish (accessibility, responsiveness, performance)

**Deliverables**:
- Language detail modal displays as floating overlay (not top-left inline)
- Dark backdrop with blur effect, smooth animations
- All 44 language logos display correctly in report
- Logo upload and URL fetch working in modal
- Edit workflow fully functional (save, persist, regenerate)
- Keyboard navigation (ESC, Tab, Enter)
- Mobile responsive
- No console errors

**Success Criteria**:
- Modal opens centered with proper z-index layering
- No jerky browser movements when opening modal
- All logos load successfully (verified with browser dev tools)
- Can edit language metadata and see changes in regenerated report
- Professional, polished user experience

### Phase 1.5.1: Screensaver Issues (INSERTED)
**Goal**: Fix screensaver bounce on auto-trigger

**Depends on**: Phase 1.5

**Research**: None (CSS/JS fix using established patterns)

**Plans**: 1 plan
1. Fix bounce + verify (scroll position management, smooth transitions)

**Deliverables**:
- Scroll position saved before screensaver starts
- Page scrolled to top before fullscreen mode applied
- Scroll position restored after screensaver exits
- Smooth transitions for both auto-trigger and manual trigger

**Success Criteria**:
- Auto-triggered screensaver transitions smoothly without bounce
- Manual pill triggers continue to work smoothly
- Scroll position preserved and restored correctly

### Phase 1.5.2: Table Highlighting (INSERTED)
**Goal**: Fix table row and cell highlighting causing text to shift

**Depends on**: Phase 1.5

**Research**: None (CSS/JS work using established patterns)

**Plans**: 2 plans
1. CSS Fix + Expandable Infrastructure (fix hover, add chevron, build expanded rows)
2. Enhanced Table Columns (add compiler version and memory peak columns)

**Deliverables**:
- Hover effects without layout shift (transparent borders, no scale transform)
- Expandable rows with chevron indicator (▶ → ▼)
- Expanded content with System | Compilation | Results sections
- Compiler version column in main table
- Memory peak column in main table

**Success Criteria**:
- Hovering over rows produces visual feedback without text movement
- Clicking rows expands/collapses with chevron rotation
- Expanded content shows organized metrics data
- New columns sortable and properly formatted

### Phase 1.5.3: Comprehensive Scoring System (INSERTED)
**Goal**: Create a comprehensive scoring calculation using all collected benchmark metrics

**Depends on**: Phase 1.5.2

**Research**: Yes - scoring methodology research required

**Proposed Scoring Methods**:

1. **Weighted Multi-Criteria Score (Recommended)**
   - Normalize each metric to 0-100 scale
   - Apply configurable weights (e.g., Time: 40%, Memory: 30%, Validation: 20%, Iterations: 10%)
   - **Why important**: Allows users to prioritize what matters most for their use case
   - Formula: `Score = Σ(weight_i × normalized_metric_i)`

2. **Geometric Mean (Industry Standard)**
   - Calculate geometric mean across all metrics
   - Used by SPEC benchmarks, Geekbench
   - **Why important**: Prevents one exceptional metric from dominating; penalizes imbalance
   - Formula: `Score = (∏ normalized_metrics)^(1/n)`

3. **Percentile Ranking**
   - Rank each language against the field per metric
   - Average percentile across all metrics
   - **Why important**: Intuitive (0-100), relative positioning, resistant to outliers
   - Display: "Faster than 85% of languages"

4. **Tier Classification (A/B/C/D/F)**
   - Group languages into performance tiers based on standard deviations
   - A: >1σ above mean, B: 0-1σ above, C: 0-1σ below, D: 1-2σ below, F: >2σ below
   - **Why important**: Quick visual assessment, familiar grading system

5. **Pareto Efficiency Score**
   - Identify non-dominated solutions (no other language better in ALL metrics)
   - Mark Pareto-optimal languages with special indicator
   - **Why important**: Highlights "best tradeoff" languages objectively

**Available Metrics for Scoring**:
- `time_1` through `time_5`: Execution time per matrix (seconds)
- `memory_1` through `memory_5`: Memory usage per matrix (MB)
- `iterations_1` through `iterations_5`: Algorithm iterations per matrix
- `validated`: Boolean validation status
- `compiler_version`: Toolchain info (categorical, not scored)
- `memory_peak`: Peak memory across all matrices

**Plans**: TBD (estimate 2-3 plans)
1. Research & finalize scoring methodology
2. Implement scoring calculation in report generator
3. Add score display to UI (column, badges, tooltips)

**Deliverables**:
- Scoring algorithm implemented in HTMLGenerator.ts
- New "Score" column in benchmark table (sortable)
- Score breakdown tooltip on hover
- Visual tier badges (A/B/C/D/F or color-coded)
- Documentation explaining scoring methodology

**Success Criteria**:
- Each language has a calculated score displayed
- Score is sortable in table
- Methodology is documented and transparent
- Users can understand why a language scored as it did

### Phase 1.5.4: UI Button Handling & Language Filtering (INSERTED)
**Goal**: Manage UI button handling and filtering - track which languages have been run and allow locking them in the Languages pulldown so benchmark runs skip locked languages

**Depends on**: Phase 1.5.3

**Research**: None (UI/state management using established patterns)

**Plans**: TBD (run /gsd:plan-phase 1.5.4 to break down)

**Deliverables**:
- Language "lock-in" mechanism in the Languages dropdown
- Visual indicator for locked/completed languages
- Benchmark runner respects locked status (skips locked languages)
- Persistent state for locked languages (survives page refresh)
- UI buttons properly wired for filtering actions

**Success Criteria**:
- Can lock a language after running its benchmark
- Locked languages show distinct visual state in dropdown
- Running "all benchmarks" skips locked languages
- Lock state persists across sessions
- Clean button handling without state conflicts

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
