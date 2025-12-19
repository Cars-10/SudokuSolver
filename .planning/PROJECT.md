# Polyglot Sudoku Solver Benchmark

## Current State (Updated: 2025-12-16)

**Status:** Brownfield - Existing framework with 80+ language directories, infrastructure present but needs validation and enhancement

**Codebase:**
- ~80 language implementation directories in `Languages/`
- 7 languages marked "complete" (C, C++, Go, Java, Python, Awk, Rust) - **require re-validation**
- Benchmark report generation (HTMLGenerator.ts, D3.js charts)
- Content Server (Node.js/Express, needs fixes)
- Docker infrastructure (needs rebuild on new server)
- 6 reference puzzle matrices (increasing difficulty)
- ~200 lines of shell scripts, ~1000 lines of TypeScript/JavaScript

**Known Issues:**
- Modal positioning broken (appears in wrong location, not near mouse click)
- Language Detail modal edit/update logic completely broken (metadata, logos, authors)
- Logo upload/paste not working
- Logo tailoring system code lost (needs re-implementation)
- Iteration counts in benchmark_history.json don't match C reference (100 vs 656)
- No algorithmic validation system in place
- No SQLite database (using scattered JSON files)
- Screenshot automation needs rebuild
- Running on new server - Docker image needs creation

## Vision for v1.0

**Complete the polyglot Sudoku solver benchmark across 15 mainstream languages (Tier 1), with full validation, metrics capture, and polished visualization system.**

This project demonstrates algorithmic consistency across programming paradigms by implementing the exact same brute-force Sudoku solver in multiple languages, measuring performance objectively, and presenting results through an automated benchmark reporting system.

The goal is not just to solve Sudoku puzzles, but to create a rigorous, reproducible framework for comparing language performance characteristics while maintaining algorithmic purity (no optimizations, same logic flow) to ensure apples-to-apples comparison.

## Problem

**Need**: A reliable, validated benchmark comparing programming language performance on identical algorithms

**Current State**:
- Existing implementations may use different algorithms (unvalidated)
- No systematic validation (iteration counts don't match reference)
- Infrastructure broken (modals, editing, logo system)
- Data scattered across JSON files (no queryable database)
- Manual screenshot workflow
- No compiler variant tracking
- Output format inconsistencies between languages

**Impact**: Cannot trust benchmark results, cannot compare languages fairly, cannot track performance over time, cannot demonstrate to others

## Success Criteria

How we know this worked:

- [ ] **15 Tier 1 languages passing validation** - C, C++, Rust, Go, Python, JavaScript, TypeScript, Java, C#, PHP, Ruby, Perl, Swift, Kotlin, Scala
- [ ] **Iteration counts match C reference exactly** for all languages, all matrices (1-5)
- [ ] **Output format matches C exactly** - spacing, headers, paths, solved puzzle
- [ ] **SQLite database operational** - all runs captured with timestamps, queryable for trends
- [ ] **Docker image built and working** - sudoku-benchmark:latest on localhost:9001
- [ ] **Content Server UI fixed** - modal positioning correct, edit/update working, logo system functional
- [ ] **Modular script architecture** - Languages/common.sh with shared functions, per-language runMe.sh
- [ ] **Automated screenshots** - Puppeteer captures viewport after each run
- [ ] **Compiler variants captured** - fastest variant identified per language, metadata tracked
- [ ] **Each language documented** - README.md explaining implementation details
- [ ] **Can answer queries**: "Which language is fastest?", "Which uses least memory?", "How has C performance changed over time?"

## Scope

### Building

**Phase 1 - Foundation & Infrastructure:**
- Docker image (sudoku-benchmark:latest, ubuntu:24.04, ~10GB with all Tier 1 toolchains)
- Content Server fixes (modal positioning, edit/update logic)
- Logo system rebuild (Sharp-based server-side processing, PNG only, SVG→PNG conversion)
- Logo tailoring system (Tailoring.json support: invert, transparent_white)
- Modular script architecture (Languages/common.sh with common functions)
- SQLite database setup (Metrics/benchmarks.db with full schema)
- Puppeteer screenshot automation (viewport-only capture after each run)
- Iteration count validation system (against ReferenceForAllMatrixRun.txt)
- Output format validation (exact match to C reference)

**Phase 2 - Language Implementation (Serial, One-by-One):**
- Restart with C language (validate baseline)
- Implement/validate 14 remaining Tier 1 languages in order:
  - C++ → Go → Rust → Python → JavaScript → TypeScript → Java → C# → PHP → Ruby → Perl → Swift → Kotlin → Scala
- Each language includes:
  - runMe.sh with modular structure
  - get_compile_flags() function for variant support
  - Exact output format matching C
  - Exact iteration count matching C reference
  - Matrices 1-5 execution (Matrix 6 shown blank in report)
  - 5-minute timeout enforcement
  - SQLite metrics capture
  - README.md documentation

**Phase 3 - Matrix 6 Enablement:**
- Enable Matrix 6 execution after all Tier 1 languages complete matrices 1-5
- 622,577,597 iterations expected (extremely compute-intensive)

**Phase 4 - Additional Tiers (Future):**
- Tier 2: 15 more popular languages
- Tier 3: 10 interesting/niche languages
- Target: 40 total languages passing validation

### Not Building

- Optimized algorithms (constraint propagation, backtracking with pruning) - future work
- Alternative solving methods (dancing links, SAT solvers) - future work
- Real-time collaborative editing - not needed
- Mobile/responsive UI - desktop-focused benchmark report
- Language implementations beyond Tier 1 in this version
- Matrix 7+ (additional difficulty levels) - 6 matrices sufficient
- Multi-server distributed execution - single-server sufficient
- CI/CD integration - manual execution acceptable for now
- Public web hosting - localhost deployment only
- Authentication/authorization - single-user system

## Context

**Background:**
- Project previously developed with 80+ language directories created
- C implementation serves as the baseline reference (brute-force algorithm)
- Reference iteration counts documented in Matrices/ReferenceForAllMatrixRun.txt
- Existing infrastructure (Content Server, HTMLGenerator, D3 charts) partially functional
- Running on new server (localhost) - requires fresh Docker setup
- Prior work established the "neon" dark theme aesthetic and MANIFESTO.md philosophy

**Reference Iteration Counts (C Baseline):**
- Matrix 1: 656 iterations
- Matrix 2: 439,269 iterations
- Matrix 3: 98,847 iterations
- Matrix 4: 9,085 iterations
- Matrix 5: 445,778 iterations
- Matrix 6: 622,577,597 iterations (not running yet - too slow for interpreted languages)

**Technical Environment:**
- macOS localhost (Darwin platform)
- Docker installed and available
- Node.js, Python3 available on host
- Languages will run inside Docker container
- External port: 9001 (Content Server)

**Algorithmic Purity Requirement:**
Every language must implement the exact same brute-force algorithm as C:
1. Read puzzle from file (9x9 grid, 0 = empty)
2. Print initial puzzle state
3. Solve using recursive backtracking:
   - Find first empty cell (0 value)
   - Try values 1-9 in order
   - For each value, check if valid (row, column, 3x3 box constraints)
   - If valid, place value and recurse
   - If recursion succeeds, return solved
   - If recursion fails, backtrack (reset to 0) and try next value
   - Count each value placement attempt as one iteration
4. Print solved puzzle state
5. Print "Solved in Iterations=N"

**Output Format Must Match C Exactly:**
```
../Matrices/1.matrix

Puzzle:
9 2 0 0 0 0 5 8 4
0 0 0 5 0 0 0 0 3
...

Puzzle:
9 2 1 6 3 7 5 8 4
6 7 4 5 1 8 9 2 3
...

Solved in Iterations=656
```

Including: trailing spaces after numbers, blank lines, path format, exact "Solved in Iterations=" text

## Constraints

- **Algorithmic Purity**: Must use exact same brute-force logic as C baseline - validated by iteration count matching exactly (zero tolerance)
- **Output Format**: Must match C output exactly - spacing, headers, paths, format
- **Timeout**: 5-minute maximum per matrix - abort run on timeout
- **Docker Image**: Single reusable image (sudoku-benchmark:latest), remove old before creating new to save disk space
- **External Port**: Must use port 9001 for Content Server
- **File Formats**: Logos must be PNG only (convert SVG→PNG)
- **Versions**: Use latest stable versions of all languages/compilers
- **Matrices**: Plan for 1-6, currently run only 1-5
- **Serial Implementation**: Complete one language fully before starting next (no parallel development)
- **Variant Strategy**: Per-language variants, fastest variant runs by default
- **Data Preservation**: Keep generating metrics.json files for debugging alongside SQLite
- **Platform**: Docker-based execution for consistency (not native macOS builds)

## Decisions Made

Key decisions from project exploration:

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Validation Method | Exact iteration count match to C reference | Simple, objective, proves algorithmic consistency without complex trace analysis |
| Jq Support | jq | Standard package, fully functional and validated. |
| Bc Support | bc (Basic Calculator) | Standard package, fully functional and validated. |
| Dash Support | dash (POSIX Shell) | Standard package, fully functional and validated. |
| Fish Support | fish shell | Brew package, fully functional and validated (Local). |
| Gnuplot Support | gnuplot | Standard package, fully functional and validated (using Gnuplot 6.0 features). |
| Jupyter Support | jupyter notebook | Executed via nbconvert, fully functional and validated. |
| SQLite Support | Node.js / SQLite Tier | Node.js implementation for SQLite tier, fully functional and validated. |
| Forth Support | GNU Forth (gforth) | Manually installed in container, fully functional and validated. |
| PostScript Support | Ghostscript (gs) | Manually installed in container, fully functional and validated. |
| Smalltalk Support | GNU Smalltalk (gst) | Manually installed in container, fully functional and validated. |
| Database Strategy | SQLite with JSON preservation | SQLite enables queries/trends, JSON kept for debugging and source of truth |
| Script Architecture | Modular (Languages/common.sh) | Reduce duplication, ensure consistency, easier maintenance across 15+ languages |
| Logo Processing | Server-side with Sharp, PNG only | Sharp is fast/reliable, PNG is universal, eliminates SVG complexity |
| Docker Strategy | Single kitchen-sink image | Simpler management, all toolchains in one place, acceptable size (~10GB) |
| Implementation Order | Serial (one language at a time) | Ensures quality, allows iteration on process, prevents half-finished implementations |
| Variant Handling | Config-driven (setupAndRunMe.sh --variant=X) | Flexible, per-language customization, trackable in database |
| Screenshot Approach | Puppeteer viewport-only after each run | Immediate visual feedback, manageable file sizes, shows progress incrementally |
| Timeout Policy | 5 minutes, abort on failure | Prevents runaway processes, enforces practical limits, forces algorithmic efficiency |
| Language Priority | Tier 1 mainstream languages first | Focus on highest-impact languages, defer niche languages to later phases |
| Matrix 6 Strategy | Show in report but don't run yet | Maintains consistency in reporting, enables later without schema changes |
| Base Image | ubuntu:24.04 | Latest LTS, good package availability, familiar ecosystem |
| Edit Flow | Server-side processing in Content Server | Centralized logic, consistent processing, leverages existing Express infrastructure |

## Open Questions

Things to figure out during execution:

- [ ] Which languages require special JVM warmup or multi-run averaging? (Java, Kotlin, Scala)
- [ ] How to handle Python 2 vs Python 3 decision? (probably just Python 3.x latest)
- [ ] Should Swift run in Docker or require macOS native? (Docker preferred for consistency)
- [ ] What's the actual Docker image size with all 15 toolchains? (estimate 8-12GB, might be more)
- [ ] Do we need separate timeout values per matrix? (Matrix 5 might need more than Matrix 1)
- [ ] Should the database track compiler/runtime version automatically or rely on manual input?
- [ ] How to handle languages with multiple implementation styles? (e.g., Java imperative vs functional)
- [ ] Should we run warmup iterations for JIT-compiled languages to get stable timings?
- [ ] What's the best way to test Docker image before committing to full build?
- [ ] Should screenshots be versioned/timestamped or overwrite previous?

---
*Initialized: 2025-12-16*
