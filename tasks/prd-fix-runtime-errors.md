# PRD: Fix Runtime Error Languages

## Introduction

Several language implementations in the benchmark suite have runtime errors preventing them from producing correct results. Investigation reveals these are primarily `runMe.sh` configuration issues where the `SOLVER_BINARY` variable is not set or the execution method doesn't match the language's requirements. This PRD covers fixing these issues to get all implementations passing Matrix 1 with 656 iterations.

## Goals

- Fix all languages with runtime/code errors to produce correct output
- Ensure all fixed languages report 656 iterations for Matrix 1
- Maintain consistent `runMe.sh` patterns across all languages
- Document AppleScript as local-only (macOS requirement)

## Languages to Fix (by complexity)

### Tier 1 - Simple runMe.sh Fixes
| Language | Issue | Fix |
|----------|-------|-----|
| Make | Missing SOLVER_BINARY, wrong approach | Create wrapper script for make invocation |
| F_Sharp | Missing SOLVER_BINARY for dotnet run | Set SOLVER_BINARY to wrapper calling `dotnet run` |
| Scala | Missing SOLVER_BINARY, scala CLI changed | Use `scala -cp . Sudoku` or compile to JAR |

### Tier 2 - Environment Issues (Docker)
| Language | Issue | Fix |
|----------|-------|-----|
| CoffeeScript | `coffee` command not found | Install coffeescript in Docker or document |
| Tcsh | `tcsh` not found | Install tcsh in Docker or document |

### Tier 3 - Performance Issue
| Language | Issue | Fix |
|----------|-------|-----|
| Dc | Timeout (algorithm too slow) | Optimize dc implementation or increase timeout |

### Documentation Only
| Language | Issue | Action |
|----------|-------|--------|
| AppleScript | Requires macOS | Mark as local-only in README |

## User Stories

### US-001: Fix Make runMe.sh
**Description:** As a developer, I want the Make implementation to run correctly so it produces 656 iterations for Matrix 1.

**Acceptance Criteria:**
- [ ] Create `run_make.sh` wrapper that converts matrix path to Make variables
- [ ] Update `runMe.sh` to set `SOLVER_BINARY="./run_make.sh"`
- [ ] Wrapper calls `make -f Makefile solve MATRIX=<path>`
- [ ] Matrix 1 produces 656 iterations in Docker
- [ ] Output includes "Solved in Iterations=656"

---

### US-002: Fix F_Sharp runMe.sh
**Description:** As a developer, I want the F# implementation to run correctly so it produces 656 iterations for Matrix 1.

**Acceptance Criteria:**
- [ ] Create `run_fsharp.sh` wrapper that calls `dotnet run -- <matrix_path>`
- [ ] Update `runMe.sh` to set `SOLVER_BINARY="./run_fsharp.sh"`
- [ ] Matrix 1 produces 656 iterations in Docker
- [ ] Output includes "Solved in Iterations=656"

---

### US-003: Fix Scala runMe.sh
**Description:** As a developer, I want the Scala implementation to run correctly so it produces 656 iterations for Matrix 1.

**Acceptance Criteria:**
- [ ] Determine correct Scala 3 execution method (scala-cli or java -cp)
- [ ] Create `run_scala.sh` wrapper if needed
- [ ] Update `runMe.sh` to set appropriate `SOLVER_BINARY`
- [ ] Matrix 1 produces 656 iterations in Docker
- [ ] Output includes "Solved in Iterations=656"

---

### US-004: Fix CoffeeScript environment
**Description:** As a developer, I want CoffeeScript to be runnable in Docker so it can be benchmarked.

**Acceptance Criteria:**
- [ ] Verify `coffee` command exists in Docker (install if missing: `npm install -g coffeescript`)
- [ ] Update `runMe.sh` to set `SOLVER_BINARY="coffee Sudoku.coffee"`
- [ ] Matrix 1 produces 656 iterations in Docker
- [ ] Output includes "Solved in Iterations=656"

---

### US-005: Fix Tcsh environment
**Description:** As a developer, I want Tcsh to be runnable in Docker so it can be benchmarked.

**Acceptance Criteria:**
- [ ] Verify `tcsh` command exists in Docker (install if missing: `apt install tcsh`)
- [ ] Update `runMe.sh` to set `SOLVER_BINARY="tcsh Sudoku.tcsh"`
- [ ] Matrix 1 produces 656 iterations in Docker
- [ ] Output includes "Solved in Iterations=656"

---

### US-006: Fix or document Dc performance
**Description:** As a developer, I want the Dc implementation to either complete within timeout or be documented as too slow.

**Acceptance Criteria:**
- [ ] Test Dc on Matrix 1 with extended timeout (600s)
- [ ] If completes: verify 656 iterations, adjust TIMEOUT_SECONDS in runMe.sh
- [ ] If still times out: document in README that Dc is too slow for larger matrices
- [ ] Update metrics.json status appropriately

---

### US-007: Document AppleScript as local-only
**Description:** As a developer, I want AppleScript documented as macOS-only so users understand it can't run in Docker.

**Acceptance Criteria:**
- [ ] Add note to `Languages/AppleScript/README.md` explaining macOS requirement
- [ ] Update `runMe.sh` to check for Darwin OS and skip gracefully on Linux
- [ ] runMe.sh should output clear message: "AppleScript requires macOS"
- [ ] Add AppleScript to "local-only" list in main documentation if one exists

## Functional Requirements

- FR-1: Each language's `runMe.sh` must set `SOLVER_BINARY` to an executable command
- FR-2: Wrapper scripts must accept matrix path as first argument
- FR-3: All fixed languages must produce exactly 656 iterations for Matrix 1
- FR-4: Output format must match specification (include "Solved in Iterations=NNN")
- FR-5: Scripts must work in Docker `sudoku-benchmark` container
- FR-6: Environment checks should fail gracefully with clear error messages

## Non-Goals

- Not fixing algorithm bugs (iteration count mismatches) - separate PRD
- Not optimizing slow implementations beyond making them complete
- Not adding new language implementations
- Not modifying the Docker image (document missing tools instead)

## Technical Considerations

- Wrapper script pattern: Create `run_<lang>.sh` that translates common.sh's `$SOLVER_BINARY <matrix_path>` call
- F# uses `dotnet run -- <args>` syntax for passing arguments
- Scala 3 changed CLI - may need `scala-cli run .` or `java -cp . Sudoku`
- Make needs matrix data converted to variables before invocation
- Docker container is based on Ubuntu - use apt for missing packages

## Success Metrics

- All 6 runtime error languages either pass Matrix 1 or have clear documentation
- Zero "error" status in metrics.json for fixable languages
- All fixed languages report exactly 656 iterations

## Open Questions

1. Should we modify the Docker image to add missing tools, or just document requirements?
2. Is Dc performance issue worth optimizing, or should we accept it's too slow?
3. Should we add a "local-only" flag to benchmark_config.json for AppleScript?
