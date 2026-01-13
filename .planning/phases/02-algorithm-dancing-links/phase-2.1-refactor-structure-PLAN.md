# Phase 2.1: Refactor - Unify Directory Structure

## Objective
Restructure the project by moving all existing "Brute Force" solvers (currently in `Languages/`) into `Algorithms/BruteForce/`. This aligns the project with the new multi-algorithm architecture (e.g., `Algorithms/DLX/`) and ensures a consistent structure for benchmarking.

## Execution Context
- **Phase**: 2.1 (Refactor)
- **Goal**: Move `Languages/*` to `Algorithms/BruteForce/*` and update all references.
- **Trigger**: User request to organize brute force solvers under Algorithms.

## Tasks

### 1. Create Structure and Move Files
**Goal**: Establish the new directory layout.
- **Action**: 
  - Create `Algorithms/BruteForce/`.
  - Move all language directories from `Languages/` to `Algorithms/BruteForce/`.
  - Move `Languages/common.sh` to `Algorithms/common.sh`.
  - Move `Languages/metadata.json` to `Algorithms/metadata.json`.
  - Move `Languages/README.md` to `Algorithms/BruteForce/README.md` (or update it).
  - Remove the empty `Languages/` directory.

### 2. Update Common Script References (Bulk)
**Goal**: Ensure all 80+ `runMe.sh` scripts can find `common.sh`.
- **Action**: Use `sed` to update the source path in all `runMe.sh` files.
- **Logic**:
  - Old path: `source ../common.sh` (from `Languages/Lang/`)
  - New path: `source ../../common.sh` (from `Algorithms/BruteForce/Lang/`) relative to `Algorithms/common.sh`.
  - **Correction**: If `common.sh` is in `Algorithms/`, and script is in `Algorithms/BruteForce/Lang/`, then `../../common.sh` is correct.
  - Also update `Algorithms/DLX/C/runMe.sh` to point to the correct location (`../../common.sh`).

### 3. Update Global Runner
**Goal**: Update `runMeGlobal.sh` to handle the new structure.
- **Action**: Modify `runMeGlobal.sh`.
- **Logic**:
  - Accept an optional third argument for `ALGORITHM` (default to "BruteForce").
  - Construct paths as `Algorithms/$ALGORITHM/$LANGUAGE`.
  - Ensure backward compatibility if possible, or update documentation.

### 4. Update Infrastructure & Scripts
**Goal**: Fix tooling that relies on the `Languages/` path.
- **Action**: Update the following files:
  - `Dockerfile` (if it copies Languages)
  - `audit_docker.js`
  - `scripts/audit_metadata.js`
  - `scripts/fix_metadata.js`
  - `scripts/enrich_metadata_*.js`
  - `Metrics/SolverRunner.js` / `SolverRunner.ts`
  - `Metrics/HTMLGenerator.ts`
  - `CLAUDE.md`, `GEMINI.md`, `README.md` (documentation updates)

### 5. Verify Benchmark Run
**Goal**: Ensure the refactor didn't break the build.
- **Action**: Run `C` and `Python` benchmarks using the new structure.
- **Command**: `./runMeGlobal.sh C 1` (implying BruteForce) and potentially `./runMeGlobal.sh DLX C 1`.

## Verification
- File structure exists: `Algorithms/BruteForce/C`.
- `Languages/` directory does not exist.
- `./runMeGlobal.sh C 1` passes.
- `node scripts/audit_metadata.js` passes (finding the moved metadata).

## Output
- Refactored codebase with consistent `Algorithms/[Algorithm]/[Language]` structure.
