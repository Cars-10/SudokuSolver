# Phase 2, Plan 1: DLX Algorithm Scaffolding

## Objective
Establish the directory structure and foundational C code for the new Dancing Links (DLX) algorithm implementation. This plan focuses on creating the skeleton of the project, including header files for data structures and a build script, without implementing the core search logic. The goal is to have a compilable, non-functional starting point.

## Execution Context
- **Phase**: 2 (Algorithm - Dancing Links)
- **Goal**: Create the C project structure for the DLX solver.
- **Parent Workflow**: /gsd:plan-phase 2

## Tasks

### 1. Create Directory Structure
**Goal**: Create the necessary directories for the C implementation of the DLX algorithm.
- **Action**: Create the directory `Algorithms/DLX/C`. This follows the `Algorithms/[Type]/[Language]` convention decided upon in `PROJECT.md`.

### 2. Create Build Script
**Goal**: Create a `runMe.sh` script to handle compilation.
- **Action**: Create `Algorithms/DLX/C/runMe.sh`.
- **Details**:
  - The script should be based on `Languages/C/runMe.sh`.
  - It must correctly locate and source the `../../Languages/common.sh` file.
  - The `compile` function should be adapted to compile `dlx_core.c` and `dlx_sudoku.c` into a binary named `dlx_solver`.
  - For now, the `main` function in the script can be a placeholder that just runs `compile`.

### 3. Define DLX Data Structures
**Goal**: Create a header file defining the core data structures for Algorithm X.
- **Action**: Create `Algorithms/DLX/C/dlx.h`.
- **Details**:
  - Define a `DlxNode` struct containing pointers for up, down, left, right, and a pointer to its column header.
  - Define a `DlxColumn` struct which inherits from `DlxNode` and adds a `size` (number of nodes in the column) and a `name`.
  - The root of the DLX matrix will be a `DlxColumn` object.

### 4. Create Core Stubs
**Goal**: Create a C file with stub functions for the DLX implementation.
- **Action**: Create `Algorithms/DLX/C/dlx_core.c`.
- **Details**:
  - Include `dlx.h`.
  - Create placeholder functions for `dlx_cover_column(DlxColumn* c)`, `dlx_uncover_column(DlxColumn* c)`, and `dlx_search(DlxColumn* root, int k, int* solution)`. These functions should be empty or contain a simple print statement for now.

### 5. Create Main Entrypoint Stub
**Goal**: Create the main C file that will eventually house the Sudoku-specific logic.
- **Action**: Create `Algorithms/DLX/C/dlx_sudoku.c`.
- **Details**:
  - Include `dlx.h`.
  - Create a `main` function that does nothing but return 0. This ensures the project can be compiled.

## Verification
- Run the `compile` function within `Algorithms/DLX/C/runMe.sh`.
- **Success Criteria**: The compilation must succeed without errors, producing an executable file named `dlx_solver`. The executable does not need to run or produce any output at this stage.

## Output
- The directory `Algorithms/DLX/C/` containing:
  - `runMe.sh`
  - `dlx.h`
  - `dlx_core.c`
  - `dlx_sudoku.c`
- A compiled binary `dlx_solver`.
