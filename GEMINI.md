# Sudoku Solver (SS) - The "Red Pill" Benchmark

## Project Overview

**Sudoku Solver (SS)** is a polyglot benchmarking project designed to compare the performance of 15+ "Tier 1" programming languages (and potentially 80+ total) on an identical task: **Brute-force Sudoku solving**.

**Core Philosophy:**
*   **Algorithmic Purity:** Every implementation must use the *exact same* recursive backtracking algorithm. No optimizations (DLX, constraint propagation, etc.) are allowed unless explicitly marked as a variant.
*   **"Red Pill" Aesthetic:** The project adheres to a "Neon" dark mode visual theme.
*   **Pragmatism:** Simple, robust solutions. Raw SQL/JSON over complex frameworks.
*   **Visibility:** "If it runs, we should see how it runs." (Metrics, logs, UI).

## Directory Structure

*   **`Algorithms/BruteForce/`**: Contains the source code for each language implementation (e.g., `Algorithms/BruteForce/C/`, `Algorithms/BruteForce/Python/`).
    *   **`common.sh`**: Shared shell functions for metrics, timing, and execution.
    *   **`README.md`**: Detailed implementation guide and patterns.
*   **`Matrices/`**: Input puzzle files (e.g., `1.matrix` to `6.matrix`) and reference outputs.
*   **`Metrics/`**: Tools for report generation and data analysis.
    *   **`HTMLGenerator.ts`**: Generates the single-file `benchmark_report.html`.
    *   **`server/`**: A Node.js Content Server for UI interactions.
*   **`runMeGlobal.sh`**: The main entry point script for running benchmarks.
*   **`.planning/`**: Project management docs (`PROJECT.md`, `MANIFESTO.md`).

## Getting Started

### Prerequisites
*   **macOS/Linux** (Darwin is the current primary dev environment).
*   **Docker** (Strongly recommended for consistent toolchains).
*   **Node.js** (For the Content Server and Report Generator).

### Running a Benchmark

To run a specific language against a specific matrix:

```bash
# Usage: ./runMeGlobal.sh [Language] [MatrixSpec]
./runMeGlobal.sh C 1        # Run C solver on 1.matrix
./runMeGlobal.sh Python 1-3 # Run Python solver on 1.matrix through 3.matrix
```

### Viewing Results

The benchmarking process generates:
1.  **`metrics.json`**: Inside the language directory.
2.  **`benchmark_report.html`**: A visual dashboard (needs generation via `Metrics/HTMLGenerator.ts`).

## Development Standards

### Adding/Fixing a Language

1.  **The "Matrix 1" Rule:** Your first goal is *always* to get `1.matrix` to run, solve correctly, and match the reference iteration count (656).
2.  **Algorithmic Consistency:** You must implement the standard recursive backtracking algorithm.
    *   **Validation:** Iteration counts must match the C reference *exactly*.
3.  **Output Format:** Output must match the C implementation byte-for-byte (headers, spacing, "Solved in Iterations=...").
4.  **Scripting:**
    *   Use `Algorithms/BruteForce/common.sh` for shared logic.
    *   Create `setupAndRunMe.sh` (or `runMe.sh`) in the language directory.
    *   Handle compilation (if needed) and execution.

### Docker Strategy
*   Prefer a shared base image (`sudoku-benchmark:latest`) over unique per-language images.
*   Map internal ports to external ports starting at **9900**.

## Key Files to Know

*   **`MANIFESTO.md`**: The philosophical guide. Read this to understand the "Why".
*   **`.planning/PROJECT.md`**: Current project status, roadmap, and known issues.
*   **`Algorithms/BruteForce/C/runMe.sh`**: The reference implementation for scripting.
*   **`Algorithms/BruteForce/C/Sudoku.c`**: The reference implementation for the algorithm.

## Known Issues (as of Dec 2025)
*   Content Server UI (modals, editing) is partially broken.
*   Iteration counts in history might not match the fresh C reference.
*   Docker infrastructure needs rebuilding on the new server.

## Cheat Sheet

*   **SS**: Sudoku Solver
*   **BR**: Benchmark Report
*   **CS**: Content Server
*   **TDD**: "Test Driven Development" (in this context: getting Matrix 1 to pass).
