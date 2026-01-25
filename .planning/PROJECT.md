# Sudoku Solver Benchmark

## What This Is
A polyglot benchmarking project that compares the performance of 70+ programming languages on a brute-force Sudoku solving task. It provides a visual dashboard for analyzing metrics like execution time, memory usage, and iteration consistency.

## Core Value
Providing a fair, transparent, and visually engaging comparison of programming language performance using a standardized algorithmic implementation.

## Current Milestone: v3.0 Quality & Insights

**Goal:** Ensure algorithmic correctness through enhanced validation, determine optimal scoring methodology through data analysis, and provide richer insights through improved visualizations.

**Target features:**
- Enhanced test framework with iteration count validation (flag invalid implementations)
- Data-driven scoring methodology analysis (distribution, correlation, outlier detection)
- UI fixes (algorithm dropdown sorting, label updates, visual bugs)
- New chart types (scatter plots, histograms, heatmaps) for deeper insights

## Requirements

### Validated
- ✓ Standard recursive backtracking algorithm across 70+ languages — existing
- ✓ Automated benchmarking suite with iteration count verification — existing
- ✓ Single-file HTML report generation with data visualization — existing
- ✓ Language metadata and author attribution system — existing
- ✓ Matrix-based testing (1.matrix to 6.matrix) — existing
- ✓ Weighted geometric mean scoring (80/20) — v1
- ✓ C baseline normalization — v1
- ✓ Cleaned up UI (Run buttons removed) — v1
- ✓ Robust Matrix Race fullscreen (screenfull) — v1
- ✓ Aligned metadata & documentation — v1
- ✓ Wikipedia metadata scraper for automated data extraction — v2
- ✓ Enhanced language descriptions with encyclopedic content — v2
- ✓ Author attribution with biographical metadata — v2
- ✓ Media cleanup automation — v2
- ✓ Execution-time validation with iteration count checking — v3.0
- ✓ Invalid implementation flagging with warning badges — v3.0
- ✓ Scoring methodology sensitivity analysis (4 weight scenarios) — v3.0
- ✓ Statistical analysis with IQR outlier detection and R² correlation — v3.0
- ✓ Algorithm dropdown alphabetical sorting — v3.0
- ✓ Matrix Race fullscreen exit bug fix — v3.0
- ✓ Scatter plot visualization (Time vs Memory with log scaling) — v3.0
- ✓ Heatmap visualization (Language x Matrix performance grid) — v3.0
- ✓ Distribution histogram with percentile markers — v3.0
- ✓ Interactive browser-based Sudoku solver with animation — v3.0
- ✓ Validation diagnostics modal with iteration mismatch details — v3.0

### Active (Future)
- [ ] Define algorithmic fingerprinting scoring (based on iteration counts)
- [ ] Implement "Zen Mode" toggle for simplified viewing

### Future
- [ ] Define algorithmic fingerprinting scoring (based on iteration counts)
- [ ] Implement "Zen Mode" toggle for simplified viewing
- [ ] Non-brute-force optimizations (DLX, constraint propagation) — must maintain algorithmic purity for benchmark fairness
- [ ] Remote/Docker execution — focusing on local performance and UI refinement

## Context
- The project uses a Neon dark mode visual theme with Matrix-inspired aesthetics.
- Solvers are implemented in a wide variety of "Tier 1" and esoteric languages.
- Performance data is stored in SQLite and JSON formats.
- **Shipped v1**: Scoring engine, UI cleanup, and metadata alignment complete (2026-01-20).
- **Shipped v2**: Wikipedia-powered metadata automation with enhanced language descriptions and author attribution (2026-01-20).
- **Shipped v3.0**: Validation infrastructure, statistical analysis, advanced D3.js visualizations, and interactive solver (2026-01-25).
- Python tooling pipeline established for metadata extraction and enrichment.
- TypeScript analysis modules provide IQR-based outlier detection, R² correlation, and sensitivity analysis.
- Interactive solver features 3D CSS grid with neon styling, glitch effects, and playback controls.
- Current codebase: ~600K LOC across 1,760+ files (TypeScript, JavaScript, Python, 88+ solver languages).

## Constraints
- **Algorithmic**: Standard recursive backtracking — Required for fair comparison across all languages.
- **Environment**: macOS (Darwin) — Primary development and execution environment.
- **Consistency**: Iteration counts must match C reference exactly (656 for Matrix 1).

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Local Execution Focus | User specified ignoring docker commands for now to focus on local refinements. | — Pending |
| Scored Ranking | Moving from raw metrics to a weighted score to provide a "best" ranking. | ✓ v1 |
| 80/20 Scoring | Weighted Time (80%) and Memory (20%) for score calculation. | ✓ v1 |
| Wikipedia Integration | Automate metadata extraction using Wikipedia API to maintain consistency and reduce manual effort. | ✓ v2 |
| Media Cleanup | Remove obsolete timestamped images, keeping only curated/logo images. | ✓ v2 |
| Execution-time Validation | Validate during benchmark execution (not report generation) for immediate feedback. | ✓ v3.0 |
| Algorithm-specific Tolerance | BruteForce requires exact iteration match; DLX/CP allow +/-1 tolerance for non-deterministic choices. | ✓ v3.0 |
| IQR Outlier Detection | Use IQR method over Z-score for robustness to skewed distributions (6 orders of magnitude variation). | ✓ v3.0 |
| Interactive Solver Modal | Launch from INFO dropdown as modal popup instead of inline tab for better focus and immersive experience. | ✓ v3.0 |

---
*Last updated: 2026-01-25 after v3.0 milestone completion*