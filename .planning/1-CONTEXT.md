# Phase 1 Context: Scoring Engine

## Decisions

### 1. Mathematical Foundation
- **Algorithm**: Weighted Geometric Mean.
- **Aggregation**: Scores are calculated per matrix (Time/Memory ratios vs. C baseline) and then combined. This ensures small matrices and large matrices contribute proportionally to the final score.
- **Directionality**: **Lower is Better**. A score of 1.0 represents parity with C. A score of 0.5 is twice as efficient; 2.0 is half as efficient.
- **Baseline**: The C implementation is the fixed "Gold Standard" (always 1.0).

### 2. Metric Weighting
- **Primary Metrics**: Execution Time and Peak Memory Usage.
- **Weight Bias**: Time-heavy (Default: 80% Time / 20% Memory).
- **Configuration**: Weights are stored in `benchmark_config.json`.

### 3. Failure Handling
- **Crash/Timeout**: Penalized. The math will use the `timeout limit` as the execution time value for that specific matrix.
- **Wrong Answer**: Immediate **DQ (Disqualification)**. Correctness is a prerequisite for a valid score.
- **Missing/Zero Memory**: Flagged in the UI. For scoring purposes, these should be excluded or marked as invalid rather than assuming C-parity.

### 4. Implementation Details
- **Injection**: Scores are calculated during the report generation process (build-time) and baked into the final HTML/JSON artifacts.
- **Precision**: Scores should be rendered with 3 decimal places (e.g., `1.042`).

## Deferred Ideas
- Dynamic weight adjustment in the browser (moved to a potential future UI/UX phase).
- Historical score tracking (already handled by `benchmark_history.db`, but integration with the new formula is TBD).
