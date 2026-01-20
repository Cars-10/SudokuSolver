# Phase 03 Research: Metadata Alignment

## Overview
This phase aims to align the documentation and UI text with the actual scoring implementation completed in Phase 1. The primary issue is a discrepancy between the implemented weighted geometric mean (Time/Memory) and the "Methodology" description in the generated HTML (which describes a 4-metric simple geometric mean).

## Findings

### 1. The Truth (Implementation)
**Source:** `Metrics/scoring.ts`
- **Formula:** Weighted Geometric Mean
- **Metrics:**
  - Time (80% weight)
  - Memory (20% weight)
- **Reference:** Normalized against the 'C' implementation (Baseline = 1.0).

### 2. The Discrepancy (Current UI/Docs)
**Source:** `Metrics/HTMLGenerator.ts`
- **Methodology Modal:** currently describes a simple geometric mean of 4 metrics:
  - Time
  - Memory
  - Iterations
  - CPU
- **Formula shown:** 4th root of product (Geometric Mean of 4 items).
- **Status:** **OUTDATED**. Needs to be replaced with the Weighted Geometric Mean explanation.

**Source:** `README.md`
- Needs verification to ensure it matches `Metrics/scoring.ts` exactly.

**Source:** `Metrics/report_client.js`
- Checks needed for tooltips (e.g., `data-score-breakdown`) to ensure they reflect the 2-metric weighted split, not 4.

## Plan Recommendations

### Files to Modify
1.  **`Metrics/HTMLGenerator.ts`**
    *   Target: The HTML string generation for the "Methodology" modal.
    *   Action: Rewrite the explanation to describe `Score = (TimeRatio^0.8 * MemRatio^0.2)`.
    *   Action: Update any mathematical representations (LaTeX or text).

2.  **`README.md`**
    *   Target: "Performance Metrics" or "Scoring" section.
    *   Action: Ensure it explicitly states the 80/20 split.

3.  **`Metrics/report_client.js`** (Optional/Verify)
    *   Target: Tooltips showing score calculation.
    *   Action: Verify they don't list unused metrics (Iterations/CPU) as part of the score.

### Risks
- **Regex replacing in TS files:** Be careful with escaping quotes in the large HTML strings within `HTMLGenerator.ts`.
- **Client-side Logic:** Ensure `report_client.js` doesn't have hardcoded scoring logic that conflicts with the pre-calculated scores injected by `scoring.ts`.

## Conclusion
The path is clear. We need to update the text to match the code. No new algorithms are needed, just text replacement and verification.
