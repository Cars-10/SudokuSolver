---
wave: 1
depends_on: []
files_modified:
  - Metrics/HTMLGenerator.ts
  - README.md
  - Metrics/report_client.js
autonomous: true
---

# Phase 3: Metadata Alignment

## Context
The scoring engine was updated in Phase 1 to use a Weighted Geometric Mean (80% Time, 20% Memory). However, the documentation (README) and the UI (Methodology Modal, Tooltips) still describe or use the old unweighted or 4-metric formula. This plan aligns them.

## Tasks

<task>
<instruction>
Update `README.md` to accurately reflect the current scoring methodology.
- Locate the "Performance Metrics" or "Scoring" section.
- Explicitly state the formula: `Score = (Time_Ratio ^ 0.8) * (Memory_Ratio ^ 0.2)`.
- Mention that normalization is against the 'C' implementation (Baseline = 1.0).
- Remove references to Iterations or CPU in the *scoring* calculation (though they are still measured).
</instruction>
<verification_criteria>
- README.md contains "Weighted Geometric Mean".
- README.md mentions "80% Time" and "20% Memory".
</verification_criteria>
</task>

<task>
<instruction>
Update `Metrics/HTMLGenerator.ts` to fix the Methodology Modal text.
- Find the `methodModal` HTML string or similar.
- Replace the description of "Geometric Mean of 4 metrics" with the Weighted Geometric Mean explanation.
- Update the formula display (if present, e.g., LaTeX or text representation) to show the weights.
- Ensure the text explains *why* (Time is king, Memory matters).
</instruction>
<verification_criteria>
- `Metrics/HTMLGenerator.ts` no longer contains "Geometric Mean of 4 metrics".
- The generated HTML (if run) would show the correct formula.
</verification_criteria>
</task>

<task>
<instruction>
Align `Metrics/report_client.js` and `Metrics/HTMLGenerator.ts` data injection.
- Check how `data-score-breakdown` is generated in `HTMLGenerator.ts`. It should likely only include Time and Memory ratios if those are the only ones used for scoring.
- If `report_client.js` expects a specific format for the radar chart, ensure it handles the 2-metric breakdown gracefully (or update the chart to just show Time vs Memory).
- If the radar chart requires more axes for visual interest, make sure the *Total Score* calculation explanation in the tooltips remains accurate (80/20).
- Update the tooltip text for the composite score to match the formula.
</instruction>
<verification_criteria>
- Tooltips in the generated report (check source code of `report_client.js` or `HTMLGenerator.ts`) don't claim to use Iterations/CPU for the score.
</verification_criteria>
</task>

<must_haves>
- README matches code.
- UI Methodology Modal matches code.
- Tooltips match code.
</must_haves>
