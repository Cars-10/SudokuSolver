# Pitfalls Research: Benchmark Quality Improvements

**Domain:** Multi-language benchmark suite enhancements (validation, scoring, visualization)
**Researched:** 2026-01-23
**Confidence:** HIGH

**Context:** Adding correctness validation, revised scoring methodology, and advanced visualizations to an existing Sudoku benchmark with 70+ language implementations and 2 years of historical data.

## Critical Pitfalls

### Pitfall 1: Overly Strict Iteration Count Validation

**What goes wrong:**
Validation rejects correct implementations due to platform-specific floating point behavior, compiler optimizations, or legitimate algorithm variations. With 70+ implementations across different architectures, false positives can invalidate large portions of the benchmark suite.

**Why it happens:**
The temptation to use exact integer equality (`iterations === C_Baselines[algorithm][matrix]`) seems foolproof for iteration counts. However, even deterministic algorithms can produce different iteration counts due to:
- Compiler optimizations reordering operations
- Different evaluation order in functional languages
- Platform-specific integer overflow behavior
- Legitimate algorithm variations that are still "correct"

**How to avoid:**
1. **Define correctness boundaries:** For BruteForce algorithm, exact match is expected (656, 439269, 98847, etc.). For algorithms with heuristics (CP, DLX), allow variance.
2. **Separate validation levels:**
   - EXACT: Iteration count must match baseline exactly (BruteForce)
   - WITHIN_RANGE: Iteration count within acceptable percentage (CP/DLX if needed)
   - SOLUTION_VALID: Correct solution regardless of iteration count (fallback)
3. **Validation warnings vs. errors:** Mismatch should be flagged for investigation but not automatically mark implementation as "invalid" in visualizations.

**Warning signs:**
- More than 5% of previously working implementations suddenly flagged as invalid
- All implementations of a specific language family (e.g., all ML-family languages) flagged together
- Validation failures clustered by platform (macOS vs Linux vs Docker)

**Phase to address:**
Phase 1 (Validation Infrastructure) — Implement tiered validation levels from the start to avoid mass false positives.

---

### Pitfall 2: Breaking Historical Comparability with Scoring Changes

**What goes wrong:**
Changing the composite score formula (currently `0.8 * time + 0.2 * memory`) invalidates all historical rankings, making trend analysis impossible and destroying 2 years of benchmark history value.

**Why it happens:**
Discovering that the current formula undervalues memory efficiency or doesn't properly normalize across algorithms creates pressure to "fix" it immediately. The World Benchmarking Alliance 2026 scoring changes demonstrate the complexity: they moved to 0-100 normalization and binary scoring, but this makes historical comparisons require complex transformation matrices.

**How to avoid:**
1. **Versioned scoring:** Introduce `scoreV2` alongside existing `score` field. Never replace, always append.
2. **Migration strategy:**
   ```typescript
   interface SolverMetrics {
     score?: number;      // Original formula (0.8*time + 0.2*mem)
     scoreV2?: number;    // New formula if changed
     scoreVersion?: string; // "v1" | "v2" for clarity
   }
   ```
3. **Historical recalculation:** Run new scoring formula against ALL historical metrics.json files to maintain comparability.
4. **Dual visualization:** Show both old and new scores during transition period (6-12 months).
5. **Document formula changes:** Include rationale, date, and conversion formula in LanguagesMetadata.ts.

**Warning signs:**
- Historical trend charts show discontinuity at formula change date
- Languages that were top performers suddenly rank poorly (or vice versa)
- No clear migration path for existing dashboards/reports
- Inability to answer "How has X improved over time?" across the change boundary

**Phase to address:**
Phase 2 (Scoring Analysis) — Design versioned scoring system BEFORE implementing any changes. Recovery cost if done wrong: HIGH (requires manual historical data reconstruction).

---

### Pitfall 3: Misleading Visualization Design

**What goes wrong:**
New charts (scatter plots, heatmaps, histograms) that use truncated axes, inconsistent scales, or inappropriate chart types create false narratives about language performance differences. With 70+ languages, visualization choices heavily influence perception.

**Why it happens:**
Pressure to show "interesting" results leads to cherry-picking scales that emphasize differences. As Harvard Business School's bad data visualization research shows, truncating the y-axis is the most common mistake, making small differences appear dramatic. With benchmark data spanning 6 orders of magnitude (Matrix 1: ~1ms, Matrix 6: ~10,000ms), inappropriate scaling is inevitable without careful design.

**How to avoid:**
1. **Logarithmic scales for time charts:** Performance spans 6 orders of magnitude. Linear scales compress fast languages into invisibility or require truncation.
2. **Consistent baseline rules:**
   - Always start time axes at 0 or use log scale
   - Always start memory axes at 0
   - Never truncate to make differences look larger
3. **Explicit normalization disclosure:** If showing normalized scores, always label "normalized to C baseline" clearly.
4. **Side-by-side comparisons:** When comparing algorithms (BruteForce vs DLX), use separate charts or clearly separated sections to avoid visual confusion.
5. **Color blindness consideration:** Use ColorBrewer palettes, avoid red/green only distinctions.
6. **Reference implementation markers:** Always show C baseline as reference line/point.

**Warning signs:**
- User feedback: "Language X looks way faster in the chart than it actually is"
- Scatter plots where clustering isn't visible due to scale compression
- Heatmaps where 90% of cells are the same color despite significant data variation
- Charts that require >30 seconds to understand what they're showing

**Phase to address:**
Phase 3 (Visualization Implementation) — Include visualization review checklist referencing Thoughtspot's misleading graphs guide.

---

### Pitfall 4: Validation Creating Noise in Reports

**What goes wrong:**
Every validation failure generates flags, warnings, and error states that clutter the report. With 70 languages × 3 algorithms × 6 matrices = 1,260 potential test cases, even 5% failure rate = 63 warnings overwhelming the useful signal.

**Why it happens:**
The impulse to "show all the data" means every mismatch, timeout, or compilation error gets equal visual weight. The existing `benchmark_issues.json` already demonstrates this: it captures errors but doesn't prioritize or contextualize them. Android's benchmark regression detection found they needed high confidence thresholds for presubmit because benchmark instability creates false positives.

**How to avoid:**
1. **Tiered visibility:**
   - CRITICAL: Algorithm fundamentally wrong (wrong solution, not iteration mismatch)
   - WARNING: Iteration mismatch but solution correct
   - INFO: Platform-specific variation, expected
2. **Aggregate invalid implementations:** Instead of 63 individual warnings, show "12 languages failed validation (expand for details)".
3. **Suppress expected failures:** If F# on macOS is known to have iteration count variance, document and suppress unless regression detected.
4. **Trend-based alerting:** Only flag if validation state *changed* (previously passed, now fails) rather than persistent known issues.
5. **Separate reports:** Main report shows valid implementations. Validation issues in separate "Diagnostics" section.

**Warning signs:**
- Main benchmark report has more red flags than actual data
- Valid data is hard to find among error messages
- Users ignore validation warnings because there are too many
- "Boy who cried wolf" effect: real regressions hidden in noise

**Phase to address:**
Phase 1 (Validation Infrastructure) — Design validation result hierarchy from start. Recovery cost if done wrong: MEDIUM (requires UI refactoring).

---

### Pitfall 5: Iteration Count Validation Without Solution Validation

**What goes wrong:**
Implementation passes iteration count check (656 iterations) but produces *wrong solution*. This false negative allows fundamentally broken implementations to appear valid. Conversely, implementation produces correct solution but with different iteration count (due to legitimate variation) and gets flagged invalid.

**Why it happens:**
Iteration count is easy to extract from output ("Solved in Iterations=656"), while solution validation requires parsing the 9×9 grid and checking Sudoku constraints. The temptation is to ship iteration-only validation first. This is backwards: solution correctness is the actual requirement; iteration count is a fingerprint for algorithm verification.

**How to avoid:**
1. **Solution-first validation:**
   ```typescript
   enum ValidationLevel {
     SOLUTION_INVALID,     // Wrong answer - critical failure
     SOLUTION_CORRECT,     // Right answer - always acceptable
     ITERATIONS_MATCH,     // Correct + matching iterations - gold standard
     ITERATIONS_MISMATCH   // Correct + different iterations - investigate
   }
   ```
2. **Parse and validate solved grid:** Extract final puzzle state, verify all rows/columns/boxes contain 1-9.
3. **Verify against input:** Ensure all fixed clues from input puzzle remain unchanged.
4. **Reference output comparison:** For each matrix, store reference solution (from C implementation) and compare grid equality.
5. **Multi-stage validation:** Solution validation is pass/fail. Iteration count is diagnostic.

**Warning signs:**
- Implementation marked "valid" but when manually checking output, solution is wrong
- No test coverage for "correct iteration count, wrong solution" scenario
- Validation logic only checks one field from metrics.json
- False confidence in validation coverage

**Phase to address:**
Phase 1 (Validation Infrastructure) — Implement solution validation BEFORE iteration count validation.

---

### Pitfall 6: Floating Point Time Comparison Precision Issues

**What goes wrong:**
Sorting languages by execution time appears to work, but when times are very close (3.2529ms vs 3.2530ms), rankings flip randomly between runs due to measurement noise. Worse, attempting to detect "statistically significant performance changes" with naive comparison leads to false regression alerts.

**Why it happens:**
The Python wrapper in common.sh measures time with `time.perf_counter()` which has ~microsecond precision, but actual execution time variation includes:
- OS scheduler jitter
- CPU frequency scaling
- Background processes
- Cache state
- Page fault randomness

For fast implementations (Matrix 1 in <10ms), measurement noise can be 10-20% of total time.

**How to avoid:**
1. **Multiple run aggregation:** For each matrix, run 3-5 times and report median (not mean, to avoid outlier skew).
2. **Relative comparison tolerances:** From Knuth's algorithm, use combined absolute + relative tolerance:
   ```
   if (abs(time1 - time2) <= 1.0ms) || (abs(time1 - time2) <= 0.05 * max(time1, time2))
   ```
3. **Confidence intervals:** Store not just median but also min/max or standard deviation for uncertainty visualization.
4. **Statistical significance testing:** For regression detection, use Mann-Whitney U test or similar rather than naive threshold.
5. **Benchmark stability metadata:** Flag matrices/languages known to have high variance for different treatment.

**Warning signs:**
- Language rankings flip between identical runs
- "Performance regression detected" alerts for changes <5%
- Scatter plots where languages appear at different positions each run
- Time comparisons claiming microsecond-level precision on millisecond-scale operations

**Phase to address:**
Phase 1 (Validation Infrastructure) — Build statistical comparison utilities before implementing regression detection.

---

### Pitfall 7: Scoring Formula Optimization Without Domain Understanding

**What goes wrong:**
Data-driven optimization of scoring weights (0.8 time + 0.2 memory) based on correlation analysis or PCA produces a formula that doesn't reflect what the benchmark actually values. For example, optimization might suggest 0.95 time + 0.05 memory because memory variance is low, but this loses the intentional memory efficiency signal.

**Why it happens:**
Machine learning mindset: "Let the data tell us the weights!" But benchmarks encode value judgments: we *want* to reward memory efficiency even if variance is low. Pure statistical optimization finds weights that maximize variance explained, not weights that reflect benchmark philosophy. EcoVadis 2025 changes show this: they added circular economy criteria not because it maximized statistical variance, but because it reflects emerging sustainability priorities.

**How to avoid:**
1. **Philosophy-first design:** Document *why* we weight time at 80%. Is it:
   - Time is more important than memory in practice?
   - Memory usage is relatively uniform across implementations?
   - Historical precedent from other benchmarks?
2. **Sensitivity analysis:** Before changing weights, show how rankings change across different formulas. If 0.7/0.3 vs 0.8/0.2 dramatically changes results, formula is unstable.
3. **Domain expert review:** Benchmark users (language implementers, performance engineers) should validate that scoring reflects real priorities.
4. **Multi-metric display:** Rather than forcing single composite score, show time rank, memory rank, and composite rank separately.
5. **Algorithm-specific scoring:** BruteForce performance vs DLX performance are different competitions; don't force single universal scoring.

**Warning signs:**
- Scoring formula changes make unusual languages (Sed, Awk) suddenly rank top-tier
- Formula optimization produces weights like 0.93/0.07 (suspiciously one-sided)
- Inability to explain formula rationale to benchmark users
- Composite score rankings don't match expert intuition of "good performance"

**Phase to address:**
Phase 2 (Scoring Analysis) — Validate formula changes against domain expert intuition before implementation.

---

### Pitfall 8: Scatter Plot Overplotting with 70+ Languages

**What goes wrong:**
Scatter plot of time vs memory for 70 languages becomes an unreadable blob where most points overlap, labels collide, and only outliers are visible. This is worse for logarithmic scales where clustering compresses further.

**Why it happens:**
Standard scatter plot libraries don't handle high-density point clouds well. With 70 implementations, especially when many compile to native code with similar performance characteristics (C, C++, Rust, Zig, etc. all cluster in bottom-left), overplotting is guaranteed. Luzmo's bad visualization examples show pie charts with too many segments; scatter plots have the same issue at 70+ points.

**How to avoid:**
1. **Interactive filtering:** Allow users to:
   - Filter by algorithm type (BruteForce only, DLX only, etc.)
   - Filter by language family (compiled, interpreted, JIT, transpiled)
   - Highlight specific language on hover, fade others
2. **Density visualization:** Use hexbin or contour plots to show clustering, with individual points only for outliers.
3. **Jittering:** Add small random offsets to overlapping points (but document this clearly to avoid misleading precision claims).
4. **Zoom and pan:** Allow users to zoom into clustered regions interactively.
5. **Tiered labels:** Show top 10 performer labels always, rest on hover.
6. **Alternative views:** Provide bar chart fallback for users who can't interpret dense scatter plots.

**Warning signs:**
- 50+ overlapping data points in same visual region
- Labels unreadable due to collision
- Users can't distinguish between similar performers
- Plot requires 4K resolution to be legible

**Phase to address:**
Phase 3 (Visualization Implementation) — Test scatter plots with full 70-language dataset early, not with 5-language subset.

---

### Pitfall 9: Heatmap Color Scale Saturation

**What goes wrong:**
Heatmap showing performance across languages × matrices uses linear color scale, causing Matrix 6 (622M iterations, 10,000ms) to saturate at maximum color intensity while Matrices 1-5 are all the same "cold" color. Result: 5/6 of the data is visually indistinguishable.

**Why it happens:**
Matrix 6 is 1,000x harder than Matrix 1 for BruteForce algorithm (622M vs 656 iterations). Linear color mapping makes 99% of the data compress into the bottom 1% of color range. GoodData's visualization mistakes specifically call out inappropriate color scales as a top error.

**How to avoid:**
1. **Logarithmic color mapping:** Use log scale for color intensity when data spans orders of magnitude.
2. **Per-matrix normalization:** Color represents percentile rank within that matrix, not absolute time.
3. **Sequential vs diverging palettes:**
   - Sequential (green → red) for absolute performance
   - Diverging (blue ← white → red) for relative to baseline
4. **Explicit color bar labels:** Show actual values at color breakpoints, not just gradient.
5. **Multiple heatmaps:** Separate heatmap per algorithm type to avoid cross-algorithm comparison issues.

**Warning signs:**
- >80% of heatmap cells are same color
- Color scale shows values from 1ms to 10,000ms on linear scale
- Users can't distinguish between 2ms and 20ms visually
- Heatmap requires manual cross-reference with table to be useful

**Phase to address:**
Phase 3 (Visualization Implementation) — Test color scales with actual min/max data ranges before finalizing.

---

### Pitfall 10: Validation Retroactively Applied to Historical Data

**What goes wrong:**
New validation rules applied to 2 years of historical metrics.json files flag implementations as "invalid" that were considered correct at the time. This creates false "regressions" where nothing actually changed in the implementation.

**Why it happens:**
After implementing validation logic, the temptation is to run it against all historical data to "clean up" the dataset. But historical data was generated under different assumptions:
- Older iterations may have had measurement bugs since fixed
- Platform differences (old Docker image, different OS version)
- Compiler updates changing iteration counts
- Matrix files might have been modified

**How to avoid:**
1. **Validation versioning:** Mark each metrics.json with validation schema version:
   ```json
   {
     "solver": "C",
     "validationVersion": "v1", // or "none" for pre-validation data
     "timestamp": "2025-01-15T..."
   }
   ```
2. **Grandfather clause:** Historical data is exempt from new validation unless re-run.
3. **Opt-in revalidation:** Provide script to re-run benchmarks with new validation, don't auto-flag old data.
4. **Validation metadata separation:** Store validation results in separate file (e.g., `validation_results.json`) rather than modifying historical metrics.json.
5. **Change detection:** Only flag as regression if *same environment* produces different results, not if validation rules changed.

**Warning signs:**
- Historical trend charts show sudden "validation failures" spike at rule change date
- Languages that haven't been touched in months suddenly flagged invalid
- Confusion between "this implementation regressed" and "we changed validation rules"
- Loss of historical data visibility due to "invalid" flags

**Phase to address:**
Phase 1 (Validation Infrastructure) — Design validation versioning from start to avoid contaminating historical data.

---

## Technical Debt Patterns

| Shortcut | Immediate Benefit | Long-term Cost | When Acceptable |
|----------|-------------------|----------------|-----------------|
| Exact iteration count check only | Simple validation logic, easy to implement | False positives from legitimate platform differences, algorithm variations | Never — solution validation must be primary |
| Single composite score | Simple ranking, easy to visualize | Cannot show memory vs time tradeoffs, hides important nuances | Never for multi-dimensional performance data |
| Linear color scales on heatmaps | Default behavior in charting libraries | Saturation when data spans orders of magnitude | Only when data range is <10× |
| Global validation flags without tiers | Uniform error handling | Good implementations flagged alongside bad ones, signal-to-noise problem | Never with 70+ implementations |
| Changing score formula in-place | Simpler code, no versioning complexity | Historical comparisons broken, trend analysis impossible | Never with >6 months of history |
| Auto-applying new validation retroactively | Consistent validation across dataset | False regressions, historical data contamination | Only with explicit re-run, never automatically |

## Integration Gotchas

| Integration | Common Mistake | Correct Approach |
|-------------|----------------|------------------|
| C_Baselines.ts validation | Using exact equality for all algorithms | BruteForce requires exact match, DLX/CP may need tolerance based on algorithm heuristics |
| HTMLGenerator.ts scoring | Hardcoding scoring formula | Import formula from config, support versioned formulas (v1, v2, etc.) |
| Chart.js heatmap colors | Using default linear scale | Detect data range, auto-select log scale when max/min > 100 |
| benchmark_issues.json parsing | Treating all errors equally | Implement severity tiers (CRITICAL, WARNING, INFO) and filter by context |
| Historical metrics comparison | Direct score comparison across time | Check validation version and scoring version before comparing |

## Performance Traps

| Trap | Symptoms | Prevention | When It Breaks |
|------|----------|------------|----------------|
| Re-parsing all metrics.json on every page load | Slow report generation (>5s for 70 languages) | Cache parsed metrics, invalidate only on file change | >50 language implementations |
| Rendering 70-point scatter plot without virtualization | Browser lag on pan/zoom | Use Canvas instead of SVG, or D3 with quadtree optimization | >40 data points |
| Per-matrix validation without batching | Validation takes 5+ minutes | Batch validation across all matrices, parallel processing | >20 matrices tested |
| Storing full output string in metrics.json | Large JSON files (>10MB), slow parse | Truncate output to first/last N lines, store full output separately | Matrix 6 output >100KB per language |
| Real-time heatmap color recalculation | UI freezes on data updates | Pre-calculate color mappings, update only on data change | >500 total data points (70 langs × 6+ matrices) |

## Validation-Specific Pitfalls

### Solution Parsing Failures

| Pitfall | User Impact | Better Approach |
|---------|-------------|-----------------|
| Assuming output format is consistent | Validation fails on correct implementations with different whitespace/formatting | Parse flexibly: handle tabs vs spaces, extra newlines, different separators |
| Not handling partial output (timeout) | Timeouts reported as "invalid solution" instead of "timeout" | Check status field first, only validate solution if status="success" |
| Hardcoded output expectations | Multi-language output (e.g., "Résolu en Iterations=656") breaks parser | Extract using regex with localization variants, or standardize output contract |

### False Positive Minimization

**Critical Rule:** With 70 languages, even 2% false positive rate = 1-2 languages wrongly flagged. Precision matters more than recall.

**Implementation:**
1. Solution correctness is binary (PASS/FAIL)
2. Iteration count is diagnostic (MATCH/MISMATCH but both acceptable if solution correct)
3. Performance is continuous (no thresholds, show actual measurements)
4. Only flag as CRITICAL if solution is wrong OR solver crashed
5. Everything else is WARNING or INFO

## Visualization-Specific Pitfalls

### Chart Type Selection

| Pitfall | User Impact | Better Approach |
|---------|-------------|-----------------|
| Using pie chart for 70 languages | Completely unreadable segments | Never use pie charts with >10 segments; use bar chart sorted by performance |
| 3D bar charts for performance | Misleading depth perception makes comparisons hard | Always use 2D charts for performance data |
| Stacked bar for unrelated metrics | Stacking time + memory creates meaningless total | Use grouped bars or separate charts |
| Line chart for unordered categories | Implies trend where none exists (languages are categorical) | Use bar chart for categorical data |

### Color Strategy

**Problem:** 70 languages require 70 distinguishable colors.

**Solution:**
1. **Category-based coloring:** Color by language family (compiled=blue, interpreted=orange, JIT=green, transpiled=purple), use shades within family
2. **Performance-based coloring:** Color by performance tier (top 10=green, middle 50=yellow, bottom 10=red)
3. **Interactive highlighting:** All gray by default, selected language highlighted in accent color
4. **Accessibility:** Always provide non-color distinction (patterns, labels, shapes) for color-blind users

## "Looks Done But Isn't" Checklist

- [ ] **Validation:** Tested against FAILING implementations, not just passing ones — verify false positive handling
- [ ] **Scoring:** Historical data recalculated with new formula — verify trend continuity
- [ ] **Scatter Plot:** Tested with full 70-language dataset — verify overplotting handled
- [ ] **Heatmap:** Tested with Matrix 6 included — verify color scale doesn't saturate
- [ ] **Validation Flags:** Tested with 10+ simultaneous failures — verify UI doesn't overflow
- [ ] **Floating Point Comparison:** Tested with measurements <1ms apart — verify noise tolerance
- [ ] **Iteration Validation:** Tested with correct solution but wrong iteration count — verify doesn't fail
- [ ] **Solution Validation:** Tested with matching iterations but wrong solution — verify fails correctly
- [ ] **Historical Compatibility:** Checked old metrics.json still render — verify no breaking changes
- [ ] **Color Blindness:** Checked all visualizations with Coblis simulator — verify deuteranopia distinguishable

## Recovery Strategies

| Pitfall | Recovery Cost | Recovery Steps |
|---------|---------------|----------------|
| Validation false positives | MEDIUM | 1. Implement tiered validation levels 2. Reprocess flagged implementations 3. Document expected variances |
| Broken historical scores | HIGH | 1. Restore original score field 2. Add scoreV2 field 3. Recalculate all historical data with both formulas 4. Update all visualizations to support dual scoring |
| Misleading visualizations | LOW | 1. Fix scale/color issues 2. Republish report 3. Add changelog note explaining fix |
| Overplotted scatter | LOW | 1. Add interactive filtering 2. Implement hover highlighting 3. Provide alternative bar chart view |
| Saturated heatmap | LOW | 1. Switch to log color scale 2. Republish report 3. Add color scale explanation |
| Contaminated historical data | HIGH | 1. Restore unmodified historical metrics.json 2. Add validationVersion field 3. Separate validation results storage |
| Validation noise overwhelming report | MEDIUM | 1. Implement severity tiers 2. Add collapsible sections 3. Move diagnostics to separate page |

## Pitfall-to-Phase Mapping

| Pitfall | Prevention Phase | Verification |
|---------|------------------|--------------|
| Overly strict iteration validation | Phase 1: Validation Infrastructure | Test against 5 languages with known platform differences |
| Breaking historical comparability | Phase 2: Scoring Analysis | Verify scoreV1 still calculable from historical data |
| Misleading visualizations | Phase 3: Visualization | Review checklist from Thoughtspot misleading graphs guide |
| Validation noise | Phase 1: Validation Infrastructure | UI review with 20+ simultaneous warnings |
| Missing solution validation | Phase 1: Validation Infrastructure | Test case: correct iterations, wrong solution |
| Floating point comparison issues | Phase 1: Validation Infrastructure | Statistical test with <1ms time differences |
| Formula optimization without domain input | Phase 2: Scoring Analysis | Expert review session with 3+ benchmark users |
| Scatter plot overplotting | Phase 3: Visualization | Test with full 70-language dataset |
| Heatmap color saturation | Phase 3: Visualization | Test with Matrix 6 included |
| Retroactive validation contamination | Phase 1: Validation Infrastructure | Verify historical metrics.json unmodified after validation run |

## Real-World Examples

### Android Benchmark CI Regression Detection
**Lesson:** Android's Jetpack Benchmark uses a "step fit" algorithm with configurable WIDTH and Threshold parameters. They found that presubmit (blocking workflow) requires higher confidence than post-submit to avoid false positives. For this project: iteration count validation should be high-confidence (exact match for BruteForce), while performance regression detection should be lower-confidence (statistical significance).

**Source:** [Fighting regressions with Benchmarks in CI](https://medium.com/androiddevelopers/fighting-regressions-with-benchmarks-in-ci-6ea9a14b5c71)

### OWASP Benchmark False Positive Tracking
**Lesson:** OWASP tracks four outcomes: True Positive, False Negative, True Negative, False Positive. They found that claiming "zero false positives" is mathematically implausible. For this project: expect and plan for validation false positives; focus on minimizing them, not eliminating them.

**Source:** [OWASP Benchmark Project](https://owasp.org/www-project-benchmark/)

### World Benchmarking Alliance 2026 Scoring Changes
**Lesson:** WBA simplified their scoring approach for 2026 with 0-100 normalization and binary elements, but had to provide detailed migration documentation because historical comparisons became complex. For this project: any scoring change requires documented conversion formula and dual-scoring transition period.

**Source:** [WBA Scoring Approach 2026](https://archive.worldbenchmarkingalliance.org/research/scoring-approach-2026-benchmarks/)

### CDP 2025 Verification Threshold Changes
**Lesson:** CDP added stricter third-party verification requirements (70% for Scope 3 emissions) for 2025, causing disruption for companies with historical data under old rules. They had to clarify that historical data was grandfathered. For this project: validation rules shouldn't retroactively invalidate historical data.

**Source:** [CDP 2025 Scoring Methodology Changes](https://www.nossadata.com/blog/cdp-2025-scoring-methodology-changes)

### Harvard Business School Bad Visualization Study
**Lesson:** Truncated y-axes are the most common visualization mistake, making small differences appear dramatic. With Matrix 6 being 1000× harder than Matrix 1, truncation temptation is extreme. For this project: use logarithmic scales or explicit normalization, never truncate to exaggerate differences.

**Source:** [Bad Data Visualization: 5 Examples](https://online.hbs.edu/blog/post/bad-data-visualization)

## Sources

### Validation and False Positives
- [OWASP Benchmark Project](https://owasp.org/www-project-benchmark/) — Framework for tracking false positives/negatives in security testing
- [SastBench: A Benchmark for Testing Agentic SAST Triage](https://arxiv.org/abs/2601.02941) — 2026 research on benchmark validation design
- [Determining A Benchmark Of False Positives With SAST Tools](https://www.mend.io/blog/benchmark-of-false-positives/) — Industry false positive rates

### Scoring Methodology Changes
- [Scoring approach for 2026 benchmarks | World Benchmarking Alliance](https://archive.worldbenchmarkingalliance.org/research/scoring-approach-2026-benchmarks/) — Real-world scoring migration example
- [CDP 2025 Scoring Methodology Changes](https://www.nossadata.com/blog/cdp-2025-scoring-methodology-changes) — Historical data compatibility challenges
- [EcoVadis 2025 Changes](https://nexioprojects.com/how-the-ecovadis-scoring-system-works-in-2025/) — Multi-dimensional scoring evolution

### Visualization Best Practices
- [Bad Data Visualization: 5 Examples](https://online.hbs.edu/blog/post/bad-data-visualization) — Common visualization mistakes
- [How to Identify Misleading Graphs and Charts](https://www.thoughtspot.com/data-trends/data-visualization/how-to-identify-misleading-graphs-and-charts) — Misleading scale patterns
- [Misleading Graph Examples: How Writers Use it To Manipulate You](https://venngage.com/blog/misleading-graphs/) — Cherry-picking and bias in presentation
- [10 Common Data Visualization Mistakes](https://medium.com/agoda-engineering/10-common-data-visualization-mistakes-and-how-to-avoid-them-e3896fe8e104) — Practical visualization pitfalls

### Algorithm Validation
- [Floating Point Comparison Algorithms](https://jtempest.github.io/float_eq-rs/book/background/float_comparison_algorithms.html) — Tolerance-based comparison strategies
- [Comparing Floating Point Numbers, 2012 Edition](https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/) — Bruce Dawson's definitive guide
- [Boost.Test Floating Point Comparison](https://www.boost.org/doc/libs/master/libs/test/doc/html/boost_test/testing_tools/extended_comparison/floating_point.html) — Knuth's epsilon-based comparison

### Regression Detection
- [Fighting regressions with Benchmarks in CI](https://medium.com/androiddevelopers/fighting-regressions-with-benchmarks-in-ci-6ea9a14b5c71) — Android's step-fit algorithm for performance regression detection
- [Numenta Anomaly Benchmark](https://www.numenta.com/assets/pdf/numenta-anomaly-benchmark/NAB-Business-Paper.pdf) — Scoring function for anomaly detection with time-weighted windows

### Data Presentation Bias
- [Cherry Picking Data: Crafting Narratives in a Sea of Statistics](https://fastercapital.com/content/Cherry-Picking-Data--Cherry-Picking-Data--Crafting-Narratives-in-a-Sea-of-Statistics.html) — Selection bias patterns
- [Misleading Beyond Visual Tricks: How People Actually Lie with Charts](https://dl.acm.org/doi/10.1145/3544548.3580910) — CHI 2023 research on chart manipulation

---
**Pitfalls research for:** Benchmark suite quality improvements (validation, scoring, visualization)
**Researched:** 2026-01-23
**Confidence:** HIGH — Based on established benchmarking literature, recent 2025-2026 scoring methodology changes in production systems, and visualization research
