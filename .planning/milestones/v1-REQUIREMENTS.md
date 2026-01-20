# Requirements Archive: v1 Scoring & UI

**Archived:** 2026-01-20
**Status:** ✅ SHIPPED

This is the archived requirements specification for v1.
For current requirements, see `.planning/REQUIREMENTS.md` (created for next milestone).

---

# Requirements

## v1 Requirements

### Scoring
- [x] **SCORE-01**: Implement weighted geometric mean scoring algorithm (Time, Memory, CPU)
- [x] **SCORE-02**: Normalize all metrics against the C baseline
- [x] **SCORE-03**: Allow configurable weights for each metric (default: equal)
- [x] **SCORE-04**: Update HTMLGenerator to calculate and inject scores at build time

### UI/UX
- [x] **UI-01**: Remove individual "Run" buttons from table cells to reduce clutter
- [x] **UI-02**: Implement `screenfull` library for robust Matrix Race fullscreen toggle
- [x] **UI-03**: Prevent Matrix Race from automatically re-entering fullscreen after exit
- [x] **UI-04**: Add clear visual indication of active/inactive fullscreen state

### Metadata
- [x] **META-01**: Update methodology text to accurately reflect the weighted geometric mean formula
- [x] **META-02**: Ensure consistency between code logic and UI descriptions

## v2 Requirements (Deferred)
- [ ] **SCORE-05**: Implement algorithmic fingerprinting scoring (based on iteration counts)
- [ ] **UI-05**: Add "Zen Mode" toggle for simplified viewing

## Out of Scope
- [ ] **OOS-01**: Remote execution (Docker/Cloud) — Focusing on local efficiency first
- [ ] **OOS-02**: Changing the core recursive backtracking algorithm — Must maintain benchmark purity

## Traceability
| Req ID | Phase | Status |
|--------|-------|--------|
| SCORE-01 | 1 | Complete |
| SCORE-02 | 1 | Complete |
| SCORE-03 | 1 | Complete |
| SCORE-04 | 1 | Complete |
| UI-01 | 2 | Complete |
| UI-02 | 2 | Complete |
| UI-03 | 2 | Complete |
| UI-04 | 2 | Complete |
| META-01 | 3 | Complete |
| META-02 | 3 | Complete |

---

## Milestone Summary

**Shipped:** 10 of 10 v1 requirements
**Adjusted:** SCORE-03 (defaults set to 80/20 instead of equal)
**Dropped:** None

---
*Archived: 2026-01-20 as part of v1 milestone completion*
