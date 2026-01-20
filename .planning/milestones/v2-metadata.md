# Milestone: v2 Metadata Enrichment

**Goal:** Automate the collection of rich metadata and author images for all supported languages using Wikipedia.

## Phase 1: Scaffolding (Story 1)
- [ ] Create `requirements.txt` with `wikipedia`.
- [ ] Create `scripts/fetch_metadata.py`.
- [ ] Implement basic language list iteration (mirroring `orderedLanguages`).

## Phase 2: Logic Implementation (Story 2)
- [ ] Implement Wikipedia search (handling "(programming language)" suffix).
- [ ] Implement Infobox/Data extraction (Authors, Dates).
- [ ] Implement Image downloading to `Algorithms/BruteForce/<Lang>/Media/`.
- [ ] Implement Cleanup logic (delete unreferenced non-logo images).

## Phase 3: Integration (Story 3)
- [ ] Generate `LanguagesMetadata.ts` content.
- [ ] Run the full update.
- [ ] Verify UI displays new images and data.

## Metrics
- Number of languages successfully enriched.
- Number of author images downloaded.
