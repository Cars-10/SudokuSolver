# PRD: Enhance Language Modal and Asset Management

## Introduction
The current Language Modal displays basic metadata but omits the rich historical context available in `languageHistories`. Additionally, numerous languages lack logo assets, which diminishes the visual quality of the benchmark report. This feature aims to enrich the Language Modal with detailed history and structured metadata while providing a dashboard to track missing assets.

## Goals
- **Enrich Modal Content:** Display the detailed "Historical Context" and new structured metadata (Paradigm, Type System) in the Language Modal.
- **Track Missing Assets:** Create a visibility mechanism (Dashboard) to identify languages missing logos.
- **Improve Data Structure:** Formalize the new metadata fields in `LanguagesMetadata.ts`.

## User Stories

### US-001: Add Historical Context to Language Modal
**Description:** As a user, I want to read the historical background of a language within its modal to understand its origins and significance.

**Acceptance Criteria:**
- [ ] Add a new "Historical Context" section to `langModal` in `HTMLGenerator.ts`.
- [ ] Position this section immediately above "Creators & Authors".
- [ ] Populate it with text from `languageHistories` in `LanguagesMetadata.ts`.
- [ ] If no history exists, hide the section or show a fallback message.
- [ ] Typecheck passes.
- [ ] Verify in browser using dev-browser skill (open modal for C, Java, or Python).

### US-002: Add Structured Metadata Fields
**Description:** As a user, I want to see specific technical attributes like "Paradigm" and "Type System" to quickly categorize the language.

**Acceptance Criteria:**
- [ ] Update `languageMetadata` in `LanguagesMetadata.ts` to include `paradigm` and `typeSystem` fields for all languages.
- [ ] Display these fields in the `modal-header-top` or a new "Key Specs" row in the modal.
- [ ] Style them as tags or pills for quick scanning.
- [ ] Typecheck passes.
- [ ] Verify in browser using dev-browser skill.

### US-003: Asset Health Dashboard
**Description:** As a developer, I want a summary of missing logos at the bottom of the report so I know which assets need to be added.

**Acceptance Criteria:**
- [ ] Create a "Asset Health" section at the bottom of the `benchmark_report.html` (or a separate admin view).
- [ ] List all languages that are missing a corresponding file in the `logos/` directory.
- [ ] (Optional) Add a visual indicator (e.g., red dot) on the main table for languages with missing assets.
- [ ] Typecheck passes.
- [ ] Verify in browser using dev-browser skill.

## Functional Requirements
- FR-1: The Language Modal must display the `languageHistories` text if available.
- FR-2: The Language Modal must display `paradigm` and `typeSystem` tags.
- FR-3: The report must generate a list of missing logo files based on the difference between the language list and the `logos/` directory contents.

## Non-Goals
- Automatically fetching missing logos from the internet (this is a manual/tool-assisted process, not part of the report generation).
- changing the "Neon" theme colors.

## Technical Considerations
- `LanguagesMetadata.ts` is the source of truth. Ensure strict typing for the new fields.
- The Asset Health Dashboard logic needs to run during report generation (`generate_report_only.ts` or `HTMLGenerator.ts`) and check for file existence. *Note: Since the report is static HTML, we might need to pre-calculate this or just check if the logo path defaults to a placeholder.*

## Success Metrics
- 100% of languages with history data display it in the modal.
- "Asset Health" section correctly identifies the ~40 missing logos found in the initial analysis.

## Open Questions
- Should the Asset Health Dashboard be visible to all users or hidden behind a flag/admin mode? (Assumption: Visible at bottom is fine for this dev tool).
