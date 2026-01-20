# Phase 1: Metadata Enrichment Plan

## Objective
Enrich `Languages/metadata.json` to support better reporting and filtering. We need to ensure every language in the `Languages/` directory has a corresponding entry in `metadata.json` with complete fields: `history` (short, <50 chars), `description`, `paradigm`, and `typeSystem`.

## Execution Context
- **Phase**: 1
- **Goal**: Complete metadata coverage for all ~80 languages.
- **Constraints**: 
  - `history` field must be concise (< 50 chars).
  - `description` field contains the longer narrative.
  - `paradigm` and `typeSystem` must be populated.

## Context
- **Source**: `Languages/` directory structure.
- **Target**: `Languages/metadata.json`.
- **Reference**: `Languages/metadata.json` existing schema.

## Tasks

### 1. Audit Current Metadata
**Goal**: Identify gaps between directory structure and JSON metadata.
- **Action**: Create and run `scripts/audit_metadata.js`.
- **Logic**: 
  - List all folders in `Languages/` (excluding special ones like `common.sh`).
  - Check if key exists in `metadata.json`.
  - Check for missing fields: `history`, `paradigm`, `typeSystem`.
  - Check `history` length (> 50 chars).
- **Output**: A report (console or markdown) listing missing languages and incomplete fields.

### 2. Standardize Schema & Fix History
**Goal**: Enforce the < 50 char limit on history and move text to description.
- **Action**: Update `metadata.json`.
- **Logic**:
  - For entries with long history, move the text to `description` (if description is empty) or truncate/summarize `history` and ensure full context is preserved in `description`.
  - Ensure `history` is a punchy tagline.

### 3. Populate Missing Fields (Batch 1: A-M)
**Goal**: Fill in `paradigm`, `typeSystem`, and `history` for languages starting with A-M.
- **Action**: Update `metadata.json`.
- **Source**: General knowledge or fast search.

### 4. Populate Missing Fields (Batch 2: N-Z)
**Goal**: Fill in `paradigm`, `typeSystem`, and `history` for languages starting with N-Z.
- **Action**: Update `metadata.json`.

### 5. Final Verification
**Goal**: Ensure 100% coverage and compliance.
- **Action**: Re-run `audit_metadata.js`.
- **Success Criteria**: 0 missing languages, 0 missing fields, 0 long history entries.

## Verification
- Run `node scripts/audit_metadata.js` and ensure clean output.
- Check `Languages/metadata.json` is valid JSON.

## Output
- Updated `Languages/metadata.json`.
