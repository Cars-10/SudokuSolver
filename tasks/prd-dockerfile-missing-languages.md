# PRD: Add Missing Languages to Dockerfile

## Introduction

Add the remaining missing language toolchains to the Docker image: FreeBASIC (BASIC), CoffeeScript, and ensure Sed is available. Most languages the user mentioned are already installed, but these three need to be added.

## Goals

- Add FreeBASIC compiler for BASIC language support
- Add CoffeeScript via npm for CoffeeScript support
- Verify/add Sed to system dependencies
- Ensure all three languages can run their Sudoku solvers in Docker

## User Stories

### US-001: Add FreeBASIC to Dockerfile
**Description:** As a developer, I want FreeBASIC installed in Docker so BASIC Sudoku solver can run.

**Acceptance Criteria:**
- [ ] Download and install FreeBASIC for Linux (aarch64 and x86_64)
- [ ] FreeBASIC binary available at `/usr/local/bin/fbc` or in PATH
- [ ] Running `fbc --version` in container succeeds
- [ ] Add to section 12 or create new section for BASIC

### US-002: Add CoffeeScript to Dockerfile
**Description:** As a developer, I want CoffeeScript installed in Docker so CoffeeScript Sudoku solver can run.

**Acceptance Criteria:**
- [ ] Add `npm install -g coffeescript` to Node.js section (section 3)
- [ ] Running `coffee --version` in container succeeds
- [ ] CoffeeScript files can be executed with `coffee Sudoku.coffee`

### US-003: Ensure Sed is available
**Description:** As a developer, I want Sed available in Docker for the Sed Sudoku solver.

**Acceptance Criteria:**
- [ ] Add `sed` to system dependencies if not already present
- [ ] Running `sed --version` in container succeeds
- [ ] Note: Sed is usually in base Ubuntu, but explicit install ensures availability

## Functional Requirements

- FR-1: FreeBASIC must support both ARM64 and x86_64 architectures
- FR-2: CoffeeScript must be installed globally via npm
- FR-3: All three tools must be in PATH and executable
- FR-4: Docker image must build successfully with new additions

## Non-Goals

- Not verifying all 88+ languages work
- Not fixing any existing language installations
- Not modifying the Sudoku solvers themselves

## Technical Considerations

### FreeBASIC Installation
FreeBASIC provides pre-built binaries for Linux. For Ubuntu 24.04:
- x86_64: Download from GitHub releases
- aarch64: May need to build from source or find ARM builds

Example installation pattern:
```dockerfile
RUN ARCH=$(dpkg --print-architecture) \
    && FB_VERSION=1.10.1 \
    && if [ "$ARCH" = "arm64" ]; then \
         # ARM64 build or source compilation
       else \
         wget https://github.com/nickcyran/freebasic/releases/download/v${FB_VERSION}/FreeBASIC-${FB_VERSION}-ubuntu-22.04-x86_64.tar.gz \
         && tar xzf ... \
       fi
```

### CoffeeScript Installation
Simple npm global install in Node.js section:
```dockerfile
&& npm install -g coffeescript
```

### Sed Installation
Add to system dependencies:
```dockerfile
sed \
```

## Success Metrics

- Docker image builds without errors
- All three language commands execute successfully in container
- Sudoku solvers for BASIC, CoffeeScript, and Sed run in Docker

## Open Questions

- Does FreeBASIC have official ARM64 Linux builds, or do we need to compile from source?
- Should we pin specific versions for reproducibility?
