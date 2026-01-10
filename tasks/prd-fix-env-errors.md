# PRD: Fix Environment Errors

## Introduction

16 language implementations have environment errors preventing them from running in Docker. This PRD covers fixing runMe.sh scripts, documenting package requirements, and resolving platform-specific issues so all languages can be benchmarked (where technically possible).

## Goals

- Fix all languages with env_error status to either run successfully or document why they cannot
- Document package requirements in each language's README
- Create architecture-specific Assembly variants (x86_64 and ARM64)
- Investigate and fix C# and Objective-C build failures
- All fixable languages should produce 656 iterations for Matrix 1

## Languages to Fix (Alphabetical)

### Missing Packages (need Docker install)
| Language | Package | Install Command |
|----------|---------|-----------------|
| Ada | gnat | `apt install gnat` |
| BASIC | freebasic | `apt install freebasic` or build from source |
| CoffeeScript | coffeescript | `npm install -g coffeescript` |
| Crystal | crystal | Special install from crystal-lang.org |
| Fish | fish | `apt install fish` |
| Jupyter | jupyter | `pip install jupyter` |
| Ksh | ksh | `apt install ksh` |
| Smalltalk | gnu-smalltalk | `apt install gnu-smalltalk` |
| Tcsh | tcsh | `apt install tcsh` |
| Vimscript | vim | `apt install vim` |
| XSLT | xsltproc | `apt install xsltproc` |
| Zsh | zsh | `apt install zsh` |

### Platform/Build Issues
| Language | Issue | Action |
|----------|-------|--------|
| AppleScript | macOS only | Already documented - skip |
| Assembly | ARM64 vs x86_64 | Create architecture variants |
| C_Sharp | Build failed | Investigate dotnet build issue |
| Objective-C | Compilation failed | Investigate, may need GNUstep |

## User Stories

### US-001: Fix Ada runMe.sh and document
**Description:** As a developer, I want Ada to run correctly when gnat is installed.

**Acceptance Criteria:**
- [ ] Check if `gnatmake` exists in Docker: `docker exec sudokusolver-app-1 which gnatmake`
- [ ] If missing, notify user to install: `apt install gnat`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting gnat requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-002: Create Assembly architecture variants
**Description:** As a developer, I want Assembly to support both x86_64 and ARM64 architectures.

**Acceptance Criteria:**
- [ ] Read existing Assembly implementation to understand current approach
- [ ] Create `Sudoku_x86_64.s` for x86_64 architecture
- [ ] Create `Sudoku_arm64.s` for ARM64/aarch64 architecture
- [ ] Update runMe.sh to detect architecture with `uname -m`
- [ ] Compile appropriate variant based on architecture
- [ ] Update README.md documenting architecture support
- [ ] Test on available architecture: verify 656 iterations

---

### US-003: Fix BASIC runMe.sh and document
**Description:** As a developer, I want BASIC (FreeBASIC) to run correctly when fbc is installed.

**Acceptance Criteria:**
- [ ] Check if `fbc` exists in Docker: `docker exec sudokusolver-app-1 which fbc`
- [ ] If missing, notify user to install FreeBASIC
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting FreeBASIC requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-004: Fix CoffeeScript runMe.sh and document
**Description:** As a developer, I want CoffeeScript to run correctly when coffee is installed.

**Acceptance Criteria:**
- [ ] Check if `coffee` exists in Docker: `docker exec sudokusolver-app-1 which coffee`
- [ ] If missing, notify user to install: `npm install -g coffeescript`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting coffeescript requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-005: Fix Crystal runMe.sh and document
**Description:** As a developer, I want Crystal to run correctly when crystal is installed.

**Acceptance Criteria:**
- [ ] Check if `crystal` exists in Docker: `docker exec sudokusolver-app-1 which crystal`
- [ ] If missing, notify user about Crystal installation (requires special setup)
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting Crystal installation steps
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-006: Investigate and fix C_Sharp build failure
**Description:** As a developer, I want C# to build and run correctly.

**Acceptance Criteria:**
- [ ] Run `docker exec -w /app/Languages/C_Sharp sudokusolver-app-1 dotnet build` to see error
- [ ] Identify root cause of build failure
- [ ] Fix project configuration or code as needed
- [ ] Update runMe.sh if needed
- [ ] Create/update README.md with any special requirements
- [ ] Test: verify 656 iterations for Matrix 1

---

### US-007: Fix Fish runMe.sh and document
**Description:** As a developer, I want Fish shell to run correctly when fish is installed.

**Acceptance Criteria:**
- [ ] Check if `fish` exists in Docker: `docker exec sudokusolver-app-1 which fish`
- [ ] If missing, notify user to install: `apt install fish`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting fish requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-008: Fix Jupyter runMe.sh and document
**Description:** As a developer, I want Jupyter to run correctly when jupyter is installed.

**Acceptance Criteria:**
- [ ] Check if `jupyter` exists in Docker: `docker exec sudokusolver-app-1 which jupyter`
- [ ] If missing, notify user to install: `pip install jupyter`
- [ ] Update runMe.sh with proper execution method for Jupyter notebooks
- [ ] Create/update README.md documenting jupyter requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-009: Fix Ksh runMe.sh and document
**Description:** As a developer, I want Korn shell to run correctly when ksh is installed.

**Acceptance Criteria:**
- [ ] Check if `ksh` exists in Docker: `docker exec sudokusolver-app-1 which ksh`
- [ ] If missing, notify user to install: `apt install ksh`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting ksh requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-010: Investigate and fix Objective-C build failure
**Description:** As a developer, I want Objective-C to build and run correctly in Linux.

**Acceptance Criteria:**
- [ ] Run compilation manually to see full error output
- [ ] Determine if GNUstep or other libraries are needed
- [ ] If fixable: install dependencies, update runMe.sh
- [ ] If not fixable in Linux: document as macOS-only like AppleScript
- [ ] Create/update README.md with requirements or limitations
- [ ] Test if fixed: verify 656 iterations for Matrix 1

---

### US-011: Fix Smalltalk runMe.sh and document
**Description:** As a developer, I want GNU Smalltalk to run correctly when gst is installed.

**Acceptance Criteria:**
- [ ] Check if `gst` exists in Docker: `docker exec sudokusolver-app-1 which gst`
- [ ] If missing, notify user to install: `apt install gnu-smalltalk`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting gnu-smalltalk requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-012: Fix Tcsh runMe.sh and document
**Description:** As a developer, I want Tcsh to run correctly when tcsh is installed.

**Acceptance Criteria:**
- [ ] Check if `tcsh` exists in Docker: `docker exec sudokusolver-app-1 which tcsh`
- [ ] If missing, notify user to install: `apt install tcsh`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting tcsh requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-013: Fix Vimscript runMe.sh and document
**Description:** As a developer, I want Vimscript to run correctly when vim is installed.

**Acceptance Criteria:**
- [ ] Check if `vim` exists in Docker: `docker exec sudokusolver-app-1 which vim`
- [ ] If missing, notify user to install: `apt install vim`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting vim requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-014: Fix XSLT runMe.sh and document
**Description:** As a developer, I want XSLT to run correctly when xsltproc is installed.

**Acceptance Criteria:**
- [ ] Check if `xsltproc` exists in Docker: `docker exec sudokusolver-app-1 which xsltproc`
- [ ] If missing, notify user to install: `apt install xsltproc`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting xsltproc requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

---

### US-015: Fix Zsh runMe.sh and document
**Description:** As a developer, I want Zsh to run correctly when zsh is installed.

**Acceptance Criteria:**
- [ ] Check if `zsh` exists in Docker: `docker exec sudokusolver-app-1 which zsh`
- [ ] If missing, notify user to install: `apt install zsh`
- [ ] Update runMe.sh with proper SOLVER_BINARY and check_toolchain
- [ ] Create/update README.md documenting zsh requirement
- [ ] Test after package install: verify 656 iterations for Matrix 1

## Functional Requirements

- FR-1: Each runMe.sh must use check_toolchain to verify tool availability
- FR-2: Each runMe.sh must set SOLVER_BINARY appropriately
- FR-3: Each language must have README.md documenting package requirements
- FR-4: Assembly must detect architecture and use appropriate source file
- FR-5: Missing packages should result in clear env_error message, not crash
- FR-6: All fixed languages must produce exactly 656 iterations for Matrix 1

## Non-Goals

- Not modifying the Docker image directly (user installs packages)
- Not creating new language implementations
- Not optimizing existing implementations
- Not fixing algorithm bugs (iteration count mismatches)

## Technical Considerations

- Use `uname -m` to detect architecture (x86_64 vs aarch64)
- Assembly: x86_64 uses NASM/GAS syntax, ARM64 uses different registers/instructions
- Objective-C may require GNUstep: `apt install gnustep-devel`
- Crystal requires adding their apt repository
- Some packages may not be available on all Linux distributions

## Success Metrics

- All 15 fixable languages have updated runMe.sh scripts
- All 15 fixable languages have README.md with requirements
- Assembly works on both x86_64 and ARM64
- After package installation, all languages produce 656 iterations

## Open Questions

1. Should we create a single install script that installs all missing packages?
2. Should Crystal be skipped if installation is too complex?
3. Is Objective-C worth the effort with GNUstep, or document as macOS-only?
