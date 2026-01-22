# Benchmark Failure Remediation Plan

**Generated:** 2026-01-21
**Total Languages Tested:** ~88
**Failures Captured:** 28 languages
**Success Rate:** ~68% (60/88)

---

## Executive Summary

The benchmark run revealed **3 major categories** of failures affecting 28 languages:

1. **Permission/Executable Issues (Exit 126)** - 15 languages - Binary exists but can't execute
2. **Missing Dependencies (Env Errors)** - 5 languages - Tools not in PATH
3. **Performance/Timeout Issues** - 2 languages - Too slow for timeouts
4. **Other Runtime Errors** - 6 languages - Various issues

---

## Category 1: Exit Code 126 - Permission/Executable Issues

**Languages Affected (15):**
- Cobol, D, Fortran, Go, Haskell, Nim, OCaml, Objective-C, Pascal, Rust, SML, Swift, V, Vala, Zig

**Root Cause:**
Exit code 126 means "command cannot execute" - typically:
- Binary compiled for wrong architecture (Intel x86_64 vs ARM64)
- Missing executable permissions
- Dynamic library dependencies missing

**Diagnosis Steps:**
```bash
# Check architecture
file Algorithms/BruteForce/Rust/Sudoku
# Check permissions
ls -l Algorithms/BruteForce/Rust/Sudoku
# Check dynamic dependencies
otool -L Algorithms/BruteForce/Rust/Sudoku  # macOS
ldd Algorithms/BruteForce/Rust/Sudoku       # Linux
```

**Remediation:**

### Option A: Recompile for ARM64 (Recommended)
```bash
cd Algorithms/BruteForce/Rust
FORCE_COMPILE=1 ./runMe.sh ../../../Matrices/1.matrix
```

### Option B: Bulk Recompile Script
```bash
#!/bin/bash
for lang in Cobol D Fortran Go Haskell Nim OCaml Objective-C Pascal Rust SML Swift V Vala Zig; do
    cd "Algorithms/BruteForce/$lang"
    echo "Recompiling $lang..."
    FORCE_COMPILE=1 ./runMe.sh ../../../Matrices/1.matrix
    cd ../../..
done
```

**Priority:** **HIGH** - These are core languages with working implementations

**Estimated Fix Time:** 30-60 minutes (bulk recompile)

---

## Category 2: Exit Code 127 - Command Not Found

**Languages Affected (2):**
- Fennel
- Jupyter

**Root Cause:**
Interpreter/runtime not installed or not in PATH

**Remediation:**

### Fennel
```bash
# Check if installed
which fennel

# Install (macOS)
brew install fennel

# Or install via Lua
luarocks install fennel
```

### Jupyter
```bash
# Check if installed
which jupyter

# Install (Python)
pip3 install jupyter notebook

# Or use system package
brew install jupyterlab
```

**Priority:** **MEDIUM** - Non-critical interpreted languages

**Estimated Fix Time:** 10 minutes

---

## Category 3: Environment Errors - Missing Tools

**Languages Affected (5):**
- **Assembly** - ARM64 compilation error ("unknown AArch64 fixup kind")
- **Brainfuck** - "No matrix files found" (path issue)
- **EmacsLisp** - "emacs not found in PATH"
- **Janet** - "janet not found in PATH"
- **Red** - "red not found in PATH"

**Remediation:**

### Assembly
**Issue:** ARM64-specific compilation failure
**Fix:** Update assembly source for ARM64 or compile for x86_64 with Rosetta
```bash
# Option 1: Install Rosetta 2 (if not installed)
softwareupdate --install-rosetta

# Option 2: Update assembly code for ARM64
# (Requires modifying Sudoku.s for ARM64 syntax)
```

### EmacsLisp
```bash
brew install emacs
```

### Janet
```bash
brew install janet
```

### Red
```bash
# Download from https://www.red-lang.org/
# Red only supports 32-bit, won't work on ARM64 Mac
```
**Note:** Red cannot run natively on Apple Silicon (ARM64)

### Brainfuck
**Issue:** Path resolution bug in runMe.sh
**Fix:** Already identified - script uses wrong relative path

**Priority:** **MEDIUM-LOW** - Specialized/esoteric languages

**Estimated Fix Time:** 15-20 minutes

---

## Category 4: Timeouts

**Languages Affected (2):**
- **Fish** - Matrices 2 and 5 timeout after 5 minutes
- **Sed** - Matrices 1 and 2 timeout after 5 minutes

**Root Cause:**
These languages are extremely slow for algorithmic tasks (shell/text processing tools)

**Remediation:**

### Option A: Increase Timeout (Quick Fix)
```bash
# Increase timeout from 300s to 3600s (1 hour)
timeout 3600 bash runMe.sh ...
```

### Option B: Run Separately with Extended Time
```bash
cd Algorithms/BruteForce/Fish
# Run only matrix 1 first (simplest)
timeout 7200 bash runMe.sh ../../../Matrices/1.matrix
```

### Option C: Mark as "Slow" and Skip in Standard Benchmarks
Add to benchmark config:
```json
{
  "Fish": { "status": "slow", "matrices": [1] },
  "Sed": { "status": "slow", "matrices": [1] }
}
```

**Priority:** **LOW** - These are inherently slow, not bugs

**Estimated Fix Time:** 2-4 hours (if running to completion)

---

## Category 5: Other Runtime Errors

**Languages Affected (3):**
- **Dc** - Exit code 3
- **M4** - Exit code 134 (abort signal)
- **Verilog** - Exit code 15
- **F_Sharp** - Exit code 150

**Remediation:**

### Dc (Exit 3)
**Diagnosis:**
```bash
cd Algorithms/BruteForce/Dc
bash runMe.sh ../../../Matrices/1.matrix 2>&1 | tail -20
```
**Likely Cause:** Algorithm bug or input parsing error

### M4 (Exit 134 - SIGABRT)
**Diagnosis:**
```bash
cd Algorithms/BruteForce/M4
bash runMe.sh ../../../Matrices/1.matrix 2>&1 | tail -20
```
**Likely Cause:** Stack overflow or assertion failure

### Verilog (Exit 15)
**Diagnosis:**
```bash
cd Algorithms/BruteForce/Verilog
bash runMe.sh ../../../Matrices/1.matrix 2>&1 | tail -20
```
**Likely Cause:** Simulation error

### F# (Exit 150)
**Likely Cause:** .NET runtime error or missing dependency
```bash
cd Algorithms/BruteForce/F_Sharp
dotnet --version  # Check .NET installed
bash runMe.sh ../../../Matrices/1.matrix 2>&1 | tail -20
```

**Priority:** **MEDIUM** - Requires individual investigation

**Estimated Fix Time:** 1-2 hours (debugging)

---

## Recommended Action Plan

### Phase 1: Quick Wins (30 min)
1. ✅ Stop running benchmark (DONE)
2. Fix Brainfuck path issue
3. Bulk recompile all Exit 126 languages

### Phase 2: Install Missing Tools (15 min)
4. Install: emacs, janet, fennel, jupyter
5. Mark Red as "unsupported" (32-bit only)

### Phase 3: Individual Debugging (2 hours)
6. Debug: Dc, M4, Verilog, F#
7. Test each fix with Matrix 1 only

### Phase 4: Slow Languages (Optional - 4 hours)
8. Run Fish, Sed with extended timeouts
9. Or mark as "slow" and skip in standard benchmarks

### Phase 5: Validation (30 min)
10. Re-run benchmark with fixes
11. Generate final HTML report
12. Verify success rate >90%

---

## Success Metrics

**Target:** 80/88 languages passing (90% success rate)

**Current:** 60/88 (68%)

**After Phase 1-3:** Expected 75-78/88 (85-88%)

**Exclusions (Acceptable):**
- Red (ARM64 incompatible)
- Assembly (requires code rewrite)
- Fish, Sed (too slow, mark as "slow-tier")

---

## Next Steps

1. **Immediate:** Bulk recompile Exit 126 languages
2. **Short-term:** Install missing tools
3. **Medium-term:** Debug individual failures
4. **Long-term:** Document slow languages in CLAUDE.md

Would you like me to proceed with Phase 1 (bulk recompile)?
