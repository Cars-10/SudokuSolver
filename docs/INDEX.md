# Sudoku Benchmark Suite Documentation

Welcome to the documentation for the multi-language Sudoku Benchmark Suite. This project benchmarks 88+ programming language implementations of an identical brute-force backtracking algorithm.

## Documentation Index

### Getting Started

| Document | Description |
|----------|-------------|
| [QUICKSTART.md](QUICKSTART.md) | Get the benchmark UI running in under 2 minutes |
| [../README.md](../README.md) | Project overview and comprehensive documentation |

### Development Guides

| Document | Description |
|----------|-------------|
| [ADDING_LANGUAGES.md](ADDING_LANGUAGES.md) | How to add a new language implementation, test suite details, and metrics capture |
| [../CLAUDE.md](../CLAUDE.md) | Claude Code project instructions and common commands |

### Architecture & Philosophy

| Document | Description |
|----------|-------------|
| [MANIFESTO.md](MANIFESTO.md) | Project philosophy, workflows, and UI architecture |
| [../Algorithms/BruteForce/C/ALGORITHM.md](../Algorithms/BruteForce/C/ALGORITHM.md) | Canonical algorithm specification with pseudocode |

### Language-Specific Documentation

Each language implementation has its own README in `Algorithms/BruteForce/{Language}/README.md` covering:
- Language-specific setup requirements
- Compilation/interpretation notes
- Known issues or workarounds

### Algorithm Types

| Algorithm | Description | Location |
|-----------|-------------|----------|
| BruteForce | Standard backtracking (88+ languages) | `Algorithms/BruteForce/` |
| DLX | Dancing Links / Algorithm X | `Algorithms/DLX/` |
| CP | Constraint Propagation | `Algorithms/CP/` |

---

## Quick Reference

### Start the UI

```bash
# One-command launch (recommended)
./runSudokuBenchmark_ui.sh

# Development mode with hot reload
./runSudokuBenchmark_ui.sh --dev

# Using Docker
./scripts/server_control.sh start
```

### Run Benchmarks

```bash
# Single language
./runBenchmarks.sh Python

# All languages
./runBenchmarks.sh --all

# Check status
./runBenchmarks.sh --status
```

### Reference Iteration Counts

All implementations must produce these exact iteration counts:

| Matrix | Iterations | Notes |
|--------|-----------|-------|
| 1 | 656 | Easiest - use for testing |
| 2 | 439,269 | Medium |
| 3 | 98,847 | Medium |
| 4 | 9,085 | Easy |
| 5 | 445,778 | Medium |
| 6 | 622,577,597 | Hard - may timeout |

---

## Need Help?

- **Adding a language?** → [ADDING_LANGUAGES.md](ADDING_LANGUAGES.md)
- **Running the UI?** → [QUICKSTART.md](QUICKSTART.md)
- **Understanding the project?** → [MANIFESTO.md](MANIFESTO.md)
- **Algorithm details?** → [../Algorithms/BruteForce/C/ALGORITHM.md](../Algorithms/BruteForce/C/ALGORITHM.md)
