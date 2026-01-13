# Technology Stack

## Core Components

### Languages (Benchmarked)
- **Primary:** C, Python, Rust, Go, Java, JavaScript, TypeScript
- **Others:** 15+ Tier 1 languages, potentially 80+ total (Ada to Zsh)
- **Constraint:** All implementations are standalone, minimal dependency

### Orchestration & Infrastructure
- **Scripting:** Bash 4.x+ (`runMeGlobal.sh`, `Languages/common.sh`)
- **Containerization:** Docker (custom `sudoku-benchmark` image)
- **Timekeeping:** Python (high-precision timer used in `common.sh`)

### Metrics & Reporting
- **Runtime:** Node.js (v18+)
- **Language:** TypeScript
- **Output:** HTML5 (Single file report), JSON (Data storage)

### Data
- **Input:** `.matrix` files (simple text grids)
- **Storage:** JSON (`metrics.json`, `metadata.json`, `session_state.json`)
- **Database:** SQLite (`benchmark_history.db` - *Found in file list*)

## Development Tools
- **Version Control:** Git
- **Package Manager:** npm (for Metrics/)
