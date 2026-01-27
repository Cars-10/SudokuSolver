# Quick Start Guide

Get the Sudoku Benchmark UI running in under 2 minutes.

## Prerequisites

- **Node.js** 18+ (check with `node --version`)
- **npm** (comes with Node.js)

## One-Command Launch

The easiest way to start the benchmark UI:

```bash
./runSudokuBenchmark_ui.sh
```

This will:
1. Install all dependencies (if needed)
2. Build the UI
3. Start the server on **http://localhost:9002**

### Development Mode

For live reloading during development:

```bash
./runSudokuBenchmark_ui.sh --dev
```

This starts both servers:
- **http://localhost:5173** - UI with hot reload
- **http://localhost:9002** - API server

### Other Options

```bash
./runSudokuBenchmark_ui.sh --help    # Show all options
./runSudokuBenchmark_ui.sh --build   # Build only, don't start server
```

---

## Manual Setup (Alternative)

If you prefer manual control:

```bash
# 1. Clone the repository
git clone https://github.com/yourrepo/SudokuSolver.git
cd SudokuSolver

# 2. Install dependencies
npm install

# 3. Install server dependencies
cd server && npm install && cd ..

# 4. Build the UI
npm run build

# 5. Start the server
cd server && npm start
```

## Access the UI

Open your browser to: **http://localhost:9002**

You should see the benchmark report with:
- Performance table showing all 88+ language implementations
- Interactive charts (bar, scatter, heatmap, etc.)
- Algorithm filtering (BruteForce, DLX)
- Language details modals

---

## Running Benchmarks

### Run a Single Language
```bash
cd Algorithms/BruteForce/Python
./runMe.sh
```

### Run All Benchmarks
```bash
./runBenchmarks.sh --all
```

### Using Docker (Recommended for Full Suite)
```bash
# Start container with all 88+ language toolchains
docker-compose up -d

# Run benchmarks inside container
docker-compose exec app bash
cd /app/Algorithms/BruteForce/C && ./runMe.sh
```

---

## Common Commands

| Command | Description |
|---------|-------------|
| `./runSudokuBenchmark_ui.sh` | **Start the UI server** |
| `./runSudokuBenchmark_ui.sh --dev` | Development mode with hot reload |
| `./runBenchmarks.sh --status` | Show benchmark status |
| `./runBenchmarks.sh --all` | Run all benchmarks |
| `./runBenchmarks.sh Python` | Run single language benchmark |

---

## Troubleshooting

### "No benchmark data available"
The server needs metrics data. Either:
- Run some benchmarks: `./runBenchmarks.sh Python`
- Or use existing metrics.json files in `Algorithms/BruteForce/*/`

### Port already in use
```bash
# Kill existing process on port 9002
lsof -ti:9002 | xargs kill -9
```

### macOS: Missing gtime/gtimeout
```bash
brew install gnu-time coreutils
```

---

## Project Structure

```
SudokuSolver/
├── runSudokuBenchmark_ui.sh  # <-- UI launcher script
├── runBenchmarks.sh          # Benchmark runner
├── src/                      # Vite UI source (TypeScript)
├── server/                   # Express API server
├── Algorithms/
│   ├── BruteForce/           # 88+ language implementations
│   └── DLX/                  # Dancing Links implementations
├── Matrices/                 # Test puzzles (1-6.matrix)
└── dist/                     # Built UI (after build)
```

---

## Need Help?

- See [INDEX.md](INDEX.md) for all documentation
- See [ADDING_LANGUAGES.md](ADDING_LANGUAGES.md) to add a new language
- See [MANIFESTO.md](MANIFESTO.md) for project philosophy
