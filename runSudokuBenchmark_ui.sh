#!/bin/bash
# =============================================================================
# Sudoku Benchmark UI Launcher
# =============================================================================
# Launches the benchmark visualization server
#
# Usage:
#   ./runSudokuBenchmark_ui.sh           # Production mode (build + serve)
#   ./runSudokuBenchmark_ui.sh --dev     # Development mode (hot reload)
#   ./runSudokuBenchmark_ui.sh --help    # Show help
#
# =============================================================================

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Change to script directory
cd "$(dirname "$0")"

# =============================================================================
# Helper Functions
# =============================================================================

print_banner() {
    echo -e "${CYAN}"
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║           🧩 Sudoku Benchmark UI Launcher 🧩                  ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

print_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  (none)      Production mode - build UI and start server on port 9002"
    echo "  --dev       Development mode - start Vite dev server with hot reload"
    echo "  --build     Build only - compile UI without starting server"
    echo "  --help      Show this help message"
    echo ""
    echo "Ports:"
    echo "  Production:  http://localhost:9002"
    echo "  Development: http://localhost:5173 (UI) + http://localhost:9002 (API)"
    echo ""
}

check_node() {
    if ! command -v node &> /dev/null; then
        echo -e "${RED}Error: Node.js is not installed${NC}"
        echo "Install Node.js 18+ from https://nodejs.org/"
        exit 1
    fi

    local version=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
    if [ "$version" -lt 18 ]; then
        echo -e "${YELLOW}Warning: Node.js 18+ recommended (you have $(node -v))${NC}"
    fi
}

install_deps() {
    echo -e "${CYAN}Checking dependencies...${NC}"

    # Root dependencies
    if [ ! -d "node_modules" ]; then
        echo "Installing root dependencies..."
        npm install
    fi

    # Server dependencies
    if [ ! -d "server/node_modules" ]; then
        echo "Installing server dependencies..."
        (cd server && npm install)
    fi

    echo -e "${GREEN}✓ Dependencies ready${NC}"
}

build_ui() {
    echo -e "${CYAN}Building UI...${NC}"
    npm run build
    echo -e "${GREEN}✓ UI built successfully${NC}"
}

start_server() {
    local port="${1:-9002}"
    echo -e "${CYAN}Starting server on port $port...${NC}"
    echo ""
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}  Sudoku Benchmark UI is running at:${NC}"
    echo -e "${GREEN}  ${CYAN}➜  http://localhost:$port${NC}"
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${YELLOW}Press Ctrl+C to stop the server${NC}"
    echo ""

    cd server && npm start
}

start_dev_mode() {
    echo -e "${CYAN}Starting development mode...${NC}"
    echo ""
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}  Development servers starting:${NC}"
    echo -e "${GREEN}  ${CYAN}➜  UI:  http://localhost:5173${NC} (with hot reload)"
    echo -e "${GREEN}  ${CYAN}➜  API: http://localhost:9002${NC}"
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo -e "${YELLOW}Press Ctrl+C to stop both servers${NC}"
    echo ""

    # Start API server in background
    (cd server && npm start) &
    SERVER_PID=$!

    # Trap to kill server on exit
    trap "kill $SERVER_PID 2>/dev/null" EXIT

    # Start Vite dev server in foreground
    npm run dev
}

# =============================================================================
# Main
# =============================================================================

print_banner
check_node

case "${1:-}" in
    --help|-h)
        print_help
        exit 0
        ;;
    --dev|-d)
        install_deps
        start_dev_mode
        ;;
    --build|-b)
        install_deps
        build_ui
        echo -e "${GREEN}Build complete! Run '$0' to start the server.${NC}"
        ;;
    "")
        install_deps
        build_ui
        start_server
        ;;
    *)
        echo -e "${RED}Unknown option: $1${NC}"
        print_help
        exit 1
        ;;
esac
