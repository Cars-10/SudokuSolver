#!/bin/bash
cd "$(dirname "$0")"

# Add GNAT to PATH (Alire installation on macOS)
if [ -d "$HOME/.local/share/alire/toolchains" ]; then
    GNAT_DIR=$(find "$HOME/.local/share/alire/toolchains" -name "gnat_native*" -type d | head -1)
    [ -n "$GNAT_DIR" ] && export PATH="$GNAT_DIR/bin:$PATH"
fi

# Add Alire to PATH
[ -x /Users/vibe/Downloads/bin/alr ] && export PATH="/Users/vibe/Downloads/bin:$PATH"

# Configure SDK for macOS linker
export SDKROOT="/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
export LIBRARY_PATH="$SDKROOT/usr/lib"

LANGUAGE="Ada"
SOLVER_BINARY="./cp"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain gnatmake
    echo "Compiling Ada CP solver..."
    gnatmake -O3 cp.adb -o cp || return 1
}

main "$@"
