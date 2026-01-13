#!/bin/bash
cd "$(dirname "$0")"

# Add sdkman Scala to PATH if available
[ -d /root/.sdkman/candidates/scala/current/bin ] && export PATH="/root/.sdkman/candidates/scala/current/bin:$PATH"

# Setup Java environment (macOS Homebrew)
if [ -d "/opt/homebrew/opt/openjdk" ]; then
    export JAVA_HOME="/opt/homebrew/opt/openjdk"
    export PATH="$JAVA_HOME/bin:$PATH"
fi

LANGUAGE="Scala"
SOLVER_BINARY="./dlx_solver"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain scalac

    echo "Compiling DLX.scala..."
    scalac DLX.scala || return 1

    # Create wrapper script to run with Java (Scala 3 compiled classes)
    cat > dlx_solver << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"

# Setup Java environment (macOS Homebrew)
if [ -d "/opt/homebrew/opt/openjdk" ]; then
    export JAVA_HOME="/opt/homebrew/opt/openjdk"
    export PATH="$JAVA_HOME/bin:$PATH"
fi

# Find Scala library path
if [ -d /root/.sdkman/candidates/scala/current/lib ]; then
    SCALA_LIB="/root/.sdkman/candidates/scala/current/lib"
elif [ -d /opt/homebrew/opt/scala/libexec/lib ]; then
    SCALA_LIB="/opt/homebrew/opt/scala/libexec/lib"
else
    SCALA_LIB="$(dirname $(which scalac))/../lib"
fi
java -cp ".:$SCALA_LIB/*" DLX "$@"
EOF
    chmod +x dlx_solver

    return 0
}

main "$@"
