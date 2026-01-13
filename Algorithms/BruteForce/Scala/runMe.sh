#!/bin/bash
# Languages/Scala/runMe.sh

cd "$(dirname "$0")"

# Add sdkman Scala to PATH if available
[ -d /root/.sdkman/candidates/scala/current/bin ] && export PATH="/root/.sdkman/candidates/scala/current/bin:$PATH"

# Setup Java environment (macOS Homebrew)
if [ -d "/opt/homebrew/opt/openjdk" ]; then
    export JAVA_HOME="/opt/homebrew/opt/openjdk"
    export PATH="$JAVA_HOME/bin:$PATH"
fi

LANGUAGE="Scala"
SOLVER_BINARY="./run_scala.sh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain scalac

    echo "Compiling Scala..." >&2
    scalac Sudoku.scala
    if [ $? -ne 0 ]; then
        report_env_error "Scala compilation failed"
    fi
    echo "Scala compilation successful" >&2

    # Create wrapper to run with java (Scala 3 doesn't support 'scala ClassName')
    cat > run_scala.sh << 'WRAPPER'
#!/bin/bash
# Find Scala library path
if [ -d /root/.sdkman/candidates/scala/current/lib ]; then
    SCALA_LIB="/root/.sdkman/candidates/scala/current/lib"
elif [ -d /opt/homebrew/opt/scala/libexec/lib ]; then
    SCALA_LIB="/opt/homebrew/opt/scala/libexec/lib"
else
    SCALA_LIB="$(dirname $(which scalac))/../lib"
fi
java -cp ".:$SCALA_LIB/*" Sudoku "$@"
WRAPPER
    chmod +x run_scala.sh
}

main "$@"
