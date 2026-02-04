# =============================================================================
# Sudoku Solver Multi-Language Benchmark - Docker Image
# =============================================================================
# This Dockerfile installs 85+ programming language toolchains for running
# the Sudoku benchmark suite.
#
# Build: docker build -t sudoku-benchmark .
# Run:   docker run -it sudoku-benchmark bash
#
# Note: This is a large image (~20GB+) due to the number of language toolchains.
# Build time can be significant (30-60+ minutes depending on network/hardware).
# =============================================================================

FROM ubuntu:24.04

LABEL maintainer="SudokuSolver Benchmark Suite"
LABEL description="Multi-language benchmark environment with 85+ language toolchains"

# Prevent interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# Set locale
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# =============================================================================
# STAGE 1: System packages and build essentials
# =============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
    # Build essentials
    build-essential \
    gcc \
    g++ \
    gfortran \
    make \
    cmake \
    autoconf \
    automake \
    libtool \
    pkg-config \
    # Version control
    git \
    curl \
    wget \
    ca-certificates \
    gnupg \
    # Archive tools
    unzip \
    zip \
    xz-utils \
    bzip2 \
    # Utilities
    time \
    timeout \
    procps \
    file \
    locales \
    software-properties-common \
    apt-transport-https \
    # Libraries
    libssl-dev \
    libffi-dev \
    zlib1g-dev \
    libreadline-dev \
    libsqlite3-dev \
    libncurses5-dev \
    libncursesw5-dev \
    libbz2-dev \
    liblzma-dev \
    libgdbm-dev \
    libgmp-dev \
    libmpfr-dev \
    libmpc-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libxslt1-dev \
    libyaml-dev \
    libpcre3-dev \
    libglib2.0-dev \
    libgtk-3-dev \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 2: Shell interpreters
# =============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
    # BASH (already installed, but ensure latest)
    bash \
    # Dash (POSIX shell)
    dash \
    # Ksh (Korn shell)
    ksh \
    # Tcsh (C shell variant)
    tcsh \
    # Zsh
    zsh \
    # Fish shell
    fish \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 3: Scripting languages
# =============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
    # Python 3
    python3 \
    python3-pip \
    python3-venv \
    python3-dev \
    # Perl
    perl \
    # Ruby
    ruby \
    ruby-dev \
    # Lua
    lua5.4 \
    liblua5.4-dev \
    # PHP
    php-cli \
    # Awk (gawk)
    gawk \
    mawk \
    # Sed
    sed \
    # Tcl
    tcl \
    tcl-dev \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 4: Node.js ecosystem (JavaScript, TypeScript, CoffeeScript)
# =============================================================================

RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g \
        typescript \
        ts-node \
        coffeescript \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 5: JVM languages (Java, Kotlin, Scala, Clojure, Groovy)
# =============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
    openjdk-21-jdk \
    && rm -rf /var/lib/apt/lists/*

ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
ENV PATH="$JAVA_HOME/bin:$PATH"

# Kotlin
RUN curl -sSL https://github.com/JetBrains/kotlin/releases/download/v2.0.21/kotlin-compiler-2.0.21.zip -o kotlin.zip \
    && unzip kotlin.zip -d /opt \
    && rm kotlin.zip \
    && ln -s /opt/kotlinc/bin/* /usr/local/bin/

# Scala (via Coursier)
RUN curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > /usr/local/bin/cs \
    && chmod +x /usr/local/bin/cs \
    && cs setup --yes --install-dir /usr/local/bin \
    && cs install scalac scala

# Clojure
RUN curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && chmod +x linux-install.sh \
    && ./linux-install.sh --prefix /usr/local \
    && rm linux-install.sh

# Groovy
RUN apt-get update && apt-get install -y --no-install-recommends groovy \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 6: .NET languages (C#, F#, Visual Basic)
# =============================================================================

RUN wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh \
    && chmod +x dotnet-install.sh \
    && ./dotnet-install.sh --channel 9.0 --install-dir /usr/share/dotnet \
    && rm dotnet-install.sh \
    && ln -s /usr/share/dotnet/dotnet /usr/local/bin/dotnet

ENV DOTNET_ROOT=/usr/share/dotnet
ENV PATH="$PATH:$DOTNET_ROOT"

# =============================================================================
# STAGE 7: Compiled systems languages
# =============================================================================

# Go
RUN wget https://go.dev/dl/go1.23.4.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go1.23.4.linux-amd64.tar.gz \
    && rm go1.23.4.linux-amd64.tar.gz

ENV PATH="$PATH:/usr/local/go/bin"
ENV GOPATH=/root/go
ENV PATH="$PATH:$GOPATH/bin"

# Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:$PATH"

# Zig
RUN wget https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz \
    && tar -xf zig-linux-x86_64-0.13.0.tar.xz -C /opt \
    && rm zig-linux-x86_64-0.13.0.tar.xz \
    && ln -s /opt/zig-linux-x86_64-0.13.0/zig /usr/local/bin/zig

# V language
RUN git clone --depth 1 https://github.com/vlang/v /opt/vlang \
    && cd /opt/vlang && make \
    && ln -s /opt/vlang/v /usr/local/bin/v

# Nim
RUN curl https://nim-lang.org/choosenim/init.sh -sSf | bash -s -- -y
ENV PATH="/root/.nimble/bin:$PATH"

# D (LDC2 - LLVM D Compiler)
RUN wget https://github.com/ldc-developers/ldc/releases/download/v1.39.0/ldc2-1.39.0-linux-x86_64.tar.xz \
    && tar -xf ldc2-1.39.0-linux-x86_64.tar.xz -C /opt \
    && rm ldc2-1.39.0-linux-x86_64.tar.xz \
    && ln -s /opt/ldc2-1.39.0-linux-x86_64/bin/* /usr/local/bin/

# Crystal
RUN curl -fsSL https://crystal-lang.org/install.sh | bash

# =============================================================================
# STAGE 8: Functional languages
# =============================================================================

# Haskell (GHC via GHCup)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
ENV PATH="/root/.ghcup/bin:$PATH"

# OCaml
RUN apt-get update && apt-get install -y --no-install-recommends opam \
    && opam init --disable-sandboxing -y \
    && eval $(opam env) \
    && rm -rf /var/lib/apt/lists/*
ENV PATH="/root/.opam/default/bin:$PATH"

# Erlang and Elixir
RUN apt-get update && apt-get install -y --no-install-recommends \
    erlang \
    elixir \
    && rm -rf /var/lib/apt/lists/*

# Racket
RUN apt-get update && apt-get install -y --no-install-recommends racket \
    && rm -rf /var/lib/apt/lists/*

# Scheme (GNU Guile)
RUN apt-get update && apt-get install -y --no-install-recommends guile-3.0 \
    && rm -rf /var/lib/apt/lists/*

# Common Lisp (SBCL)
RUN apt-get update && apt-get install -y --no-install-recommends sbcl \
    && rm -rf /var/lib/apt/lists/*

# SML (MLton and SML/NJ)
RUN apt-get update && apt-get install -y --no-install-recommends \
    mlton \
    smlnj \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 9: Additional compiled languages
# =============================================================================

# Ada (GNAT)
RUN apt-get update && apt-get install -y --no-install-recommends gnat \
    && rm -rf /var/lib/apt/lists/*

# Fortran (already installed with gfortran above)

# Pascal (Free Pascal)
RUN apt-get update && apt-get install -y --no-install-recommends fp-compiler \
    && rm -rf /var/lib/apt/lists/*

# COBOL (GnuCOBOL)
RUN apt-get update && apt-get install -y --no-install-recommends gnucobol \
    && rm -rf /var/lib/apt/lists/*

# Vala
RUN apt-get update && apt-get install -y --no-install-recommends \
    valac \
    libglib2.0-dev \
    && rm -rf /var/lib/apt/lists/*

# Assembly (NASM)
RUN apt-get update && apt-get install -y --no-install-recommends nasm \
    && rm -rf /var/lib/apt/lists/*

# Objective-C (via GCC/Clang)
RUN apt-get update && apt-get install -y --no-install-recommends \
    clang \
    gobjc \
    libobjc4 \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 10: Scientific/Data languages
# =============================================================================

# Julia
RUN wget https://julialang-s3.julialang.org/bin/linux/x64/1.11/julia-1.11.2-linux-x86_64.tar.gz \
    && tar -xzf julia-1.11.2-linux-x86_64.tar.gz -C /opt \
    && rm julia-1.11.2-linux-x86_64.tar.gz \
    && ln -s /opt/julia-1.11.2/bin/julia /usr/local/bin/julia

# R
RUN apt-get update && apt-get install -y --no-install-recommends r-base \
    && rm -rf /var/lib/apt/lists/*

# Octave (MATLAB clone)
RUN apt-get update && apt-get install -y --no-install-recommends octave \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 11: Specialized/Esoteric languages
# =============================================================================

# Prolog (SWI-Prolog)
RUN apt-get update && apt-get install -y --no-install-recommends swi-prolog \
    && rm -rf /var/lib/apt/lists/*

# Forth (GForth)
RUN apt-get update && apt-get install -y --no-install-recommends gforth \
    && rm -rf /var/lib/apt/lists/*

# Smalltalk (GNU Smalltalk)
RUN apt-get update && apt-get install -y --no-install-recommends gnu-smalltalk \
    && rm -rf /var/lib/apt/lists/*

# bc and dc (calculators)
RUN apt-get update && apt-get install -y --no-install-recommends bc dc \
    && rm -rf /var/lib/apt/lists/*

# PostScript (Ghostscript)
RUN apt-get update && apt-get install -y --no-install-recommends ghostscript \
    && rm -rf /var/lib/apt/lists/*

# XSLT (xsltproc)
RUN apt-get update && apt-get install -y --no-install-recommends xsltproc \
    && rm -rf /var/lib/apt/lists/*

# M4 macro processor
RUN apt-get update && apt-get install -y --no-install-recommends m4 \
    && rm -rf /var/lib/apt/lists/*

# jq (JSON processor)
RUN apt-get update && apt-get install -y --no-install-recommends jq \
    && rm -rf /var/lib/apt/lists/*

# Gnuplot
RUN apt-get update && apt-get install -y --no-install-recommends gnuplot-nox \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 12: More specialized languages
# =============================================================================

# Dart
RUN wget https://storage.googleapis.com/dart-archive/channels/stable/release/3.5.4/sdk/dartsdk-linux-x64-release.zip \
    && unzip dartsdk-linux-x64-release.zip -d /opt \
    && rm dartsdk-linux-x64-release.zip \
    && ln -s /opt/dart-sdk/bin/* /usr/local/bin/

# Haxe
RUN add-apt-repository ppa:haxe/releases -y \
    && apt-get update && apt-get install -y --no-install-recommends haxe neko \
    && mkdir -p /root/haxelib && haxelib setup /root/haxelib \
    && rm -rf /var/lib/apt/lists/*

# Raku (Perl 6)
RUN apt-get update && apt-get install -y --no-install-recommends rakudo \
    && rm -rf /var/lib/apt/lists/*

# PowerShell
RUN wget -q https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update && apt-get install -y --no-install-recommends powershell \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 13: Embedded/Niche languages
# =============================================================================

# Janet
RUN git clone --depth 1 https://github.com/janet-lang/janet /tmp/janet \
    && cd /tmp/janet && make && make install \
    && rm -rf /tmp/janet

# Fennel (Lisp for Lua)
RUN wget https://fennel-lang.org/downloads/fennel-1.5.1 -O /usr/local/bin/fennel \
    && chmod +x /usr/local/bin/fennel

# Wren CLI
RUN git clone --depth 1 https://github.com/wren-lang/wren-cli /tmp/wren-cli \
    && cd /tmp/wren-cli && make \
    && cp bin/wren_cli /usr/local/bin/wren \
    && rm -rf /tmp/wren-cli

# Rexx (Regina)
RUN apt-get update && apt-get install -y --no-install-recommends regina-rexx \
    && rm -rf /var/lib/apt/lists/*

# Pike
RUN apt-get update && apt-get install -y --no-install-recommends pike8.0 \
    && ln -sf /usr/bin/pike8.0 /usr/local/bin/pike \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 14: Hardware/Simulation languages
# =============================================================================

# Verilog (Icarus Verilog)
RUN apt-get update && apt-get install -y --no-install-recommends \
    iverilog \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 15: Text editor scripting
# =============================================================================

# Emacs (for EmacsLisp)
RUN apt-get update && apt-get install -y --no-install-recommends emacs-nox \
    && rm -rf /var/lib/apt/lists/*

# Vim (for Vimscript)
RUN apt-get update && apt-get install -y --no-install-recommends vim \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# STAGE 16: Icon language (build from source)
# =============================================================================

RUN wget https://www.cs.arizona.edu/icon/ftp/packages/unix/icon-v951src.tgz \
    && tar -xzf icon-v951src.tgz \
    && cd icon-v951src && make Configure name=linux && make \
    && cp bin/* /usr/local/bin/ \
    && cd .. && rm -rf icon-v951src icon-v951src.tgz

# =============================================================================
# STAGE 17: Languages requiring special handling
# =============================================================================

# Brainfuck interpreter (build simple C interpreter)
RUN echo '#include <stdio.h>\n\
#include <stdlib.h>\n\
#include <string.h>\n\
int main(int argc, char **argv) {\n\
    if (argc < 2) return 1;\n\
    FILE *f = fopen(argv[1], "r");\n\
    if (!f) return 1;\n\
    fseek(f, 0, SEEK_END);\n\
    long sz = ftell(f);\n\
    fseek(f, 0, SEEK_SET);\n\
    char *code = malloc(sz + 1);\n\
    fread(code, 1, sz, f);\n\
    code[sz] = 0;\n\
    fclose(f);\n\
    unsigned char tape[30000] = {0};\n\
    unsigned char *ptr = tape;\n\
    char *pc = code;\n\
    int loop[1000], lp = 0;\n\
    while (*pc) {\n\
        switch (*pc) {\n\
            case '>': ++ptr; break;\n\
            case '<': --ptr; break;\n\
            case '+': ++*ptr; break;\n\
            case '-': --*ptr; break;\n\
            case '.': putchar(*ptr); break;\n\
            case ',': *ptr = getchar(); break;\n\
            case '[':\n\
                if (*ptr) { loop[lp++] = pc - code; }\n\
                else { int d = 1; while (d) { pc++; if (*pc == '[') d++; if (*pc == ']') d--; } }\n\
                break;\n\
            case ']': if (*ptr) pc = code + loop[lp-1] - 1; else lp--; break;\n\
        }\n\
        pc++;\n\
    }\n\
    free(code);\n\
    return 0;\n\
}' > /tmp/bf.c \
    && gcc -O3 -o /usr/local/bin/bf /tmp/bf.c \
    && rm /tmp/bf.c

# =============================================================================
# STAGE 18: Red language (x86_64 Linux binary)
# Note: Red has limited platform support and may have issues
# =============================================================================

RUN wget https://static.red-lang.org/dl/linux/red-latest -O /usr/local/bin/red \
    && chmod +x /usr/local/bin/red || echo "Warning: Red download may have failed"

# =============================================================================
# STAGE 19: Swift (Linux version)
# Note: Swift on Linux requires specific setup
# =============================================================================

RUN wget https://download.swift.org/swift-6.0.2-release/ubuntu2404/swift-6.0.2-RELEASE/swift-6.0.2-RELEASE-ubuntu24.04.tar.gz \
    && tar -xzf swift-6.0.2-RELEASE-ubuntu24.04.tar.gz -C /opt \
    && rm swift-6.0.2-RELEASE-ubuntu24.04.tar.gz \
    && ln -s /opt/swift-6.0.2-RELEASE-ubuntu24.04/usr/bin/* /usr/local/bin/

# =============================================================================
# STAGE 20: FreeBASIC
# Note: FreeBASIC has architecture-specific builds
# =============================================================================

RUN wget https://sourceforge.net/projects/fbc/files/FreeBASIC-1.10.1/Binaries-Linux/FreeBASIC-1.10.1-linux-x86_64.tar.gz/download -O freebasic.tar.gz \
    && tar -xzf freebasic.tar.gz \
    && cd FreeBASIC-1.10.1-linux-x86_64 && ./install.sh -i /usr/local \
    && cd .. && rm -rf FreeBASIC-1.10.1-linux-x86_64 freebasic.tar.gz

# =============================================================================
# Cleanup and final setup
# =============================================================================

# Clean up apt cache
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Create app directory
WORKDIR /app

# Copy project files
COPY . /app

# Make scripts executable
RUN find /app -name "*.sh" -exec chmod +x {} \;

# Set default command
CMD ["bash"]

# =============================================================================
# Environment summary
# Languages included:
# - Shell: bash, dash, ksh, tcsh, zsh, fish, awk, sed
# - Scripting: python3, perl, ruby, lua, php, tcl
# - JavaScript: node, typescript, coffeescript
# - JVM: java, kotlin, scala, clojure, groovy
# - .NET: dotnet (C#, F#, VB.NET)
# - Systems: C, C++, go, rust, zig, v, nim, d, crystal
# - Functional: haskell, ocaml, erlang, elixir, racket, scheme, sbcl, sml
# - Scientific: julia, r, octave
# - Classic: ada, fortran, pascal, cobol, objective-c
# - Esoteric: prolog, forth, smalltalk, bc, dc, postscript, xslt, m4, jq, gnuplot
# - Specialized: dart, haxe, raku, powershell, janet, fennel, wren, rexx, pike
# - Hardware: verilog (iverilog)
# - Editor: emacs (EmacsLisp), vim (Vimscript)
# - Assembly: nasm
# - Other: icon, brainfuck, red, swift, freebasic
#
# Known issues - see Issues.html for details:
# - AppleScript: macOS only, not available in Docker
# - Red: Limited platform support, may not work reliably
# - Some languages may require additional runtime configuration
# =============================================================================
