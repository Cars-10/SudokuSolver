# Sudoku Benchmark - Polyglot Docker Image
# Base: Ubuntu 24.04 LTS (Noble Numbat)
# Contains all 15 Tier 1 language toolchains

FROM ubuntu:24.04

# Prevent interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Set timezone
ENV TZ=UTC
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# ============================================================================
# SYSTEM DEPENDENCIES
# ============================================================================
RUN apt-get update && apt-get install -y \
    # Build essentials
    build-essential \
    gcc \
    g++ \
    make \
    cmake \
    # System utilities
    curl \
    wget \
    git \
    unzip \
    zip \
    time \
    jq \
    # Libraries for image processing (Sharp/libvips + SVG support)
    libvips-dev \
    librsvg2-dev \
    # Chromium for Puppeteer
    chromium-browser \
    # Font packages for international support
    fonts-ipafont-gothic \
    fonts-wqy-zenhei \
    fonts-thai-tlwg \
    fonts-kacst \
    fonts-freefont-ttf \
    libxss1 \
    # SSL certificates
    ca-certificates \
    gnupg \
    software-properties-common \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================
# LANGUAGE TOOLCHAINS
# ============================================================================

# ----------------------------------------------------------------------------
# 1. C/C++ (via build-essential above)
# ----------------------------------------------------------------------------
# gcc --version: 13.x
# g++ --version: 13.x

# ----------------------------------------------------------------------------
# 1b. Fortran (gfortran)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    gfortran \
    && rm -rf /var/lib/apt/lists/*
# gfortran --version: 13.x

# ----------------------------------------------------------------------------
# 1c. Ada (GNAT)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    gnat \
    && rm -rf /var/lib/apt/lists/*
# gnat --version: 13.x

# ----------------------------------------------------------------------------
# 2. Python 3 (3.12+)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-dev \
    && rm -rf /var/lib/apt/lists/*
# python3 --version: 3.12.x

# ----------------------------------------------------------------------------
# 3. Node.js (20.x LTS)
# ----------------------------------------------------------------------------
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*
# node --version: v20.x.x

# Install global Node.js tools
RUN npm install -g typescript ts-node tsx@latest

# ----------------------------------------------------------------------------
# 4. Go (1.21+)
# ----------------------------------------------------------------------------
RUN GO_VERSION=1.21.5 \
    && wget https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz \
    && rm go${GO_VERSION}.linux-amd64.tar.gz
ENV PATH="/usr/local/go/bin:${PATH}"
ENV GOPATH="/root/go"
ENV PATH="${GOPATH}/bin:${PATH}"
# go version: go1.21.x

# ----------------------------------------------------------------------------
# 5. Rust (stable via rustup)
# ----------------------------------------------------------------------------
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"
# rustc --version: 1.x.x

# ----------------------------------------------------------------------------
# 6. Java (OpenJDK 21)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    openjdk-21-jdk \
    && rm -rf /var/lib/apt/lists/*
# Create architecture-agnostic symlink for JAVA_HOME
RUN ln -sf /usr/lib/jvm/java-21-openjdk-* /usr/lib/jvm/java-21-openjdk
ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"
# java --version: openjdk 21.x

# ----------------------------------------------------------------------------
# 7. Kotlin (via SDKMAN)
# ----------------------------------------------------------------------------
RUN curl -s "https://get.sdkman.io" | bash
RUN bash -c "source /root/.sdkman/bin/sdkman-init.sh && sdk install kotlin"
ENV PATH="/root/.sdkman/candidates/kotlin/current/bin:${PATH}"
# kotlinc -version: 1.9.x

# ----------------------------------------------------------------------------
# 8. Scala (via SDKMAN)
# ----------------------------------------------------------------------------
RUN bash -c "source /root/.sdkman/bin/sdkman-init.sh && sdk install scala"
ENV PATH="/root/.sdkman/candidates/scala/current/bin:${PATH}"
# scala -version: 3.x.x

# ----------------------------------------------------------------------------
# 9. C# / .NET SDK (8.0)
# ----------------------------------------------------------------------------
RUN wget https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y dotnet-sdk-8.0 \
    && rm -rf /var/lib/apt/lists/*
# dotnet --version: 8.0.x

# ----------------------------------------------------------------------------
# 10. PHP (8.3)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    php8.3-cli \
    php8.3-common \
    && rm -rf /var/lib/apt/lists/*
# php --version: 8.3.x

# ----------------------------------------------------------------------------
# 11. Ruby (3.x)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    ruby-full \
    && rm -rf /var/lib/apt/lists/*
# ruby --version: 3.x.x

# ----------------------------------------------------------------------------
# 12. Perl (5.x - already in Ubuntu)
# ----------------------------------------------------------------------------
# perl --version: 5.38.x (built-in)

# ----------------------------------------------------------------------------
# 13. Swift (5.9+) - DEFERRED
# ----------------------------------------------------------------------------
# Note: Swift requires manual download for Ubuntu 24.04
# swift --version: DEFERRED

# ============================================================================
# TIER 2 LANGUAGES (Scripting & Functional)
# ============================================================================

# ----------------------------------------------------------------------------
# 14. Lua (5.4)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    lua5.4 \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf /usr/bin/lua5.4 /usr/bin/lua
# lua -v: 5.4.x

# ----------------------------------------------------------------------------
# 15. R (4.x)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    r-base \
    && rm -rf /var/lib/apt/lists/*
# Rscript --version: 4.x.x

# ----------------------------------------------------------------------------
# 16. Julia (1.10+)
# ----------------------------------------------------------------------------
RUN JULIA_VERSION=1.10.7 \
    && ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then JULIA_ARCH="aarch64"; else JULIA_ARCH="x86_64"; fi \
    && wget -q https://julialang-s3.julialang.org/bin/linux/${JULIA_ARCH}/1.10/julia-${JULIA_VERSION}-linux-${JULIA_ARCH}.tar.gz \
    && tar xzf julia-${JULIA_VERSION}-linux-${JULIA_ARCH}.tar.gz -C /usr/local --strip-components=1 \
    && rm julia-${JULIA_VERSION}-linux-${JULIA_ARCH}.tar.gz
# julia --version: 1.10.x

# ----------------------------------------------------------------------------
# 17. Elixir (1.15+)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    elixir \
    && rm -rf /var/lib/apt/lists/*
# elixir --version: 1.15.x

# ----------------------------------------------------------------------------
# 18. Haskell (GHC 9.x)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    ghc \
    && rm -rf /var/lib/apt/lists/*
# ghc --version: 9.x.x

# ----------------------------------------------------------------------------
# 19. OCaml (4.14+)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    ocaml \
    && rm -rf /var/lib/apt/lists/*
# ocaml -version: 4.14.x

# ----------------------------------------------------------------------------
# 20. Groovy (via SDKMAN, already installed)
# ----------------------------------------------------------------------------
RUN bash -c "source /root/.sdkman/bin/sdkman-init.sh && sdk install groovy"
ENV PATH="/root/.sdkman/candidates/groovy/current/bin:${PATH}"
# groovy --version: 4.x.x

# ----------------------------------------------------------------------------
# 21. Dart (3.x)
# ----------------------------------------------------------------------------
RUN ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then DART_ARCH="arm64"; else DART_ARCH="x64"; fi \
    && wget -q https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-${DART_ARCH}-release.zip \
    && unzip -q dartsdk-linux-${DART_ARCH}-release.zip -d /usr/local \
    && rm dartsdk-linux-${DART_ARCH}-release.zip
ENV PATH="/usr/local/dart-sdk/bin:${PATH}"
# dart --version: 3.x.x

# ----------------------------------------------------------------------------
# 22. Crystal (1.x) - x86_64 only (no ARM64 binaries available)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    libyaml-dev libssl-dev libgmp-dev libevent-dev \
    && rm -rf /var/lib/apt/lists/*
# Crystal only has x86_64 binaries - skip on ARM64
RUN ARCH=$(dpkg --print-architecture) \
    && CRYSTAL_VERSION=1.14.0 \
    && if [ "$ARCH" != "arm64" ]; then \
         wget -q https://github.com/crystal-lang/crystal/releases/download/${CRYSTAL_VERSION}/crystal-${CRYSTAL_VERSION}-1-linux-x86_64.tar.gz \
         && tar xzf crystal-${CRYSTAL_VERSION}-1-linux-x86_64.tar.gz -C /usr/local --strip-components=1 \
         && rm crystal-${CRYSTAL_VERSION}-1-linux-x86_64.tar.gz; \
       else \
         echo "Crystal: Skipping on ARM64 (no binaries available)"; \
       fi
# crystal --version: 1.14.x (x86_64 only)

# ----------------------------------------------------------------------------
# 23. Nim (2.x)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    nim \
    && rm -rf /var/lib/apt/lists/*
# nim --version: 2.x.x

# ----------------------------------------------------------------------------
# 24. D (LDC2)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    ldc \
    && rm -rf /var/lib/apt/lists/*
# ldc2 --version: 1.x.x

# ============================================================================
# TIER 3 LANGUAGES (Diverse Paradigms)
# ============================================================================

# ----------------------------------------------------------------------------
# 25. Gawk (GNU Awk - better than mawk for complex scripts)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    gawk \
    && rm -rf /var/lib/apt/lists/*
# gawk --version: 5.x.x

# ----------------------------------------------------------------------------
# 26. Tcl (Tool Command Language)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    tcl \
    && rm -rf /var/lib/apt/lists/*
# tclsh: 8.6.x

# ----------------------------------------------------------------------------
# 27. Free Pascal
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    fpc \
    && rm -rf /var/lib/apt/lists/*
# fpc -v: 3.2.x

# ----------------------------------------------------------------------------
# 28. SWI-Prolog
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    swi-prolog \
    && rm -rf /var/lib/apt/lists/*
# swipl --version: 9.x.x

# ----------------------------------------------------------------------------
# 29. Guile (GNU Scheme)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    guile-3.0 \
    && rm -rf /var/lib/apt/lists/*
# guile --version: 3.0.x

# ----------------------------------------------------------------------------
# 30. SBCL (Steel Bank Common Lisp)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    sbcl \
    && rm -rf /var/lib/apt/lists/*
# sbcl --version: 2.x.x

# ----------------------------------------------------------------------------
# 31. Clojure (via official installer)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    rlwrap \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && chmod +x linux-install.sh \
    && ./linux-install.sh \
    && rm linux-install.sh
# clojure --version: 1.x.x

# ----------------------------------------------------------------------------
# 32. Zig (modern systems language)
# ----------------------------------------------------------------------------
RUN ZIG_VERSION=0.13.0 \
    && ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then ZIG_ARCH="aarch64"; else ZIG_ARCH="x86_64"; fi \
    && wget -q https://ziglang.org/download/${ZIG_VERSION}/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz \
    && tar xf zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz -C /usr/local \
    && ln -s /usr/local/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}/zig /usr/local/bin/zig \
    && rm zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz
# zig version: 0.13.x

# ----------------------------------------------------------------------------
# 33. GnuCOBOL
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    gnucobol \
    && rm -rf /var/lib/apt/lists/*
# cobc --version: 3.x.x

# ============================================================================
# TIER 4 LANGUAGES (Transpiled & Niche)
# ============================================================================

# ----------------------------------------------------------------------------
# 34. CoffeeScript (via npm - already have Node.js)
# ----------------------------------------------------------------------------
RUN npm install -g coffeescript
# coffee --version: 2.x.x

# ----------------------------------------------------------------------------
# 35. Racket
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    racket \
    && rm -rf /var/lib/apt/lists/*
# racket --version: 8.x.x

# ----------------------------------------------------------------------------
# 36. Raku (Rakudo)
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    rakudo \
    && rm -rf /var/lib/apt/lists/*
# raku --version: Rakudo 2024.x

# ----------------------------------------------------------------------------
# 37. GNU Octave
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    octave \
    && rm -rf /var/lib/apt/lists/*
# octave --version: 8.x.x

# ----------------------------------------------------------------------------
# 38. V Language
# ----------------------------------------------------------------------------
RUN ARCH=$(dpkg --print-architecture) \
    && V_VERSION=weekly.2025.51 \
    && if [ "$ARCH" = "arm64" ]; then V_ARCH="linux_arm64"; else V_ARCH="linux_amd64"; fi \
    && wget -q https://github.com/vlang/v/releases/download/${V_VERSION}/v_${V_ARCH}.zip \
    && unzip -q v_${V_ARCH}.zip -d /usr/local \
    && ln -s /usr/local/v/v /usr/local/bin/v \
    && rm v_${V_ARCH}.zip
# v --version: V 0.4.x

# ----------------------------------------------------------------------------
# 39. Vala
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    valac \
    libglib2.0-dev \
    && rm -rf /var/lib/apt/lists/*
# valac --version: 0.56.x

# ----------------------------------------------------------------------------
# 40. GNU Forth
# ----------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    gforth \
    && rm -rf /var/lib/apt/lists/*
# gforth --version: 0.7.x

# ----------------------------------------------------------------------------
# 41. Smalltalk - DEFERRED
# ----------------------------------------------------------------------------
# GNU Smalltalk: Not available in Ubuntu 24.04 repositories
# Pharo Smalltalk: ARM64 VM download issues on Ubuntu 24.04
# TODO: Revisit when Ubuntu packages or Pharo ARM64 binaries stabilize
# smalltalk --version: DEFERRED

# ============================================================================
# PUPPETEER CONFIGURATION
# ============================================================================
ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
ENV PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium-browser

# ============================================================================
# APPLICATION SETUP
# ============================================================================
WORKDIR /app

# Copy and install Node.js dependencies
COPY server/package*.json /app/server/
RUN cd /app/server && npm install

# Create symlink for shared node_modules
RUN ln -s /app/server/node_modules /app/node_modules

# Copy server code
COPY server/ /app/server/

# Copy metrics processing code
COPY Metrics/ /app/Metrics/

# Create directories for runtime data
RUN mkdir -p /app/Languages /app/Matrices /app/logos /app/screenshots

# ============================================================================
# RUNTIME CONFIGURATION
# ============================================================================
EXPOSE 9001

# Set environment variable for port (server/index.js should use process.env.PORT)
ENV PORT=9001

# Set working directory to app root (not /app/server)
WORKDIR /app/server

CMD ["node", "index.js"]

# ============================================================================
# TOOLCHAIN VERSIONS (for reference)
# ============================================================================
# This image contains Tier 1 + Tier 2 + Tier 3 + Tier 4 toolchains:
#
# Tier 1 (Core Languages):
# - C/C++: gcc/g++ 13.x
# - Python: 3.12.x
# - JavaScript: Node.js 20.x
# - TypeScript: Latest (via npm)
# - Go: 1.21.x
# - Rust: stable 1.x
# - Java: OpenJDK 21.x
# - Kotlin: 1.9.x
# - Scala: 3.x
# - C#: .NET 8.0.x
# - PHP: 8.3.x
# - Ruby: 3.x
# - Perl: 5.38.x (built-in)
# - Swift: DEFERRED (Ubuntu 24.04 support pending)
#
# Tier 2 (Compiled):
# - Fortran: gfortran 13.x
# - Ada: gnat 13.x
# - D: ldc 1.x
# - Nim: 2.x
# - Crystal: 1.14.x
#
# Tier 2 (Scripting & Functional):
# - Lua: 5.4.x
# - R: 4.x
# - Julia: 1.10.x
# - Elixir: 1.15.x
# - Haskell: GHC 9.x
# - OCaml: 4.14.x
# - Groovy: 4.x
# - Dart: 3.x
#
# Tier 3 (Diverse Paradigms):
# - Gawk: 5.x (text processing)
# - Tcl: 8.6.x (embeddable scripting)
# - Free Pascal: 3.2.x (classic compiled)
# - SWI-Prolog: 9.x (logic programming)
# - Guile: 3.0.x (GNU Scheme)
# - SBCL: 2.x (Common Lisp)
# - Clojure: 1.x (JVM Lisp)
# - Zig: 0.13.x (modern systems)
# - GnuCOBOL: 3.x (legacy business)
#
# Tier 4 (Transpiled & Niche):
# - CoffeeScript: 2.x (via npm, transpiles to JS)
# - Racket: 8.x (modern Scheme)
# - Raku: 2024.x (Perl 6 successor)
# - GNU Octave: 8.x (MATLAB-compatible)
# - V: 0.4.x (simple systems language)
# - Vala: 0.56.x (GNOME/GLib language)
# - GNU Forth: 0.7.x (stack-based language)
# - Smalltalk: DEFERRED (GNU/Pharo unavailable on Ubuntu 24.04 ARM64)
#
# Total: 43 languages (Swift + Smalltalk deferred, Erlang via elixir)
# Estimated image size: 15-20GB
# ============================================================================
