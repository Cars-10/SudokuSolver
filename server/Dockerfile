# Sudoku Benchmark - Polyglot Docker Image
# Base: Ubuntu 24.04 LTS (Noble Numbat)
# Contains all Tier 1-5 language toolchains

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
    build-essential \
    gcc \
    g++ \
    make \
    cmake \
    curl \
    wget \
    git \
    unzip \
    zip \
    time \
    jq \
    dc \
    bc \
    sed \
    vim \
    xsltproc \
    zsh \
    fish \
    ksh \
    iverilog \
    expect \
    swig \
    libvips-dev \
    librsvg2-dev \
    chromium-browser \
    fonts-ipafont-gothic \
    fonts-wqy-zenhei \
    fonts-thai-tlwg \
    fonts-kacst \
    fonts-freefont-ttf \
    libxss1 \
    ca-certificates \
    gnupg \
    software-properties-common \
    && rm -rf /var/lib/apt/lists/*

# ============================================================================ 
# LANGUAGE TOOLCHAINS
# ============================================================================ 

# ---------------------------------------------------------------------------- 
# 1. C/C++, Fortran, Ada, Objective-C
# ---------------------------------------------------------------------------- 
RUN apt-get update && apt-get install -y \
    gfortran \
    gnat \
    gobjc \
    gnustep \
    gnustep-devel \
    && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------- 
# 2. Python 3, Jupyter
# ---------------------------------------------------------------------------- 
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-dev \
    python3-venv \
    && rm -rf /var/lib/apt/lists/* \
    && pip3 install --break-system-packages jupyter nbconvert

# ---------------------------------------------------------------------------- 
# 3. Node.js (20.x LTS)
# ---------------------------------------------------------------------------- 
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/* \
    && npm install -g typescript ts-node tsx@latest \
    && npm install -g coffeescript

# ---------------------------------------------------------------------------- 
# 4. Go (1.21+)
# ---------------------------------------------------------------------------- 
RUN ARCH=$(dpkg --print-architecture) \
    && GO_VERSION=1.21.5 \
    && wget https://go.dev/dl/go${GO_VERSION}.linux-${ARCH}.tar.gz \
    && tar -C /usr/local -xzf go${GO_VERSION}.linux-${ARCH}.tar.gz \
    && rm go${GO_VERSION}.linux-${ARCH}.tar.gz
ENV PATH="/usr/local/go/bin:${PATH}"
ENV GOPATH="/root/go"
ENV PATH="${GOPATH}/bin:${PATH}"

# ---------------------------------------------------------------------------- 
# 5. Rust (stable via rustup)
# ---------------------------------------------------------------------------- 
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"

# ---------------------------------------------------------------------------- 
# 6. Java (OpenJDK 21)
# ---------------------------------------------------------------------------- 
RUN apt-get update && apt-get install -y \
    openjdk-21-jdk \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf /usr/lib/jvm/java-21-openjdk-* /usr/lib/jvm/java-21-openjdk
ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"

# ---------------------------------------------------------------------------- 
# 7. JVM Tools (Kotlin, Scala, Groovy) via SDKMAN
# ---------------------------------------------------------------------------- 
RUN curl -s "https://get.sdkman.io" | bash
RUN bash -c "source /root/.sdkman/bin/sdkman-init.sh && sdk install kotlin && sdk install scala && sdk install groovy"
ENV PATH="/root/.sdkman/candidates/kotlin/current/bin:/root/.sdkman/candidates/scala/current/bin:/root/.sdkman/candidates/groovy/current/bin:${PATH}"

# ---------------------------------------------------------------------------- 
# 8. C# / .NET SDK (8.0)
# ---------------------------------------------------------------------------- 
RUN wget https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y dotnet-sdk-8.0 \
    && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------- 
# 9. PHP, Ruby
# ---------------------------------------------------------------------------- 
RUN apt-get update && apt-get install -y \
    php8.3-cli \
    php8.3-common \
    ruby-full \
    && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------- 
# 10. Swift (6.0)
# ---------------------------------------------------------------------------- 
RUN ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then \
         SWIFT_VERSION=6.0.2 \
         && wget -q https://download.swift.org/swift-${SWIFT_VERSION}-release/ubuntu2404-aarch64/swift-${SWIFT_VERSION}-RELEASE/swift-${SWIFT_VERSION}-RELEASE-ubuntu24.04-aarch64.tar.gz \
         && tar xzf swift-${SWIFT_VERSION}-RELEASE-ubuntu24.04-aarch64.tar.gz -C /usr/local --strip-components=1 \
         && rm swift-${SWIFT_VERSION}-RELEASE-ubuntu24.04-aarch64.tar.gz; \
       fi

# ---------------------------------------------------------------------------- 
# 11. Scripting & Niche (Lua, R, Julia, Elixir, Haskell, OCaml, Dart, Nim, D, Zig)
# ---------------------------------------------------------------------------- 
RUN apt-get update && apt-get install -y \
    lua5.4 \
    r-base \
    elixir \
    ghc \
    ocaml \
    nim \
    ldc \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf /usr/bin/lua5.4 /usr/bin/lua

RUN JULIA_VERSION=1.10.7 \
    && ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then JULIA_ARCH="aarch64"; else JULIA_ARCH="x86_64"; fi \
    && wget -q https://julialang-s3.julialang.org/bin/linux/${JULIA_ARCH}/1.10/julia-${JULIA_VERSION}-linux-${JULIA_ARCH}.tar.gz \
    && tar xzf julia-${JULIA_VERSION}-linux-${JULIA_ARCH}.tar.gz -C /usr/local --strip-components=1 \
    && rm julia-${JULIA_VERSION}-linux-${JULIA_ARCH}.tar.gz

RUN ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then DART_ARCH="arm64"; else DART_ARCH="x64"; fi \
    && wget -q https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-${DART_ARCH}-release.zip \
    && unzip -q dartsdk-linux-${DART_ARCH}-release.zip -d /usr/local \
    && rm dartsdk-linux-${DART_ARCH}-release.zip
ENV PATH="/usr/local/dart-sdk/bin:${PATH}"

RUN ZIG_VERSION=0.13.0 \
    && ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then ZIG_ARCH="aarch64"; else ZIG_ARCH="x86_64"; fi \
    && wget -q https://ziglang.org/download/${ZIG_VERSION}/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz \
    && tar xf zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz -C /usr/local \
    && ln -s /usr/local/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}/zig /usr/local/bin/zig \
    && rm zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz

# ---------------------------------------------------------------------------- 
# 12. More (Tcl, Pascal, Prolog, Guile, SBCL, Clojure, GnuCOBOL, Racket, Raku, Octave, V, Vala, Forth, Smalltalk, Haxe, Rexx, Pike, Icon, Fennel, Assembly, EmacsLisp)
# ---------------------------------------------------------------------------- 
RUN apt-get update && apt-get install -y \
    tcl \
    fpc \
    swi-prolog \
    guile-3.0 \
    sbcl \
    gnucobol \
    racket \
    rakudo \
    octave \
    valac \
    libglib2.0-dev \
    gforth \
    haxe \
    neko \
    regina-rexx \
    pike8.0-core \
    icont \
    iconx \
    icon-ipl \
    nasm \
    emacs-nox \
    rlwrap \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && chmod +x linux-install.sh \
    && ./linux-install.sh \
    && rm linux-install.sh \
    && mkdir -p /usr/share/haxe/lib \
    && haxelib setup /usr/share/haxe/lib

RUN ARCH=$(dpkg --print-architecture) \
    && V_VERSION=weekly.2025.51 \
    && if [ "$ARCH" = "arm64" ]; then V_ARCH="linux_arm64"; else V_ARCH="linux_amd64"; fi \
    && wget -q https://github.com/vlang/v/releases/download/${V_VERSION}/v_${V_ARCH}.zip \
    && unzip -q v_${V_ARCH}.zip -d /usr/local \
    && ln -s /usr/local/v/v /usr/local/bin/v \
    && rm v_${V_ARCH}.zip

RUN curl -L https://fennel-lang.org/downloads/fennel-1.4.2 -o /usr/local/bin/fennel && chmod +x /usr/local/bin/fennel

# ---------------------------------------------------------------------------- 
# 13. Build from Source / Manual (Wren, Janet)
# ---------------------------------------------------------------------------- 
RUN git clone --depth 1 --branch 0.4.0 https://github.com/wren-lang/wren-cli.git /tmp/wren-cli \
    && cd /tmp/wren-cli \
    && git submodule update --init --recursive \
    && find . -name "*.make" -exec sed -i 's/-m64//g' {} \; \
    && sed -i 's/static void write(/static void wrenWrite(/g' src/cli/vm.c \
    && sed -i 's/config.writeFn = write;/config.writeFn = wrenWrite;/g' src/cli/vm.c \
    && cd projects/make \
    && make config=release_64bit \
    && mv ../../bin/wren_cli /usr/local/bin/wren \
    && chmod +x /usr/local/bin/wren \
    && rm -rf /tmp/wren-cli

RUN git clone --depth 1 https://github.com/janet-lang/janet.git /tmp/janet \
    && cd /tmp/janet \
    && make \
    && make install \
    && rm -rf /tmp/janet

# ============================================================================ 
# PUPPETEER CONFIGURATION
# ============================================================================ 
ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
ENV PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium-browser

# ============================================================================ 
# APPLICATION SETUP
# ============================================================================ 
WORKDIR /app
COPY server/package*.json /app/server/
RUN cd /app/server && npm install
RUN ln -s /app/server/node_modules /app/node_modules
COPY server/ /app/server/
COPY Metrics/ /app/Metrics/
RUN mkdir -p /app/Languages /app/Matrices /app/logos /app/screenshots

# ============================================================================ 
# RUNTIME CONFIGURATION
# ============================================================================ 
EXPOSE 9001

# Set environment variable for port
ENV PORT=9001

WORKDIR /app/server

CMD ["node", "index.js"]
