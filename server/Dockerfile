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
ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
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
# 13. Swift (5.9+)
# ----------------------------------------------------------------------------
# Note: Swift download URLs are broken (404). Deferring Swift to Phase 5.
# RUN SWIFT_VERSION=5.9.2 \
#     && SWIFT_PLATFORM=ubuntu2204 \
#     && wget https://download.swift.org/swift-${SWIFT_VERSION}-release/ubuntu2204/swift-${SWIFT_VERSION}-RELEASE/swift-${SWIFT_VERSION}-RELEASE-${SWIFT_PLATFORM}.tar.gz \
#     && tar xzf swift-${SWIFT_VERSION}-RELEASE-${SWIFT_PLATFORM}.tar.gz -C /usr/local --strip-components=1 \
#     && rm swift-${SWIFT_VERSION}-RELEASE-${SWIFT_PLATFORM}.tar.gz
# swift --version: 5.9.x (deferred to Phase 5)

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
# This image contains 14 of 15 Tier 1 toolchains:
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
# - Swift: DEFERRED TO PHASE 5 (download URLs broken)
#
# Estimated image size: 8-12GB
# ============================================================================
