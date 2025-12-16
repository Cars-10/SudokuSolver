FROM node:18-bullseye

# Install Python 3, Chromium, and build utils
RUN apt-get update && apt-get install -y \
    python3 \
    build-essential \
    chromium \
    time \
    fonts-ipafont-gothic fonts-wqy-zenhei fonts-thai-tlwg fonts-kacst fonts-freefont-ttf libxss1 \
    && rm -rf /var/lib/apt/lists/*

# Puppeteer config
ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
ENV PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium

# Install TypeScript support and tsx
RUN npm install -g typescript ts-node tsx

WORKDIR /app/server

# Copy package files first for caching
COPY server/package.json ./
RUN npm install

# Symlink node_modules to root so siblings can share dependencies
RUN ln -s /app/server/node_modules /app/node_modules

# Copy server code
COPY server/ .

# We don't copy Matrices here because we will mount them
# This allows live editing of scripts/matrices without rebuilding

EXPOSE 3000

CMD ["node", "index.js"]
