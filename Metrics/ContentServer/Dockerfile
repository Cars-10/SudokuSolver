FROM node:18-alpine

RUN apk add --no-cache build-base python3 bash

WORKDIR /app

COPY package.json .
RUN npm install

COPY server.js .

# Mount the repo root to /data in the container
# CleanedUp will be at /data/CleanedUp
VOLUME /data

EXPOSE 3000

CMD ["npm", "start"]
