# Build stage
FROM gcc:latest AS builder
WORKDIR /usr/src/sudoku
COPY Sudoku.c .
RUN gcc -O3 -o Sudoku Sudoku.c

# Runtime stage
FROM debian:stable-slim
RUN apt-get update && apt-get install -y time bc python3 && rm -rf /var/lib/apt/lists/*
WORKDIR /usr/src/sudoku
COPY --from=builder /usr/src/sudoku/Sudoku .
COPY setupAndRunMe.sh .
RUN chmod +x setupAndRunMe.sh
ENTRYPOINT ["./setupAndRunMe.sh"]
