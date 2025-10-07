# Multi-stage Docker build for Fire FHIR Server

# Stage 1: Build
FROM rust:1.75-slim as builder

WORKDIR /app

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy manifests
COPY Cargo.toml Cargo.lock ./

# Copy source code
COPY src ./src
COPY migrations ./migrations

# Build for release
RUN cargo build --release

# Stage 2: Runtime
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 fire && \
    chown -R fire:fire /app

# Copy binary from builder
COPY --from=builder /app/target/release/fire /app/fire

# Copy migrations
COPY --from=builder /app/migrations /app/migrations

# Switch to non-root user
USER fire

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:3000/health/live || exit 1

# Set environment variables
ENV RUST_LOG=fire=info,tower_http=info,sqlx=warn
ENV SERVER_HOST=0.0.0.0
ENV SERVER_PORT=3000

# Run the binary
CMD ["/app/fire"]
