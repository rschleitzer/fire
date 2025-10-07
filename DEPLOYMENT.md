# Fire FHIR Server - Deployment Guide

This guide covers various deployment options for the Fire FHIR server.

## Table of Contents

- [Docker Deployment](#docker-deployment)
- [Docker Compose](#docker-compose)
- [Kubernetes Deployment](#kubernetes-deployment)
- [Production Configuration](#production-configuration)
- [Monitoring and Observability](#monitoring-and-observability)

## Docker Deployment

### Building the Docker Image

```bash
docker build -t fire-fhir-server:latest .
```

### Running with Docker

```bash
# Start PostgreSQL
docker run -d \
  --name fire-postgres \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=fhir \
  -p 5432:5432 \
  postgres:15-alpine

# Start Fire FHIR Server
docker run -d \
  --name fire-fhir \
  -e DATABASE_URL=postgres://postgres:postgres@fire-postgres:5432/fhir \
  -e SERVER_HOST=0.0.0.0 \
  -e SERVER_PORT=3000 \
  -p 3000:3000 \
  --link fire-postgres \
  fire-fhir-server:latest
```

## Docker Compose

### Local Development

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f fire

# Stop all services
docker-compose down

# Clean up volumes
docker-compose down -v
```

### Production Deployment

```bash
# Build and start in production mode
docker-compose -f docker-compose.yml up -d

# Scale the application (requires load balancer)
docker-compose up -d --scale fire=3
```

### Development with Hot Reload

```bash
# Use development configuration
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up

# This will:
# - Mount source code as volume
# - Enable debug logging
# - Use cargo run for hot reload
```

## Kubernetes Deployment

### Prerequisites

- Kubernetes cluster (1.24+)
- kubectl configured
- Helm (optional)

### Database Setup

Create a PostgreSQL instance:

```yaml
# postgres-deployment.yaml
apiVersion: v1
kind: Secret
metadata:
  name: postgres-secret
type: Opaque
stringData:
  POSTGRES_USER: postgres
  POSTGRES_PASSWORD: changeme
  POSTGRES_DB: fhir
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: postgres-pvc
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: postgres
spec:
  replicas: 1
  selector:
    matchLabels:
      app: postgres
  template:
    metadata:
      labels:
        app: postgres
    spec:
      containers:
      - name: postgres
        image: postgres:15-alpine
        envFrom:
        - secretRef:
            name: postgres-secret
        ports:
        - containerPort: 5432
        volumeMounts:
        - name: postgres-storage
          mountPath: /var/lib/postgresql/data
        livenessProbe:
          exec:
            command:
            - pg_isready
            - -U
            - postgres
          initialDelaySeconds: 30
          periodSeconds: 10
      volumes:
      - name: postgres-storage
        persistentVolumeClaim:
          claimName: postgres-pvc
---
apiVersion: v1
kind: Service
metadata:
  name: postgres
spec:
  selector:
    app: postgres
  ports:
  - port: 5432
    targetPort: 5432
```

### Fire FHIR Server Deployment

```yaml
# fire-deployment.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: fire-config
data:
  SERVER_HOST: "0.0.0.0"
  SERVER_PORT: "3000"
  DB_MAX_CONNECTIONS: "50"
  DB_MIN_CONNECTIONS: "10"
  DB_CONNECTION_TIMEOUT_SECS: "30"
  DB_IDLE_TIMEOUT_SECS: "600"
  RUST_LOG: "fire=info,tower_http=info,sqlx=warn"
---
apiVersion: v1
kind: Secret
metadata:
  name: fire-secret
type: Opaque
stringData:
  DATABASE_URL: postgres://postgres:changeme@postgres:5432/fhir
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: fire-fhir-server
spec:
  replicas: 3
  selector:
    matchLabels:
      app: fire-fhir-server
  template:
    metadata:
      labels:
        app: fire-fhir-server
    spec:
      containers:
      - name: fire
        image: fire-fhir-server:latest
        imagePullPolicy: IfNotPresent
        ports:
        - containerPort: 3000
          name: http
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: fire-secret
              key: DATABASE_URL
        envFrom:
        - configMapRef:
            name: fire-config
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health/live
            port: 3000
          initialDelaySeconds: 10
          periodSeconds: 30
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 10
        startupProbe:
          httpGet:
            path: /health/live
            port: 3000
          failureThreshold: 30
          periodSeconds: 10
---
apiVersion: v1
kind: Service
metadata:
  name: fire-fhir-server
spec:
  type: LoadBalancer
  selector:
    app: fire-fhir-server
  ports:
  - port: 80
    targetPort: 3000
    protocol: TCP
```

### Deploy to Kubernetes

```bash
# Apply database deployment
kubectl apply -f postgres-deployment.yaml

# Wait for database to be ready
kubectl wait --for=condition=ready pod -l app=postgres --timeout=300s

# Apply Fire FHIR server deployment
kubectl apply -f fire-deployment.yaml

# Wait for deployment
kubectl wait --for=condition=available deployment/fire-fhir-server --timeout=300s

# Get service URL
kubectl get service fire-fhir-server
```

### Horizontal Pod Autoscaling

```yaml
# hpa.yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: fire-fhir-server-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: fire-fhir-server
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

## Production Configuration

### Environment Variables

Essential production environment variables:

```bash
# Database
DATABASE_URL=postgres://user:password@host:5432/fhir
DB_MAX_CONNECTIONS=50
DB_MIN_CONNECTIONS=10
DB_CONNECTION_TIMEOUT_SECS=30
DB_IDLE_TIMEOUT_SECS=600

# Server
SERVER_HOST=0.0.0.0
SERVER_PORT=3000

# Logging
RUST_LOG=fire=info,tower_http=info,sqlx=warn
```

### Database Performance Tuning

PostgreSQL configuration recommendations:

```ini
# postgresql.conf
shared_buffers = 256MB
effective_cache_size = 1GB
maintenance_work_mem = 64MB
checkpoint_completion_target = 0.9
wal_buffers = 16MB
default_statistics_target = 100
random_page_cost = 1.1
effective_io_concurrency = 200
work_mem = 5MB
min_wal_size = 1GB
max_wal_size = 4GB
max_connections = 100
```

### Connection Pooling

Recommended settings for different load levels:

**Low traffic (< 100 req/min):**
```
DB_MAX_CONNECTIONS=10
DB_MIN_CONNECTIONS=2
```

**Medium traffic (100-1000 req/min):**
```
DB_MAX_CONNECTIONS=25
DB_MIN_CONNECTIONS=5
```

**High traffic (> 1000 req/min):**
```
DB_MAX_CONNECTIONS=50
DB_MIN_CONNECTIONS=10
```

### Reverse Proxy Setup (nginx)

```nginx
upstream fire_backend {
    least_conn;
    server fire-1:3000;
    server fire-2:3000;
    server fire-3:3000;
}

server {
    listen 80;
    server_name fhir.example.com;

    # Redirect to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name fhir.example.com;

    ssl_certificate /etc/ssl/certs/fhir.crt;
    ssl_certificate_key /etc/ssl/private/fhir.key;

    # Security headers
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;

    location / {
        proxy_pass http://fire_backend;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
    }

    location /health {
        access_log off;
        proxy_pass http://fire_backend;
    }
}
```

## Monitoring and Observability

### Health Check Endpoints

```bash
# Liveness probe (process alive)
curl http://localhost:3000/health/live

# Readiness probe (ready to serve traffic)
curl http://localhost:3000/health/ready

# Full health check (includes database)
curl http://localhost:3000/health
```

### Logging

Structured logs with request IDs:

```bash
# Set log level
export RUST_LOG=fire=debug,tower_http=debug,sqlx=debug

# View logs with JSON parsing
docker-compose logs fire | jq '.'
```

### Prometheus Metrics (Future)

The server is designed to support Prometheus metrics:

```yaml
# Future metrics endpoint
GET /metrics
```

### Capability Statement

Discover server capabilities:

```bash
curl http://localhost:3000/fhir/metadata
```

## Backup and Recovery

### Database Backup

```bash
# Manual backup
docker exec fire-postgres pg_dump -U postgres fhir > backup_$(date +%Y%m%d_%H%M%S).sql

# Automated backups with cron
0 2 * * * docker exec fire-postgres pg_dump -U postgres fhir | gzip > /backups/fhir_$(date +\%Y\%m\%d).sql.gz
```

### Database Restore

```bash
# Restore from backup
docker exec -i fire-postgres psql -U postgres fhir < backup.sql
```

## Security Considerations

1. **Use TLS/SSL** - Always use HTTPS in production
2. **Database credentials** - Use secrets management (Kubernetes Secrets, AWS Secrets Manager)
3. **Network segmentation** - Database should not be publicly accessible
4. **Regular updates** - Keep dependencies and base images updated
5. **Rate limiting** - Implement rate limiting at reverse proxy level
6. **Audit logging** - Enable comprehensive request logging

## Troubleshooting

### Container won't start

```bash
# Check logs
docker-compose logs fire

# Check database connectivity
docker exec fire-postgres pg_isready

# Verify migrations
docker exec fire-fhir /app/fire --version
```

### Database connection issues

```bash
# Test connection from container
docker exec fire-fhir pg_isready -h postgres -U postgres

# Check connection pool settings
docker exec fire-fhir env | grep DB_
```

### Performance issues

```bash
# Check database stats
docker exec fire-postgres psql -U postgres -c "SELECT * FROM pg_stat_database WHERE datname='fhir';"

# Monitor connections
docker exec fire-postgres psql -U postgres -c "SELECT count(*) FROM pg_stat_activity WHERE datname='fhir';"
```
