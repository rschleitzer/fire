# Fire - FHIR R5 Server in Rust

A high-performance FHIR R5 (Fast Healthcare Interoperability Resources) server implementation in Rust.

**FHIR Version**: R5 (5.0.0)

## Architecture

- **Web Framework**: Axum
- **Database**: PostgreSQL with JSONB
- **Table-per-resource** design with separate current/history tables
- **No ORM** - uses sqlx for type-safe queries

See [CLAUDE.md](CLAUDE.md) for detailed architectural decisions.

## Prerequisites

- Rust 1.70+ (installed via rustup)
- PostgreSQL 14+
- sqlx-cli (for migrations)

## Setup

1. **Install sqlx-cli**:
```bash
cargo install sqlx-cli --no-default-features --features postgres
```

2. **Set up PostgreSQL database**:
```bash
createdb fhir
```

3. **Configure environment** (optional):
```bash
cp .env .env.local
# Edit .env.local with your database credentials
```

Default configuration:
- Database: `postgres://postgres:postgres@localhost/fhir`
- Server: `127.0.0.1:3000`
- Database pool: 10 max connections, 2 min connections
- Connection timeout: 30 seconds
- Idle timeout: 600 seconds (10 minutes)

Environment variables:
- `DATABASE_URL` - PostgreSQL connection string
- `SERVER_HOST` - Server bind address (default: 127.0.0.1)
- `SERVER_PORT` - Server port (default: 3000)
- `DB_MAX_CONNECTIONS` - Max database connections (default: 10)
- `DB_MIN_CONNECTIONS` - Min database connections (default: 2)
- `DB_CONNECTION_TIMEOUT_SECS` - Connection timeout in seconds (default: 30)
- `DB_IDLE_TIMEOUT_SECS` - Idle connection timeout in seconds (default: 600)
- `RUST_LOG` - Log level configuration (default: fire=info,tower_http=info,sqlx=warn)

4. **Run migrations**:
Migrations are automatically run on server startup, or manually with:
```bash
sqlx migrate run
```

## Running

```bash
cargo run
```

The server will start on `http://127.0.0.1:3000`

## API Endpoints

### Patient Resource

- `POST /fhir/Patient` - Create patient
- `GET /fhir/Patient/:id` - Read patient
- `PUT /fhir/Patient/:id` - Update patient
- `DELETE /fhir/Patient/:id` - Delete patient (soft delete)
- `GET /fhir/Patient?name=Smith&birthdate=gt1990-01-01` - Search patients
- `GET /fhir/Patient/:id/_history` - Get patient history
- `GET /fhir/Patient/:id/_history/:version_id` - Get specific version

### Observation Resource

- `POST /fhir/Observation` - Create observation
- `GET /fhir/Observation/:id` - Read observation
- `PUT /fhir/Observation/:id` - Update observation
- `DELETE /fhir/Observation/:id` - Delete observation (soft delete)
- `GET /fhir/Observation?status=final&patient=123` - Search observations
- `GET /fhir/Observation/:id/_history` - Get observation history
- `GET /fhir/Observation/:id/_history/:version_id` - Get specific version

### Transaction Bundles

- `POST /fhir` - Process transaction or batch bundle

### System Endpoints

- `GET /fhir/metadata` - FHIR capability statement
- `GET /health` - Health check endpoint
- `GET /health/ready` - Readiness check
- `GET /health/live` - Liveness check

### Search Parameters

#### Patient Search
- `name` - Search by family or given name (partial match)
- `family` - Search by family name
- `given` - Search by given name
- `identifier` - Search by identifier value
- `birthdate` - Search by birthdate (supports prefixes: eq, ne, gt, lt, ge, le)
- `gender` - Search by gender
- `active` - Search by active status (true/false)

#### Observation Search
- `status` - Search by status (e.g., final, preliminary)
- `code` - Search by observation code
- `category` - Search by category code
- `patient` - Search by patient ID (e.g., patient=123 for Patient/123)
- `subject` - Search by subject reference (e.g., subject=Patient/123)
- `date` - Search by effective date (supports prefixes: eq, ne, gt, lt, ge, le)

#### Common Parameters
- `_count` - Page size (default: 50, max: 1000)
- `_offset` - Pagination offset
- `_sort` - Sort results (comma-separated, prefix with `-` for descending)
- `_total` - Include total count (`accurate`)
- `_include` - Include referenced resources (e.g., `Observation:patient`, `Observation:subject`)
- `_revinclude` - Include resources that reference the result (e.g., `Observation:patient`, `Observation:subject`)

### Search Modifiers

- `:exact` - Exact string match (e.g., `family:exact=Smith`)
- `:contains` - Partial match (default, e.g., `family:contains=Smi`)
- `:missing` - Check if field is missing (e.g., `family:missing=true`)

## Example Requests

### Create Patient
```bash
curl -X POST http://localhost:3000/fhir/Patient \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Patient",
    "name": [{
      "family": "Smith",
      "given": ["John"]
    }],
    "gender": "male",
    "birthDate": "1980-01-01"
  }'
```

### Search Patients

Basic search:
```bash
curl "http://localhost:3000/fhir/Patient?name=Smith&birthdate=gt1980-01-01"
```

With modifiers and sorting:
```bash
curl "http://localhost:3000/fhir/Patient?family:exact=Smith&_sort=-birthdate,given&_total=accurate"
```

Check for missing family name:
```bash
curl "http://localhost:3000/fhir/Patient?family:missing=true"
```

Search patients with their observations:
```bash
curl "http://localhost:3000/fhir/Patient?name=Smith&_revinclude=Observation:patient"
```

### Search Observations

Basic search:
```bash
curl "http://localhost:3000/fhir/Observation?status=final&patient=123"
```

Search by date range:
```bash
curl "http://localhost:3000/fhir/Observation?date=gt2024-01-01&date=lt2024-12-31&_sort=-date"
```

Search by code:
```bash
curl "http://localhost:3000/fhir/Observation?code=8867-4&_total=accurate"
```

Search with included Patient resources:
```bash
curl "http://localhost:3000/fhir/Observation?patient=123&_include=Observation:patient"
```

### Create Observation
```bash
curl -X POST http://localhost:3000/fhir/Observation \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Observation",
    "status": "final",
    "code": {
      "coding": [{
        "system": "http://loinc.org",
        "code": "8867-4",
        "display": "Heart rate"
      }]
    },
    "subject": {
      "reference": "Patient/123"
    },
    "effectiveDateTime": "2024-01-15T10:30:00Z",
    "valueQuantity": {
      "value": 72,
      "unit": "beats/minute",
      "system": "http://unitsofmeasure.org",
      "code": "/min"
    }
  }'
```

### Transaction Bundle

Create multiple resources in a single transaction:
```bash
curl -X POST http://localhost:3000/fhir \
  -H "Content-Type: application/json" \
  -d '{
    "resourceType": "Bundle",
    "type": "transaction",
    "entry": [
      {
        "request": {
          "method": "POST",
          "url": "Patient"
        },
        "resource": {
          "resourceType": "Patient",
          "name": [{"family": "Smith", "given": ["John"]}],
          "gender": "male",
          "birthDate": "1980-01-01"
        }
      },
      {
        "request": {
          "method": "POST",
          "url": "Observation"
        },
        "resource": {
          "resourceType": "Observation",
          "status": "final",
          "code": {
            "coding": [{
              "system": "http://loinc.org",
              "code": "8867-4",
              "display": "Heart rate"
            }]
          },
          "subject": {"reference": "Patient/123"},
          "valueQuantity": {
            "value": 72,
            "unit": "beats/minute"
          }
        }
      }
    ]
  }'
```

## Development

### Build

The project uses SQLx with compile-time query verification. You can build in two ways:

**Option 1: With database (default)**
```bash
# Start PostgreSQL
docker compose up postgres -d

# Build (queries verified against database)
cargo build
```

**Option 2: Offline mode (no database required)**
```bash
# Build using cached query metadata from .sqlx/ directory
SQLX_OFFLINE=true cargo build
```

To regenerate the offline query metadata after modifying SQL queries:
```bash
export DATABASE_URL="postgres://postgres:postgres@localhost:5432/fhir"
cargo sqlx prepare --workspace
```

The `.sqlx/` directory contains cached query metadata and should be committed to version control.

### Run tests

Tests require a test database. Create one first:
```bash
createdb fhir_test
```

Set the test database URL (optional, defaults to fhir_test):
```bash
export DATABASE_URL=postgres://postgres:postgres@localhost/fhir_test
```

Run all tests:
```bash
cargo test
```

Run specific test suite:
```bash
cargo test patient_tests
cargo test observation_tests
```

Run tests with output:
```bash
cargo test -- --nocapture
```

The test suite includes:
- **Unit tests**: Validation, search parameter extraction
- **Integration tests**: CRUD operations for Patient and Observation
- **Repository tests**: Database operations, versioning, history
- **Search tests**: Parameter filtering, sorting, pagination

### Check code
```bash
cargo clippy
cargo fmt --check
```

## Project Structure

```
fire/
├── src/
│   ├── main.rs              # Server entry point
│   ├── lib.rs               # Library root
│   ├── config.rs            # Configuration
│   ├── error.rs             # Error handling
│   ├── api/                 # HTTP handlers
│   │   ├── handlers/
│   │   │   └── patient.rs
│   │   └── routes.rs
│   ├── models/              # Data models
│   │   ├── traits.rs
│   │   ├── patient.rs
│   │   └── observation.rs
│   ├── repository/          # Database operations
│   │   ├── patient.rs
│   │   └── observation.rs
│   ├── search/              # Search query builder
│   │   └── mod.rs
│   └── services/            # Business logic
│       └── validation.rs    # FHIR validation
├── migrations/              # SQL migrations
│   ├── 001_create_patient_tables.sql
│   └── 002_create_observation_tables.sql
├── Cargo.toml
└── CLAUDE.md               # Architecture documentation
```

## Current Status

✅ **Phase 1 Complete** - Core Foundation
- Project setup with Cargo
- Database connection with sqlx
- Patient resource (current + history tables)
- CRUD operations for Patient
- Basic search (name, birthdate, identifier, gender, active)

✅ **Phase 2 Complete** - Advanced Search & Validation
- Search modifiers: `:exact`, `:contains`, `:missing`
- `_sort` parameter with multiple fields and directions
- `_total` parameter for accurate counts
- Optimized query building with proper SQL injection protection
- FHIR resource validation:
  - Resource type validation
  - Required field validation
  - Data type validation (gender, dates, etc.)
  - Structure validation (names, identifiers)
  - Comprehensive test coverage

✅ **Phase 3 Complete** - Observation Resource
- Full Observation resource implementation
- CRUD operations (create, read, update, delete)
- Rich search parameter extraction:
  - Status, category, code
  - Subject/patient references
  - Effective dates and periods
  - Multiple value types (quantity, codeable concept, string)
- Observation-specific validation
- Comprehensive database schema with optimized indexes
- History tracking with version management

✅ **Phase 4 Complete** - Advanced Search Features
- Observation search with multiple parameters:
  - Status, code, category, patient, subject, date
- `_include` parameter for including referenced Patient resources
- Support for `_sort`, `_count`, `_offset`, `_total` parameters
- FHIR-compliant search bundle responses

✅ **Phase 5 Complete** - Transaction Bundles & Reverse Includes
- Transaction bundle support for atomic multi-resource operations
- Batch bundle support for non-atomic operations
- Support for POST, PUT, GET, DELETE within bundles
- `_revinclude` parameter for reverse reference inclusion
- Observation:patient and Observation:subject reverse includes

✅ **Phase 6 Complete** - Production Readiness & Testing
- Comprehensive structured logging with tracing
- Request ID tracking (X-Request-ID header)
- Request/response timing and status logging
- Enhanced error handling with detailed logging
- FHIR OperationOutcome responses for all errors
- Configurable log levels via environment variables
- Health check endpoints (health, readiness, liveness)
- FHIR capability statement endpoint (metadata)
- Configurable database connection pooling
- Production-grade configuration management
- Comprehensive test suite:
  - Unit tests for validation and search parameters
  - Integration tests for CRUD operations
  - Repository tests for database operations
  - Search and history tests

## Production Features

### Logging

The server uses structured logging with the `tracing` framework:

- **Request tracking**: Each request gets a unique ID (X-Request-ID header)
- **Structured logs**: JSON-formatted logs with context
- **Log levels**: Configurable via `RUST_LOG` environment variable
- **Performance tracking**: Request duration logging
- **Error tracking**: Detailed error logging with stack traces

Configure logging:
```bash
export RUST_LOG=fire=info,tower_http=info,sqlx=warn
```

Log levels:
- `error` - Critical errors only
- `warn` - Warnings and errors
- `info` - General information (default)
- `debug` - Detailed debugging
- `trace` - Very verbose tracing

### Error Handling

All errors return FHIR-compliant OperationOutcome resources:
```json
{
  "resourceType": "OperationOutcome",
  "issue": [{
    "severity": "error",
    "code": "not-found",
    "diagnostics": "Resource not found"
  }]
}
```

### Health Checks

The server provides multiple health check endpoints for monitoring:

```bash
# Overall health (includes database connectivity)
curl http://localhost:3000/health

# Kubernetes readiness probe
curl http://localhost:3000/health/ready

# Kubernetes liveness probe
curl http://localhost:3000/health/live
```

### Capability Statement

The server exposes its capabilities via the FHIR metadata endpoint:

```bash
curl http://localhost:3000/fhir/metadata
```

Returns a CapabilityStatement describing:
- Supported resources (Patient, Observation)
- Supported interactions (read, create, update, delete, search)
- Available search parameters
- Transaction/batch bundle support

### Database Configuration

Connection pooling is fully configurable for production deployments:

```bash
# Example production configuration
export DB_MAX_CONNECTIONS=50
export DB_MIN_CONNECTIONS=10
export DB_CONNECTION_TIMEOUT_SECS=30
export DB_IDLE_TIMEOUT_SECS=600
```

✅ **Phase 7 Complete** - Deployment & DevOps
- Multi-stage Docker builds for optimized images
- Docker Compose for local and production deployment
- GitHub Actions CI/CD pipelines
- Kubernetes deployment manifests
- Comprehensive deployment documentation
- Health checks and monitoring integration
- Production-grade configuration examples

## Deployment

Fire FHIR Server supports multiple deployment options:

### Docker

```bash
# Build image
docker build -t fire-fhir-server .

# Run with docker-compose
docker-compose up -d
```

### Kubernetes

```bash
# Deploy to Kubernetes
kubectl apply -f k8s/

# Check status
kubectl get pods -l app=fire-fhir-server
```

See [DEPLOYMENT.md](DEPLOYMENT.md) for complete deployment guides including:
- Docker and Docker Compose
- Kubernetes with HPA
- Production configuration
- Monitoring and observability
- Backup and recovery

## Next Steps

- Phase 8: Authentication & Authorization (OAuth2, SMART on FHIR)
- Phase 9: Additional FHIR resources (Encounter, Condition, Procedure, etc.)
- Phase 10: Performance optimization and caching
- Phase 11: GraphQL API support

## License

MIT