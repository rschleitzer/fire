# Fire - FHIR Server in Rust

A high-performance FHIR (Fast Healthcare Interoperability Resources) server implementation in Rust.

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

### Search Parameters

- `name` - Search by family or given name (partial match)
- `family` - Search by family name
- `given` - Search by given name
- `identifier` - Search by identifier value
- `birthdate` - Search by birthdate (supports prefixes: eq, ne, gt, lt, ge, le)
- `gender` - Search by gender
- `active` - Search by active status (true/false)
- `_count` - Page size (default: 50, max: 1000)
- `_offset` - Pagination offset

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
```bash
curl "http://localhost:3000/fhir/Patient?name=Smith&birthdate=gt1980-01-01"
```

## Development

### Build
```bash
cargo build
```

### Run tests
```bash
cargo test
```

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
│   │   └── patient.rs
│   ├── repository/          # Database operations
│   │   └── patient.rs
│   └── search/              # Search query builder
│       └── mod.rs
├── migrations/              # SQL migrations
│   └── 001_create_patient_tables.sql
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

## Next Steps

- Phase 2: Advanced search with modifiers
- Phase 3: Additional resources (Observation, MedicationRequest)
- Phase 4: Transaction bundles
- Phase 5: Production readiness (auth, logging, testing)

## License

MIT