# Contributing to Fire FHIR Server

Thank you for your interest in contributing to Fire! This document provides guidelines and instructions for contributing.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Pull Request Process](#pull-request-process)
- [Adding New Resources](#adding-new-resources)

## Code of Conduct

This project follows the Rust Code of Conduct. Please be respectful and constructive in all interactions.

## Getting Started

### Prerequisites

- Rust 1.75 or later
- PostgreSQL 14 or later
- Git
- Basic understanding of FHIR (Fast Healthcare Interoperability Resources)

### Development Setup

1. **Clone the repository:**
   ```bash
   git clone https://github.com/yourusername/fire.git
   cd fire
   ```

2. **Install dependencies:**
   ```bash
   rustup update
   cargo install sqlx-cli --no-default-features --features postgres
   ```

3. **Set up database:**
   ```bash
   createdb fhir
   createdb fhir_test
   ```

4. **Configure environment:**
   ```bash
   cp .env.example .env
   # Edit .env with your database credentials
   ```

5. **Run migrations:**
   ```bash
   sqlx migrate run
   ```

6. **Build and run:**
   ```bash
   cargo build
   cargo run
   ```

## Project Structure

```
fire/
├── src/
│   ├── api/                    # HTTP handlers and routes
│   │   ├── handlers/          # Request handlers
│   │   │   ├── patient.rs     # Patient CRUD operations
│   │   │   ├── observation.rs # Observation CRUD operations
│   │   │   ├── bundle.rs      # Transaction bundle handling
│   │   │   ├── health.rs      # Health check endpoints
│   │   │   └── metadata.rs    # Capability statement
│   │   ├── routes.rs          # Route definitions
│   │   └── mod.rs
│   ├── models/                # Data models
│   │   ├── patient.rs         # Patient model & search params
│   │   ├── observation.rs     # Observation model & search params
│   │   ├── traits.rs          # Common traits
│   │   └── mod.rs
│   ├── repository/            # Database operations
│   │   ├── patient.rs         # Patient repository
│   │   ├── observation.rs     # Observation repository
│   │   └── mod.rs
│   ├── services/              # Business logic
│   │   ├── validation.rs      # FHIR validation
│   │   └── mod.rs
│   ├── search/                # Search query building
│   │   └── mod.rs
│   ├── middleware/            # HTTP middleware
│   │   ├── request_id.rs      # Request ID tracking
│   │   ├── logging.rs         # Request logging
│   │   └── mod.rs
│   ├── config.rs              # Configuration
│   ├── error.rs               # Error types
│   ├── lib.rs                 # Library root
│   └── main.rs                # Application entry point
├── migrations/                # Database migrations
├── tests/                     # Integration tests
├── Dockerfile                 # Docker image definition
├── docker-compose.yml         # Docker Compose configuration
└── README.md                  # Main documentation
```

## Development Workflow

### 1. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/your-bug-fix
```

### 2. Make Changes

- Write clean, documented code
- Follow Rust idioms and best practices
- Add tests for new functionality
- Update documentation as needed

### 3. Run Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_name

# Run with output
cargo test -- --nocapture
```

### 4. Check Code Quality

```bash
# Format code
cargo fmt

# Check formatting
cargo fmt -- --check

# Run clippy
cargo clippy -- -D warnings

# Run all checks
cargo fmt -- --check && cargo clippy -- -D warnings && cargo test
```

### 5. Commit Changes

Use conventional commit messages:

```bash
git commit -m "feat: add search by date for Observation"
git commit -m "fix: correct version increment in update operation"
git commit -m "docs: update README with new endpoints"
git commit -m "test: add integration tests for bundles"
```

Commit types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

## Coding Standards

### Rust Style

Follow the [Rust Style Guide](https://doc.rust-lang.org/nightly/style-guide/):

```rust
// Good: Clear, documented function
/// Create a new patient resource
pub async fn create(&self, content: Value) -> Result<Patient> {
    tracing::info!("Creating new patient");

    // Validate resource
    validate_patient(&content)?;

    // Rest of implementation...
}

// Good: Descriptive variable names
let patient_id = Uuid::new_v4();
let last_updated = Utc::now();

// Bad: Unclear names
let id = Uuid::new_v4();
let ts = Utc::now();
```

### Error Handling

Always use the `Result` type and provide context:

```rust
// Good: Descriptive errors
fn validate_patient(content: &Value) -> Result<()> {
    let resource_type = content
        .get("resourceType")
        .and_then(|v| v.as_str())
        .ok_or_else(|| FhirError::ValidationError(
            "Missing resourceType field".to_string()
        ))?;

    if resource_type != "Patient" {
        return Err(FhirError::ValidationError(
            format!("Expected Patient, got {}", resource_type)
        ));
    }

    Ok(())
}
```

### Logging

Use structured logging with context:

```rust
// Good: Structured logging
tracing::info!(
    patient_id = %id,
    version = version_id,
    "Patient created successfully"
);

// Good: Error logging
tracing::error!(
    error = %e,
    patient_id = %id,
    "Failed to create patient"
);
```

### Documentation

Document all public APIs:

```rust
/// Create a new patient resource (version 1)
///
/// # Arguments
/// * `content` - JSON content of the patient resource
///
/// # Returns
/// * `Result<Patient>` - Created patient with assigned ID and version
///
/// # Errors
/// * `FhirError::ValidationError` - If resource validation fails
/// * `FhirError::DatabaseError` - If database operation fails
pub async fn create(&self, content: Value) -> Result<Patient> {
    // Implementation...
}
```

## Testing

### Test Structure

```rust
#[tokio::test]
async fn test_create_patient() {
    // Arrange
    let pool = common::setup_test_db().await;
    let repo = PatientRepository::new(pool.clone());
    let patient_json = common::test_patient_json();

    // Act
    let patient = repo.create(patient_json).await.expect("Failed to create");

    // Assert
    assert_eq!(patient.version_id, 1);
    assert_eq!(patient.deleted, false);

    // Cleanup
    common::cleanup_test_db(&pool).await;
}
```

### Test Coverage

Aim for comprehensive coverage:

- ✅ Unit tests for validation logic
- ✅ Integration tests for CRUD operations
- ✅ Repository tests with real database
- ✅ Search parameter tests
- ✅ Error handling tests

### Running Specific Tests

```bash
# Run patient tests only
cargo test patient_tests

# Run observation tests only
cargo test observation_tests

# Run a specific test
cargo test test_create_patient

# Run tests with logging
RUST_LOG=debug cargo test -- --nocapture
```

## Pull Request Process

1. **Update Documentation**
   - Update README.md if adding features
   - Add/update API documentation
   - Update CHANGELOG.md

2. **Ensure Tests Pass**
   ```bash
   cargo test
   cargo clippy -- -D warnings
   cargo fmt -- --check
   ```

3. **Create Pull Request**
   - Clear title describing the change
   - Detailed description of what and why
   - Link related issues
   - Add screenshots if UI changes

4. **PR Template**
   ```markdown
   ## Description
   Brief description of changes

   ## Type of Change
   - [ ] Bug fix
   - [ ] New feature
   - [ ] Breaking change
   - [ ] Documentation update

   ## Testing
   - [ ] Unit tests added/updated
   - [ ] Integration tests added/updated
   - [ ] Manual testing performed

   ## Checklist
   - [ ] Code follows project style guidelines
   - [ ] Self-review completed
   - [ ] Documentation updated
   - [ ] Tests pass locally
   ```

5. **Review Process**
   - Address review comments
   - Keep PR focused and small
   - Rebase on main if needed

## Adding New Resources

To add a new FHIR resource (e.g., Encounter):

### 1. Database Migration

Create `migrations/00X_create_encounter_tables.sql`:

```sql
CREATE TABLE encounter (
    id UUID PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted BOOLEAN NOT NULL DEFAULT FALSE,
    content JSONB NOT NULL,
    -- Search parameters
    status TEXT,
    class_code TEXT,
    patient_reference TEXT,
    -- Add indexes
);

CREATE TABLE encounter_history (
    -- Same structure plus:
    history_operation TEXT NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

### 2. Model

Create `src/models/encounter.rs`:

```rust
use serde_json::Value;
use uuid::Uuid;

#[derive(Debug, Clone, FromRow)]
pub struct Encounter {
    pub id: Uuid,
    pub version_id: i32,
    pub content: Value,
    pub status: Option<String>,
    // ... search parameters
}

pub fn extract_encounter_search_params(content: &Value) -> EncounterSearchParams {
    // Extract search parameters
}
```

### 3. Validation

Add to `src/services/validation.rs`:

```rust
pub fn validate_encounter(content: &Value) -> Result<()> {
    // Validate required fields
    // Validate enumerations
    // Validate references
}
```

### 4. Repository

Create `src/repository/encounter.rs`:

```rust
impl EncounterRepository {
    pub async fn create(&self, content: Value) -> Result<Encounter> { }
    pub async fn read(&self, id: &Uuid) -> Result<Encounter> { }
    pub async fn update(&self, id: &Uuid, content: Value) -> Result<Encounter> { }
    pub async fn delete(&self, id: &Uuid) -> Result<()> { }
    pub async fn search(&self, params: &HashMap<String, String>) -> Result<Vec<Encounter>> { }
    pub async fn history(&self, id: &Uuid) -> Result<Vec<EncounterHistory>> { }
}
```

### 5. Handlers

Create `src/api/handlers/encounter.rs`:

```rust
pub async fn create_encounter(
    State(repo): State<SharedEncounterRepo>,
    Json(content): Json<Value>,
) -> Result<(StatusCode, Json<Value>)> { }

// Add other handlers...
```

### 6. Routes

Update `src/api/routes.rs`:

```rust
pub fn encounter_routes(repo: SharedEncounterRepo) -> Router {
    Router::new()
        .route("/fhir/Encounter", get(search_encounters).post(create_encounter))
        .route("/fhir/Encounter/:id", get(read_encounter).put(update_encounter).delete(delete_encounter))
        .with_state(repo)
}
```

### 7. Tests

Create `tests/encounter_tests.rs`:

```rust
#[tokio::test]
async fn test_create_encounter() { }

#[tokio::test]
async fn test_search_encounter_by_patient() { }

// Add comprehensive tests...
```

### 8. Documentation

Update:
- README.md with new endpoints
- Capability statement in metadata handler
- DEPLOYMENT.md if needed

## Questions or Issues?

- Open an issue for bugs or feature requests
- Start a discussion for questions
- Check existing issues before creating new ones

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
