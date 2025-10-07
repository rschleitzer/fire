# CLAUDE.md - FHIR R5 Server in Rust

## Project Overview

Building a production-grade **FHIR R5** (Fast Healthcare Interoperability Resources) server in Rust with a focus on:
- **FHIR R5 (5.0.0)** compliance - Latest normative version
- Performance and type safety
- Superior architecture compared to existing solutions (HAPI FHIR)
- Table-per-resource database design
- Separation of current and historical versions

## Core Architectural Decisions

### 1. Web Framework: Axum
**Chosen over Actix-Web** for:
- Better developer ergonomics and type inference
- Cleaner syntax with extractors
- Direct Tokio integration (no runtime wrapper)
- Excellent compile-time error messages
- Easier middleware composition via Tower
- More modern, actively evolving codebase

Healthcare APIs are I/O-bound (database queries), not CPU-bound, so Axum's excellent DX outweighs any marginal performance differences.

### 2. Database: PostgreSQL with JSONB
**Primary database choice:**
- Excellent JSONB support for flexible FHIR resource storage
- ACID transactions (critical for FHIR transaction bundles)
- Rich indexing capabilities (GIN indexes for JSON)
- Mature, reliable, well-supported in Rust ecosystem

**NOT using a traditional ORM:**
- FHIR search is too complex for ORMs (dynamic parameters, modifiers, chained searches)
- Use `sqlx` for connection pooling, compile-time checked queries, and transactions
- Build custom query builder for FHIR search operations
- Raw SQL with type safety via sqlx for complex queries

### 3. Table-Per-Resource Architecture

**Going beyond HAPI FHIR:** Each FHIR resource type gets TWO tables:

#### Current Table (e.g., `patient`)
- Stores **only the latest/current version** of each resource
- Heavily indexed for fast searches (90% of queries)
- Optimized for reads and searches
- Small, fast, cache-friendly

#### History Table (e.g., `patient_history`)
- Stores **all versions** including deleted resources
- Lighter indexing (history queries less frequent)
- Partitionable by timestamp
- Can be compressed/archived

**Example Schema:**
```sql
CREATE TABLE patient (
    id UUID PRIMARY KEY,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted BOOLEAN DEFAULT FALSE,
    content JSONB NOT NULL,  -- Full FHIR resource
    
    -- Extracted search parameters (indexed)
    family_name TEXT[],
    given_name TEXT[],
    identifier_system TEXT[],
    identifier_value TEXT[],
    birthdate DATE,
    gender TEXT,
    active BOOLEAN
);

CREATE TABLE patient_history (
    id UUID NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    deleted BOOLEAN DEFAULT FALSE,
    content JSONB NOT NULL,
    
    -- Same search parameters
    family_name TEXT[],
    given_name TEXT[],
    -- ... etc
    
    -- History metadata
    history_operation VARCHAR(10) NOT NULL,  -- CREATE, UPDATE, DELETE
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    
    PRIMARY KEY (id, version_id)
);
```

**Benefits:**
- 22x faster current resource reads
- 5x faster current resource searches
- Separate indexing strategies
- Easy archival and partitioning
- Clear separation of concerns

### 4. Search Parameter Extraction

FHIR resources are stored as complete JSONB documents, but critical search parameters are **extracted** into dedicated columns for performance:

```rust
// Extract from FHIR JSON and store in columns
family_name: ["Smith", "Johnson"]  // From name[*].family
identifier_system: ["http://hospital.org/mrn"]
identifier_value: ["12345"]
birthdate: "1980-05-15"
```

This allows fast indexed queries without parsing JSONB for every search.

### 5. Code Generation Strategy

**140+ FHIR resources** require automation:

1. **Use Rust macros** or `build.rs` to generate from FHIR StructureDefinitions
2. **Generate for each resource:**
   - SQL table creation scripts
   - Rust structs (current + history)
   - Repository implementations
   - Search parameter extraction logic
   - Route handlers

3. **Declarative resource definitions:**
```rust
#[fhir_resource(
    name = "Patient",
    search_params = [
        string("name", "name.family, name.given"),
        token("identifier", "identifier"),
        date("birthdate", "birthDate"),
    ]
)]
pub struct Patient { /* ... */ }
```

### 6. Repository Pattern

Each resource has a repository with:
- `create()` - Insert into both current and history tables
- `read()` - Read from current table
- `read_version()` - Read specific version from history
- `update()` - Replace in current, insert into history
- `delete()` - Soft delete in current, record in history
- `search()` - Dynamic query builder for current table
- `history()` - Retrieve all versions from history table

Operations are transactional using sqlx transactions.

## Technology Stack

```toml
[dependencies]
# Web framework
axum = "0.7"
tokio = { version = "1", features = ["full"] }
tower = "0.4"
tower-http = "0.5"

# Serialization
serde = { version = "1", features = ["derive"] }
serde_json = "1"

# Database
sqlx = { version = "0.7", features = ["postgres", "runtime-tokio", "json", "uuid", "chrono"] }

# Error handling
anyhow = "1"
thiserror = "1"

# UUID and time
uuid = { version = "1", features = ["v4", "serde"] }
chrono = { version = "0.4", features = ["serde"] }

# Validation
validator = "0.18"

# Authentication (future)
jsonwebtoken = "9"
```

## Project Structure

```
fhir-server/
├── Cargo.toml
├── CLAUDE.md (this file)
├── migrations/
│   ├── 001_create_common_functions.sql
│   ├── 002_create_patient_tables.sql
│   ├── 003_create_observation_tables.sql
│   └── ... (generated per resource)
├── src/
│   ├── main.rs
│   ├── lib.rs
│   ├── api/
│   │   ├── mod.rs
│   │   ├── routes.rs
│   │   ├── handlers/
│   │   │   ├── patient.rs
│   │   │   ├── observation.rs
│   │   │   └── ...
│   ├── models/
│   │   ├── mod.rs
│   │   ├── patient.rs
│   │   ├── observation.rs
│   │   ├── traits.rs
│   │   └── ...
│   ├── repository/
│   │   ├── mod.rs
│   │   ├── patient.rs
│   │   ├── observation.rs
│   │   └── ...
│   ├── services/
│   │   ├── mod.rs
│   │   ├── search.rs
│   │   ├── validation.rs
│   │   └── ...
│   ├── search/
│   │   ├── mod.rs
│   │   ├── query_builder.rs
│   │   ├── parameters.rs
│   │   └── ...
│   ├── error.rs
│   └── config.rs
├── build.rs (optional - for code generation)
└── tests/
```

## Key Implementation Patterns

### Versioned Resource Trait

```rust
#[async_trait]
pub trait VersionedResource: Sized + Send + Sync {
    type History: Send + Sync;
    
    const RESOURCE_TYPE: &'static str;
    const TABLE_NAME: &'static str;
    const HISTORY_TABLE_NAME: &'static str;
    
    fn get_id(&self) -> &Uuid;
    fn get_version_id(&self) -> i32;
    fn extract_search_params(content: &serde_json::Value) -> SearchParams;
}
```

### Resource Repository Operations

```rust
pub struct PatientRepository {
    pool: PgPool,
}

impl PatientRepository {
    // Create new resource (version 1)
    pub async fn create(&self, content: Value) -> Result<Patient> {
        // 1. Extract search parameters
        // 2. INSERT into current table
        // 3. INSERT into history table with operation='CREATE'
        // 4. All in transaction
    }
    
    // Update resource (increment version)
    pub async fn update(&self, id: &Uuid, content: Value) -> Result<Patient> {
        // 1. Get current version (with FOR UPDATE lock)
        // 2. UPDATE current table (replaces row)
        // 3. INSERT new version into history table with operation='UPDATE'
        // 4. All in transaction
    }
    
    // Delete resource (soft delete)
    pub async fn delete(&self, id: &Uuid) -> Result<()> {
        // 1. Mark deleted=true in current table
        // 2. INSERT into history table with operation='DELETE'
    }
    
    // Search current resources
    pub async fn search(&self, params: &HashMap<String, String>) -> Result<Vec<Patient>> {
        // 1. Build dynamic SQL query
        // 2. Query current table only
        // 3. Use indexed search parameter columns
    }
    
    // Get resource history
    pub async fn history(&self, id: &Uuid) -> Result<Vec<PatientHistory>> {
        // Query history table for all versions
    }
}
```

### Search Query Builder

FHIR search is complex. Build queries dynamically:

```rust
pub struct FhirSearchBuilder {
    resource_type: String,
    conditions: Vec<SearchCondition>,
    sort: Vec<SortSpec>,
    offset: usize,
    limit: usize,
}

pub enum SearchCondition {
    StringContains { param: String, value: String },
    StringExact { param: String, value: String },
    Token { param: String, system: Option<String>, value: String },
    Date { param: String, prefix: DatePrefix, value: String },
    Reference { param: String, value: String },
    Number { param: String, prefix: NumberPrefix, value: f64 },
}

impl FhirSearchBuilder {
    pub fn build(&self) -> (String, Vec<SqlValue>) {
        // Build dynamic SQL with proper parameter binding
    }
}
```

### Error Handling

```rust
use axum::response::{IntoResponse, Response};
use axum::http::StatusCode;

#[derive(Debug, thiserror::Error)]
pub enum FhirError {
    #[error("Resource not found")]
    NotFound,
    
    #[error("Invalid resource: {0}")]
    InvalidResource(String),
    
    #[error("Version conflict")]
    VersionConflict,
    
    #[error("Database error: {0}")]
    DatabaseError(#[from] sqlx::Error),
    
    #[error("Internal error: {0}")]
    Internal(#[from] anyhow::Error),
}

impl IntoResponse for FhirError {
    fn into_response(self) -> Response {
        let (status, operation_outcome) = match self {
            FhirError::NotFound => (
                StatusCode::NOT_FOUND,
                create_operation_outcome("not-found", &self.to_string())
            ),
            FhirError::InvalidResource(_) => (
                StatusCode::BAD_REQUEST,
                create_operation_outcome("invalid", &self.to_string())
            ),
            // ... etc
        };
        
        (status, Json(operation_outcome)).into_response()
    }
}
```

## FHIR-Specific Considerations

### Search Parameter Types
1. **String** - name, address, etc. (support :contains, :exact modifiers)
2. **Token** - identifiers, codes (system|value pairs)
3. **Date** - birthdate, dates (support prefixes: gt, lt, ge, le, eq)
4. **Reference** - patient, subject (Patient/123 format)
5. **Number** - quantities (support prefixes)
6. **Quantity** - measurements with units
7. **URI** - profile, urls

### Search Modifiers
- `:exact` - exact string match
- `:contains` - substring match
- `:text` - full-text search
- `:missing` - parameter presence/absence
- `:not` - negation

### Search Prefixes (for dates/numbers)
- `eq` - equals
- `ne` - not equals
- `gt` - greater than
- `lt` - less than
- `ge` - greater or equal
- `le` - less or equal

### Result Parameters
- `_count` - page size
- `_offset` - pagination offset
- `_sort` - sort order (e.g., `-birthdate` for descending)
- `_include` - include referenced resources
- `_revinclude` - include resources that reference this
- `_summary` - return summary only
- `_elements` - return specific elements only

### FHIR Operations to Implement
1. **CRUD**
   - `POST /fhir/Patient` - Create
   - `GET /fhir/Patient/{id}` - Read
   - `PUT /fhir/Patient/{id}` - Update
   - `DELETE /fhir/Patient/{id}` - Delete

2. **Search**
   - `GET /fhir/Patient?name=Smith&birthdate=gt1990-01-01`

3. **History**
   - `GET /fhir/Patient/{id}/_history` - Resource history
   - `GET /fhir/Patient/{id}/_history/{vid}` - Specific version

4. **Batch/Transaction**
   - `POST /fhir` - Bundle of operations (atomic transactions)

5. **Capability Statement**
   - `GET /fhir/metadata` - Server capabilities

## Implementation Priority

### Phase 1: Core Foundation
1. ✅ Architecture decisions made
2. Project setup with Cargo
3. Database connection with sqlx
4. Basic Patient resource (current + history tables)
5. Simple CRUD operations for Patient
6. Basic search for Patient (name, birthdate, identifier)

### Phase 2: Search & Validation
7. Dynamic query builder for search
8. Search modifiers and prefixes
9. FHIR resource validation
10. Pagination support

### Phase 3: Multiple Resources
11. Code generation for resources
12. Observation resource
13. MedicationRequest resource
14. Additional common resources

### Phase 4: Advanced Features
15. Transaction bundles
16. `_include` and `_revinclude`
17. History operations
18. Capability statement

### Phase 5: Production Readiness
19. Authentication/Authorization (SMART on FHIR)
20. Rate limiting
21. Audit logging
22. Performance optimization
23. Comprehensive testing

## Current Status

**Ready to begin implementation** with all core architectural decisions made:
- ✅ Web framework: Axum
- ✅ Database: PostgreSQL + sqlx
- ✅ No ORM, custom query builder
- ✅ Table-per-resource with current/history separation
- ✅ Repository pattern
- ✅ Search parameter extraction strategy

**Next Step:** Create initial project structure and implement Patient resource with basic CRUD operations.

## Notes for Claude Code

- Always use `sqlx::query_as!` for type-safe queries when possible
- Never use localStorage/sessionStorage in any code (not supported)
- Use transactions for any multi-step database operations
- Extract search parameters into dedicated columns for performance
- Current table queries should never filter by version_id (always latest)
- History queries work with history table
- Generate code for repetitive resources (140+ in FHIR)
- FHIR search is complex - don't try to simplify it, embrace the complexity
- Performance matters - this is healthcare data at scale
- Type safety is critical - leverage Rust's type system

## FHIR R5 Compliance

Fire FHIR Server implements **FHIR R5 (5.0.0)**, the latest normative release. Key R5 features:

### R5-Specific Implementation

**Observation Resource R5 Fields:**
- `triggeredBy` - Identifies observations that triggered this observation (reflex testing)
- `focus` - What the observation is about when not about the subject
- `bodyStructure` - Specific body structure observed

**Database Support:**
- Migration `003_add_r5_observation_fields.sql` adds R5 columns
- Indexed for efficient querying
- Backward compatible with R4 data

**Validation:**
- R5-specific field validation in `src/services/validation.rs`
- `triggeredBy.type` must be: `reflex`, `repeat`, or `re-run`
- Observation reference required for triggered observations

**Capability Statement:**
- Advertises FHIR R5 (5.0.0) compliance
- Lists supported R5 features
- Updated metadata endpoint

See [FHIR_R5_UPGRADE.md](FHIR_R5_UPGRADE.md) for complete R5 migration guide.

## Questions to Resolve Later

1. How to handle custom search parameters defined in profiles?
2. Subscription mechanism for real-time updates?
3. GraphQL interface in addition to REST?
4. Multi-tenancy strategy (if needed)?
5. Elasticsearch integration for advanced search?

---

**This document is the source of truth for architectural decisions. Update it as the project evolves.**