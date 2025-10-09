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

**Code Generation Approach:**
This project will use template-based code generation to handle 140+ FHIR resources. See the [Code Generation Reference](#code-generation-with-dsslopenjade) section below for complete patterns and examples. For a step-by-step tutorial, see [GeneratingCode.md](GeneratingCode.md).

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

## Code Generation with DSSSL/OpenJade

This section provides a complete reference for template-based code generation using DSSSL (Document Style Semantics and Specification Language) and OpenJade. While Fire will use Rust macros/build.rs, these patterns inform our code generation strategy for the 140+ FHIR resources.

### Why DSSSL for Fire FHIR

**Advantages:**
- **Proven at scale** - Successfully generated production code for complex healthcare systems
- **XML-native** - FHIR StructureDefinitions are XML, DSSSL is designed for XML processing
- **Declarative** - Separate metamodel (DTD) from templates (DSSSL)
- **Powerful** - LISP-based, full programming language for complex transformations
- **Inline generation** - Templates embedded directly in target language syntax

**Application to Fire:**
- Parse FHIR R5 StructureDefinitions (XML)
- Generate Rust structs for 140+ resources
- Generate SQL DDL for current + history tables
- Generate repository implementations
- Generate search parameter extractors
- Generate route handlers

### Core DSSSL Concepts

**Element Rules** - Process XML elements:
```scheme
(element resource-name
    (generate-rust-struct))
```

**Flow Objects** - Generate output:
```scheme
(make entity                          ; Create file
    system-id: "patient.rs"
    (make formatting-instruction      ; File contents
        data: "pub struct Patient {}"))
```

**Node Navigation:**
```scheme
(current-node)           ; Current XML element
(parent node)            ; Parent element
(children node)          ; Child elements
(attribute-string "name" node)  ; Attribute value
```

**Iteration:**
```scheme
(map function list)      ; Apply function to each item
(apply func args)        ; Apply function to argument list
(node-list->list nl)     ; Convert DSSSL node-list to Scheme list
```

### DSSSL Pattern Library

Complete reference of production-ready patterns for code generation.

#### String Manipulation

```scheme
; Case conversion
(define (downcase-string s)
  (let ((l (string-length s))) (let loop ((str "") (pos 0))
    (if (>= pos l) str
        (loop ($ str (string (char-downcase (string-ref s pos)))) (+ 1 pos))))))

(define (upcase-string s)
  (let ((l (string-length s))) (let loop ((str "") (pos 0))
    (if (>= pos l) str
        (loop ($ str (string (char-upcase (string-ref s pos)))) (+ 1 pos))))))

(define (first-letter-downcase s)
    ($ (downcase-string (substring s 0 1)) (substring s 1 (string-length s))))

(define (first-letter-upcase s)
    ($ (upcase-string (substring s 0 1)) (substring s 1 (string-length s))))

; String operations
(define $ string-append)  ; Shorthand

(define (string-replace string target repl)
  (let loop ((str string) (pos 0))
    (if (>= pos (string-length str)) str
        (loop (repl-substring str target repl pos)
              (if (repl-substring? str target pos)
                  (+ (string-length repl) pos)
                  (+ 1 pos))))))

(define (string-remove c str)
    (apply $ (map (lambda (x) (if (char=? x c) (string) (string x)))
                  (string->list str))))

; Escape language keywords
(define (escape-rust-keyword identifier)
    ($ (case identifier
        (("type" "mod" "trait" "impl" "fn" "let" "mut" "ref" "match") "r#")
        (else ""))
       identifier))
```

#### Navigation and Selection

```scheme
; Shorthand for attribute access
(define % attribute-string)

; Common accessors
(define (name-of node) (% "name" node))
(define (name) (name-of (current-node)))
(define (type-of node) (% "type" node))

; Tree navigation
(define (select-children name node)
    (select-elements (children node) name))

(define (select-descendants name node)
    (select-elements (descendants node) name))

; Link resolution (IDREF)
(define (link node)
    (let ((linkattr (% "link" node)))
        (if linkattr (element-with-id linkattr) #f)))

(define (name-of-link node)
    (name-of (link node)))

(define (ref node)
    (element-with-id (% "ref" node)))
```

#### Iteration Patterns

```scheme
; Generic iterators
(define (for nodelist func)
    (apply $ (map func (node-list->list nodelist))))

(define (for-files nodelist func)
    (apply sosofo-append (map func (node-list->list nodelist))))

; Iterate over children
(define (for-children func)
    (for (children (current-node)) func))

(define (for-children-of element func)
    (for (children element) func))

; Iterate over selected elements
(define (for-selected-children element func)
    (for (select-children element (current-node)) func))

(define (for-selected-descendants element func)
    (for (select-elements (descendants (current-node)) element) func))
```

#### Boolean Predicates

```scheme
; XML-style (yes/no)
(define (yes? attribute node)
    (let ((attrstring (% attribute node)))
        (if attrstring (string=? "yes" attrstring) #f)))

(define (no? attribute node)
    (let ((attrstring (% attribute node)))
        (if attrstring (string=? "no" attrstring) #f)))

; JSON-style (true/false)
(define (true? attribute node)
    (let ((attrstring (% attribute node)))
        (if attrstring (string=? "true" attrstring) #f)))

(define (false? attribute node)
    (let ((attrstring (% attribute node)))
        (if attrstring (string=? "false" attrstring) #f)))
```

#### File Generation

```scheme
; Core file generation
(declare-flow-object-class formatting-instruction
    "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")

(declare-flow-object-class entity
    "UNREGISTERED::James Clark//Flow Object Class::entity")

(define (file filename contents)
    (make entity
        system-id: filename
        (make formatting-instruction data: contents)))
```

#### Type Mapping

```scheme
; Map FHIR types to Rust types
(define (rust-type fhir-type)
    (case fhir-type
        (("string" "code" "uri" "id" "markdown") "String")
        (("boolean") "bool")
        (("integer" "positiveInt" "unsignedInt") "i32")
        (("decimal") "f64")
        (("date") "chrono::NaiveDate")
        (("dateTime" "instant") "chrono::DateTime<chrono::Utc>")
        (("base64Binary") "Vec<u8>")
        (("Coding") "Coding")
        (("CodeableConcept") "CodeableConcept")
        (("Reference") "Reference")
        (("Quantity") "Quantity")
        (else fhir-type)))  ; Complex types use their name

; Map FHIR types to PostgreSQL types
(define (postgres-type fhir-type)
    (case fhir-type
        (("string" "code" "uri" "id" "markdown") "TEXT")
        (("boolean") "BOOLEAN")
        (("integer" "positiveInt" "unsignedInt") "INTEGER")
        (("decimal") "DECIMAL")
        (("date") "DATE")
        (("dateTime" "instant") "TIMESTAMPTZ")
        (("base64Binary") "BYTEA")
        (else "JSONB")))  ; Complex types stored as JSON
```

#### Conditional Generation

```scheme
; Generate code only if condition met
(define (generate-if condition code)
    (if condition code ""))

; Wrap with prefix/suffix if condition met
(define (wrap-if condition prefix content suffix)
    (if condition
        ($ prefix content suffix)
        content))

; Generate different code based on type
(define (generate-by-type type)
    (case type
        (("string") (generate-string-handler))
        (("integer") (generate-integer-handler))
        (("Reference") (generate-reference-handler))
        (else (generate-default-handler))))
```

#### Multi-Database Abstraction

```scheme
; Database identifiers
(define (db-moniker dbtype)
    (case dbtype
        ((PostgreSql) "Postgres")
        ((SqlServer) "Sql")
        ((Sqlite) "Sqlite")))

; SQL dialect differences
(define (sql-quote-left dbtype)
    (case dbtype
        ((PostgreSql Sqlite) "\"")
        ((SqlServer) "[")
        (else "")))

(define (sql-quote-right dbtype)
    (case dbtype
        ((PostgreSql Sqlite) "\"")
        ((SqlServer) "]")
        (else "")))

(define (sql-parameter-prefix dbtype)
    (case dbtype
        ((PostgreSql SqlServer) "@")
        ((Sqlite) "$")))

(define (sql-go dbtype)
    (case dbtype
        ((SqlServer) "GO")
        (else ";")))
```

#### Variant Type Handling (FHIR Choice Elements)

FHIR has "choice" elements where a property can be one of multiple types (e.g., `value[x]` in Observation).

```scheme
; Check if property has multiple type options
(define (is-variant? property)
    (> (node-list-length (select-children "variant" property)) 1))

; Generate suffix for variant property names
; Example: value[x] becomes valueString, valueInteger, etc.
(define (variant-suffix property variant is-variant?)
    (if is-variant?
        (first-letter-upcase (% "type" variant))
        ""))

; Generate all variant properties
(define (generate-variant-property property)
    (let ((variant? (is-variant? property)))
        (for-children-of (select-children "variant" property)
            (lambda (variant)
                ($ "pub " (name-of property) (variant-suffix property variant variant?)
                   ": Option<" (rust-type (% "type" variant)) ">,\n")))))
```

### FHIR-Specific Applications

#### Generate Rust Struct from FHIR Resource

**Input Model (XML):**
```xml
<resource name="Patient">
    <element name="id" type="id" cardinality="0..1"/>
    <element name="active" type="boolean" cardinality="0..1"/>
    <element name="name" type="HumanName" cardinality="0..*"/>
    <element name="gender" type="code" cardinality="0..1"/>
    <element name="birthDate" type="date" cardinality="0..1"/>
</resource>
```

**Generator:**
```scheme
(element resource
    (file ($ "src/models/" (downcase-string (name)) ".rs")
        ($
"use chrono::{DateTime, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct " (name) " {
    pub id: Option<String>,
    pub meta: Option<Meta>,
"
            (for-selected-children "element" (lambda (elem)
                (let ((elem-name (name-of elem))
                      (elem-type (rust-type (type-of elem)))
                      (is-array (string=? "0..*" (% "cardinality" elem))))
                    ($
"    pub " (escape-rust-keyword elem-name) ": "
                        (if is-array "Vec<" "Option<")
                        elem-type
                        (if is-array ">" ">")
                        ",\n"))))
"}\n")))
```

**Output:**
```rust
use chrono::{DateTime, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Patient {
    pub id: Option<String>,
    pub meta: Option<Meta>,
    pub active: Option<bool>,
    pub name: Vec<HumanName>,
    pub gender: Option<String>,
    pub birthDate: Option<NaiveDate>,
}
```

#### Generate SQL DDL for Current + History Tables

```scheme
(define (generate-tables resource)
    (let ((resource-name (downcase-string (name-of resource)))
          (search-params (select-children "searchParam" resource)))
        (file ($ "migrations/create_" resource-name "_tables.sql")
            ($
"-- Current table (latest version only)
CREATE TABLE " resource-name " (
    id UUID PRIMARY KEY,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted BOOLEAN DEFAULT FALSE,
    content JSONB NOT NULL,

    -- Search parameters
"
                (for search-params (lambda (param)
                    (let ((param-name (name-of param))
                          (param-type (type-of param)))
                        ($
"    " param-name " " (search-param-sql-type param-type) ",\n"))))
"
    -- Indexes
    INDEX idx_" resource-name "_last_updated (last_updated),
    INDEX idx_" resource-name "_deleted (deleted)
"
                (for search-params (lambda (param)
                    ($
"    INDEX idx_" resource-name "_" (name-of param) " (" (name-of param) "),\n")))
");

-- History table (all versions)
CREATE TABLE " resource-name "_history (
    id UUID NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    deleted BOOLEAN DEFAULT FALSE,
    content JSONB NOT NULL,
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Same search parameters
"
                (for search-params (lambda (param)
                    ($
"    " (name-of param) " " (search-param-sql-type (type-of param)) ",\n")))
"
    PRIMARY KEY (id, version_id)
);\n"))))

(define (search-param-sql-type param-type)
    (case param-type
        (("string") "TEXT")
        (("token") "TEXT")
        (("reference") "TEXT")
        (("date") "TIMESTAMPTZ")
        (("number") "DECIMAL")
        (else "TEXT")))
```

#### Generate Repository Implementation

```scheme
(define (generate-repository resource)
    (let ((resource-name (name-of resource))
          (table-name (downcase-string resource-name)))
        (file ($ "src/repository/" (downcase-string resource-name) ".rs")
            ($
"use sqlx::PgPool;
use uuid::Uuid;
use crate::models::" (downcase-string resource-name) "::" resource-name ";
use crate::error::FhirError;

pub struct " resource-name "Repository {
    pool: PgPool,
}

impl " resource-name "Repository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    pub async fn create(&self, resource: " resource-name ") -> Result<" resource-name ", FhirError> {
        let mut tx = self.pool.begin().await?;

        // Insert into current table
        sqlx::query!(
            r#\"
            INSERT INTO " table-name " (id, version_id, content)
            VALUES ($1, 1, $2)
            \"#,
            resource.id,
            serde_json::to_value(&resource)?
        )
        .execute(&mut tx)
        .await?;

        // Insert into history table
        sqlx::query!(
            r#\"
            INSERT INTO " table-name "_history
            (id, version_id, content, history_operation)
            VALUES ($1, 1, $2, 'CREATE')
            \"#,
            resource.id,
            serde_json::to_value(&resource)?
        )
        .execute(&mut tx)
        .await?;

        tx.commit().await?;
        Ok(resource)
    }

    pub async fn read(&self, id: &Uuid) -> Result<Option<" resource-name ">, FhirError> {
        let row = sqlx::query!(
            r#\"SELECT content FROM " table-name " WHERE id = $1 AND deleted = false\"#,
            id
        )
        .fetch_optional(&self.pool)
        .await?;

        match row {
            Some(r) => Ok(Some(serde_json::from_value(r.content)?)),
            None => Ok(None)
        }
    }

    pub async fn update(&self, id: &Uuid, resource: " resource-name ") -> Result<" resource-name ", FhirError> {
        let mut tx = self.pool.begin().await?;

        // Get current version
        let current_version = sqlx::query!(
            r#\"SELECT version_id FROM " table-name " WHERE id = $1 FOR UPDATE\"#,
            id
        )
        .fetch_one(&mut tx)
        .await?
        .version_id;

        let new_version = current_version + 1;

        // Update current table
        sqlx::query!(
            r#\"
            UPDATE " table-name "
            SET version_id = $1, content = $2, last_updated = NOW()
            WHERE id = $3
            \"#,
            new_version,
            serde_json::to_value(&resource)?,
            id
        )
        .execute(&mut tx)
        .await?;

        // Insert into history
        sqlx::query!(
            r#\"
            INSERT INTO " table-name "_history
            (id, version_id, content, history_operation)
            VALUES ($1, $2, $3, 'UPDATE')
            \"#,
            id,
            new_version,
            serde_json::to_value(&resource)?
        )
        .execute(&mut tx)
        .await?;

        tx.commit().await?;
        Ok(resource)
    }

    pub async fn delete(&self, id: &Uuid) -> Result<(), FhirError> {
        let mut tx = self.pool.begin().await?;

        sqlx::query!(
            r#\"UPDATE " table-name " SET deleted = true WHERE id = $1\"#,
            id
        )
        .execute(&mut tx)
        .await?;

        sqlx::query!(
            r#\"
            INSERT INTO " table-name "_history
            (id, version_id, content, history_operation)
            SELECT id, version_id, content, 'DELETE'
            FROM " table-name "
            WHERE id = $1
            \"#,
            id
        )
        .execute(&mut tx)
        .await?;

        tx.commit().await?;
        Ok(())
    }
}
"))))
```

### Complete Generation Example

**Metamodel (DTD):**
```xml
<!ELEMENT model (resource+)>
<!ELEMENT resource (element+, searchParam*)>
<!ATTLIST resource
    name CDATA #REQUIRED>

<!ELEMENT element EMPTY>
<!ATTLIST element
    name CDATA #REQUIRED
    type CDATA #REQUIRED
    cardinality CDATA "0..1">

<!ELEMENT searchParam EMPTY>
<!ATTLIST searchParam
    name CDATA #REQUIRED
    type (string|token|reference|date|number) #REQUIRED>
```

**Model Instance:**
```xml
<model>
    <resource name="Patient">
        <element name="id" type="id"/>
        <element name="active" type="boolean"/>
        <element name="name" type="HumanName" cardinality="0..*"/>
        <element name="birthDate" type="date"/>

        <searchParam name="name" type="string"/>
        <searchParam name="birthdate" type="date"/>
        <searchParam name="active" type="token"/>
    </resource>
</model>
```

**Generator Rules:**
```scheme
; Main entry point
(element model
    (make sequence
        (process-children)))

; Generate all files for each resource
(element resource
    (make sequence
        (generate-rust-struct)
        (generate-tables)
        (generate-repository)
        (generate-handlers)))

(define (generate-rust-struct)
    ; See "Generate Rust Struct" example above
    )

(define (generate-tables)
    ; See "Generate SQL DDL" example above
    )

(define (generate-repository)
    ; See "Generate Repository" example above
    )
```

### Best Practices for Fire

1. **Separate Metamodel from Generator**
   - DTD defines FHIR resource structure
   - DSSSL contains generation logic
   - Model instances are FHIR StructureDefinitions (converted to XML if needed)

2. **Generate for Development, Check In Results**
   - Run generator during development
   - Check generated code into git
   - Allows code review of generated code
   - Enables manual tweaks when needed

3. **Use Inline Templates**
   - Embed Rust code directly in DSSSL strings
   - Easier to maintain and understand
   - See generated code structure immediately

4. **Modular Generator Organization**
   ```
   codegen/
   ├── map.dsl           # Main entry, includes all modules
   ├── utilities.scm     # Helper functions, string manipulation
   ├── rust.scm          # Rust struct generation
   ├── sql.scm           # SQL DDL generation
   ├── repository.scm    # Repository implementation
   └── handlers.scm      # Axum handler generation
   ```

5. **Test Generation**
   - Generate for a few resources manually
   - Verify output matches expectations
   - Then generate all 140+ resources

6. **Handle FHIR Complexity**
   - Choice types (value[x]) → multiple fields
   - Cardinality (0..1 vs 0..*) → Option vs Vec
   - References → special handling for Resource/id format
   - Search parameters → extract to columns + build indexes

### Production-Proven File Structure

Based on successful production healthcare code generators (SchwebNet, Telemed5000), use this proven modular structure:

**Directory Layout:**
```
fire/
├── model/
│   └── fhir-resources.xml       # Model instance (FHIR definitions)
├── codegen/
│   ├── map.dsl                  # Main entry point (includes all modules)
│   ├── config.scm               # Configuration (database types, paths, switches)
│   ├── utilities.scm            # Flow object declarations, string helpers
│   ├── general.scm              # Cross-cutting functions (namespaces, paths)
│   ├── rules.scm                # Element processing rules
│   ├── rust.scm                 # Rust struct generation
│   ├── sql.scm                  # SQL DDL generation (multi-DB)
│   ├── repository.scm           # Repository pattern implementation
│   ├── handlers.scm             # Axum handler generation
│   ├── context.scm              # Database context generation
│   └── validation.scm           # Validation code generation
├── catalog                      # SGML catalog for OpenJade
├── xml.dcl                      # XML declaration for OpenJade
└── generate.sh                  # Generation script
```

**Main Entry File (codegen/map.dsl):**
```xml
<?xml version="1.0"?>
<!doctype style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!ENTITY config         SYSTEM "config.scm">
<!ENTITY utilities      SYSTEM "utilities.scm">
<!ENTITY general        SYSTEM "general.scm">
<!ENTITY rules          SYSTEM "rules.scm">
<!ENTITY sql            SYSTEM "sql.scm">
<!ENTITY rust           SYSTEM "rust.scm">
<!ENTITY repository     SYSTEM "repository.scm">
<!ENTITY handlers       SYSTEM "handlers.scm">
<!ENTITY context        SYSTEM "context.scm">
<!ENTITY validation     SYSTEM "validation.scm">

]>

&config;
&utilities;
&general;
&rules;
&sql;
&rust;
&repository;
&handlers;
&context;
&validation;
```

**Module Responsibilities:**

1. **config.scm** - Generation switches and configuration
   ```scheme
   ; Database types to generate for
   (define database-types (list 'PostgreSql))

   ; Generation mode switch
   (define generated-suffix
       ; ".generated"  ; Development mode - generate to separate directory
       ""             ; Production mode - overwrite existing files
   )

   ; Path prefixes
   (define src-path "src/")
   (define migration-path "migrations/")
   ```

2. **utilities.scm** - Foundation (always load first)
   ```scheme
   ; Flow object declarations
   (declare-flow-object-class formatting-instruction ...)
   (declare-flow-object-class entity ...)

   ; Core helpers
   (define $ string-append)
   (define % attribute-string)
   (define (file filename contents) ...)
   (define (downcase-string s) ...)
   (define (for nodelist func) ...)
   ```

3. **general.scm** - Cross-cutting functions
   ```scheme
   ; Project-wide helpers
   (define (fhir-namespace) "fire::fhir")
   (define (resource-table-name resource) ...)
   (define (generation-timestamp) ...)
   ```

4. **rules.scm** - Element processing rules
   ```scheme
   ; Main model processing
   (element model
       (for-files (children (current-node)) generate-resource))

   ; Resource processing
   (element resource
       (make sequence
           (rust-struct-file)
           (sql-tables-file)
           (repository-file)
           (handlers-file)))
   ```

5. **sql.scm** - Database schema generation
   ```scheme
   ; Generate for all database types
   (define (sql-create-file resource dbtype) ...)
   (define (sql-indexes resource dbtype) ...)
   ```

6. **rust.scm, repository.scm, handlers.scm, etc.** - Specialized code generation

### Running the Generator

#### Environment Setup

**Required Files:**

1. **catalog** - SGML catalog in project root:
   ```
   SGMLDECL xml.dcl
   ```

2. **xml.dcl** - Copy from OpenJade installation:
   ```bash
   # Location varies by platform:
   # Linux: /usr/share/openjade-*/pubtext/xml.dcl
   # macOS (MacPorts): /opt/local/share/sgml/openjade/xml.dcl
   # Windows: C:\Program Files\openjade-*\pubtext\xml.dcl
   cp /path/to/openjade/pubtext/xml.dcl .
   ```

**Environment Variables:**

Create a setup script for your platform:

**Linux/macOS (generate.sh):**
```bash
#!/bin/bash
set -e

# OpenJade paths (adjust for your installation)
export JADEPATH="/usr/local/share/openjade"

# Required environment variables
export SGML_CATALOG_FILES="$(pwd)/catalog"
export SP_CHARSET_FIXED="YES"
export SP_ENCODING="XML"

# Add OpenJade to PATH if needed
# export PATH="$PATH:/usr/local/bin"

# Parse FHIR StructureDefinitions to XML model
echo "Parsing FHIR StructureDefinitions..."
python3 scripts/parse_fhir_definitions.py \
    --input fhir-spec/resources \
    --output model/fhir-resources.xml

# Run OpenJade generator
echo "Generating code with OpenJade..."
openjade -t sgml -d codegen/map.dsl model/fhir-resources.xml

echo "Generation complete. Files created in src/, migrations/, etc."
echo "Review changes before committing."
```

**Windows (generate.bat):**
```batch
@echo off
setlocal

REM OpenJade paths (adjust for your installation)
set JADEPATH=C:\Program Files\openjade-1.3.1
set PATH=%PATH%;%JADEPATH%\bin

REM Required environment variables
set SGML_CATALOG_FILES=%CD%\catalog
set SP_CHARSET_FIXED=YES
set SP_ENCODING=XML

REM Parse FHIR StructureDefinitions to XML model
echo Parsing FHIR StructureDefinitions...
python scripts\parse_fhir_definitions.py ^
    --input fhir-spec\resources ^
    --output model\fhir-resources.xml

REM Run OpenJade generator
echo Generating code with OpenJade...
openjade -t sgml -d codegen\map.dsl model\fhir-resources.xml

echo Generation complete. Files created in src\, migrations\, etc.
echo Review changes before committing.
```

**Environment Variable Reference:**

- **SGML_CATALOG_FILES** - Path to catalog file (required)
- **SP_CHARSET_FIXED** - Set to "YES" to fix character encoding issues
- **SP_ENCODING** - Set to "XML" for XML input processing
- **JADEPATH** - Path to OpenJade installation (Windows primarily)

#### Installation

**Linux:**
```bash
# Ubuntu/Debian
sudo apt-get install openjade

# Fedora/RHEL
sudo dnf install openjade

# Arch
sudo pacman -S openjade
```

**macOS:**
```bash
# MacPorts
sudo port install openjade

# Homebrew (may need manual compilation)
brew install openjade
```

**Windows:**
- Download precompiled binaries from OpenJade community sources
- Or compile from source: https://sourceforge.net/projects/openjade/

#### Usage Workflow

```bash
# 1. Make the script executable (Unix)
chmod +x generate.sh

# 2. Run generation
./generate.sh

# 3. Review generated files
git diff src/ migrations/

# 4. Run tests to verify
cargo test

# 5. Format generated code
cargo fmt

# 6. Commit changes
git add src/ migrations/
git commit -m "Generate code for FHIR resources"
```

#### Troubleshooting

**Issue: "openjade: command not found"**
- Solution: Install OpenJade or add to PATH

**Issue: "cannot open catalog file"**
- Solution: Ensure SGML_CATALOG_FILES points to correct location
- Check: `echo $SGML_CATALOG_FILES` (Unix) or `echo %SGML_CATALOG_FILES%` (Windows)

**Issue: "cannot find xml.dcl"**
- Solution: Copy xml.dcl from OpenJade installation to project root

**Issue: Character encoding errors**
- Solution: Set `SP_CHARSET_FIXED=YES` and `SP_ENCODING=XML`

**Issue: "undefined function" in DSSSL**
- Solution: Check module load order in map.dsl (utilities must load first)

**Issue: Generated files have wrong line endings**
- Solution: Configure git to handle line endings: `git config core.autocrlf input`

### Transition Strategy

**Phase 1:** Use DSSSL for initial generation
- Generates Rust structs, SQL DDL, repositories
- Gets 140 resources up quickly
- Validates architecture

**Phase 2:** Migrate to Rust macros/build.rs
- Port DSSSL patterns to Rust procedural macros
- Use proc-macro crate for attribute macros
- Maintain same structure, just Rust-native

**Phase 3:** Runtime optimization
- Keep generated code
- Add handwritten optimizations
- Profile and improve hot paths

---

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

Fire FHIR Server implements **FHIR R5 (5.0.0)**, the latest normative release.

### Observation Resource Fields

**Standard Observation Fields:**
- `triggeredBy` - Identifies observations that triggered this observation (reflex testing)
- `focus` - What the observation is about when not about the subject
- `bodyStructure` - Specific body structure observed

**Database Support:**
- All fields indexed for efficient querying
- Full JSONB storage for complete resource representation

**Validation:**
- Field validation in `src/services/validation.rs`
- `triggeredBy.type` must be: `reflex`, `repeat`, or `re-run`
- Observation reference required for triggered observations

**Capability Statement:**
- Advertises FHIR R5 (5.0.0) compliance
- Lists all supported features
- Available at `/fhir/metadata` endpoint

## Questions to Resolve Later

1. How to handle custom search parameters defined in profiles?
2. Subscription mechanism for real-time updates?
3. GraphQL interface in addition to REST?
4. Multi-tenancy strategy (if needed)?
5. Elasticsearch integration for advanced search?

---

**This document is the source of truth for architectural decisions. Update it as the project evolves.**