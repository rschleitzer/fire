# CLAUDE.md - FHIR R5 Server in Rust

## Project Overview

Building a production-grade **FHIR R5** (Fast Healthcare Interoperability Resources) server in Rust with a focus on:
- **FHIR R5 (5.0.0)** compliance - Latest normative version
- Performance and type safety
- Superior architecture compared to existing solutions (HAPI FHIR)
- Table-per-resource database design
- Separation of current and historical versions

## Core Architectural Decisions

### 1. Web Framework: Axum ✅
**Chosen over Actix-Web** for better developer ergonomics, cleaner syntax, direct Tokio integration, and easier middleware composition. Healthcare APIs are I/O-bound, so Axum's excellent DX outweighs marginal performance differences.

### 2. Database: PostgreSQL with JSON ✅
- JSON for straight FHIR resource storage
- ACID transactions (critical for bundles)
- GIN indexes for JSON queries
- Using `sqlx` with compile-time checked queries
- Custom query builder for FHIR search

### 3. Table-Per-Resource Architecture ✅

**Each FHIR resource type gets TWO tables:**
- **Current Table** - Only latest version, heavily indexed, fast searches
- **History Table** - All versions, lighter indexing, archivable

**Benefits:** Faster reads, faster searches, separate indexing strategies.

### 4. Search Parameter Extraction ✅
Resources stored as complete JSON + critical search parameters extracted to dedicated indexed columns.

### 5. Repository Pattern ✅
Each resource has: `create()`, `read()`, `read_version()`, `upsert()`, `update()`, `delete()`, `search()`, `history()`. All operations use sqlx transactions.

### 6. Pagination Strategy: Offset-Based ✅

**Current Implementation:**
- Uses standard SQL `LIMIT` + `OFFSET`
- Query parameters: `_count` (default: 50), `_offset` (default: 0)
- Returns `total` count and `next` link in Bundle

**Trade-offs:**
- ✅ Simple, stateless, FHIR-compliant
- ✅ Supports random page access
- ✅ No storage overhead (unlike HAPI's snapshot cache)
- ⚠️ Phantom reads possible (duplicates/missing items if data changes between pages)
- ⚠️ Performance degrades with high offsets (OFFSET 10000 is slow)

**Why This Works for Fire:**
- Healthcare data changes slowly compared to other domains
- Most users don't paginate beyond first few pages
- FHIR spec doesn't mandate consistency across page requests
- Azure FHIR Service and others use same approach

**Alternative Approaches (Not Implemented):**
- **Keyset pagination** - Would use `WHERE (last_updated, id) > ($1, $2)` for consistent results, no phantom reads, but forward-only
- **HAPI-style snapshots** - Stores search result IDs in database tables for perfect consistency, but adds storage/cleanup complexity
- **Firely-style cursors** - Uses native DB cursors with session management, database-specific

**Future Consideration:** Could add keyset pagination as optional mode for bulk exports via `_cursor` parameter while keeping offset for UI pagination.

## Current Status

### ✅ Completed (3 Resources)
- Project setup with Axum, sqlx, PostgreSQL
- **Patient resource** with full CRUD, search, history
  - String search (name, family, given)
  - Token search (identifier, gender)
  - Date search (birthdate with prefixes)
  - Reference search (general-practitioner)
  - Search modifiers (`:exact`, `:contains`, `:missing`)
  - Search chaining (forward and reverse)
  - `_include` and `_revinclude` support
- **Practitioner resource** with full CRUD, search, history
  - Name search with all modifiers
  - Identifier and telecom search
  - `_revinclude` for Patient references
- **Observation resource** with full CRUD, search, history
  - Subject and patient references
  - Code and value search
  - Date search with prefixes
  - `_include` for subject references
  - Search chaining to Patient
- Transaction bundles (POST /fhir)
- Capability statement (GET /fhir/metadata)
- Health check endpoints
- Error handling with proper FHIR OperationOutcome
- Middleware (logging, request_id)
- Content negotiation (JSON, XML, HTML)

### 🚧 Next Phase: Code Generation for Remaining Resources

**Current Approach:** Manual implementation of Patient, Observation, Practitioner as reference implementations.

**Code Generation Strategy:** Two-stage generation with curated intermediate model.

## Code Generation Strategy

### Philosophy: Curated Intermediate Model

Fire uses a **battle-tested two-stage approach**:

```
FHIR R5 JSONs → Curated XML Model → Generated Code
  (messy)          (clean/fixed)       (correct)
```

**Why This Works:**
1. **FHIR JSONs have inconsistencies** - Missing fields, vague search params (phonetic), overlapping ValueSets
2. **Intermediate XML is editable** - Fix issues once, version control corrections
3. **DSSSL/OpenJade for generation** - Proven at scale, XML-native, declarative templates
4. **Document corrections** - Each fix documented with `<note>` explaining deviation from spec

### Phase 1: XML Model Generation

**Tool: `fhir-to-xml` (Rust CLI in `src/bin/`)**

Reads FHIR R5 JSON definitions, outputs curated XML containing only what Fire needs:

```rust
// Extracts from FHIR JSONs
let structure_defs = load_json("fhir-r5/profiles-resources.json");
let search_params = load_json("fhir-r5/search-parameters.json");
let value_sets = load_json("fhir-r5/valuesets.json");

// Outputs provisional model, curated incrementally as we go
write_xml("model/fhir-fire.xml", extract_and_curate(structure_defs, search_params));
```

**Example XML Output:**
```xml
<model>
  <resource name="Patient" base-url="/fhir/Patient">
    <!-- Core fields -->
    <element name="id" type="id" cardinality="0..1"/>
    <element name="active" type="boolean" cardinality="0..1"/>
    <element name="name" type="HumanName" cardinality="0..*"/>

    <!-- Search parameters (indexed columns) -->
    <search-param name="family" type="string" path="Patient.name.family" index="gin"/>
    <search-param name="birthdate" type="date" path="Patient.birthDate" index="btree"/>

    <!-- References (for chaining) -->
    <reference name="general-practitioner" target="Practitioner" cardinality="0..*"/>

    <!-- Documented fixes -->
    <search-param name="general-practitioner" type="reference">
      <modifier name="missing" supported="true"/>
      <note>FHIR spec says unsupported, but clients expect it - corrected</note>
    </search-param>

    <!-- Intentional omissions -->
    <ignore search-param="phonetic" reason="Requires Soundex, low usage, defer to v2"/>
  </resource>
</model>
```

### Phase 2: Manual Curation

**File: `model/fhir-fire.xml`** (version controlled)

- Fix FHIR spec inconsistencies
- Correct misspecified search params
- Add Fire-specific optimizations
- Document every deviation with `<note>`

**Workflow:**
1. Run generator → code doesn't compile
2. Find issue in FHIR spec or generated code
3. Edit `model/fhir-fire.xml` (add `<note>` documenting fix), edit generator if applicable
4. Re-run generator
5. Repeat until all resources work
6. Commit XML model, generator, and generated code

### Phase 3: DSSSL Code Generation

**Directory Structure:**
```
fire/
├── model/
│   └── fhir-fire.xml              # Curated model (version controlled)
├── codegen/
│   ├── map.dsl                    # Main entry (includes all modules)
│   ├── config.scm                 # Generation switches
│   ├── utilities.scm              # String helpers, flow objects
│   ├── general.scm                # Cross-cutting functions
│   ├── rules.scm                  # Element processing rules
│   ├── rust-structs.scm           # Generate Rust models
│   ├── sql-tables.scm             # Generate migrations
│   ├── repositories.scm           # Generate repository methods
│   ├── handlers.scm               # Generate Axum handlers
│   └── search-extractors.scm     # Generate search param extractors
├── catalog                        # SGML catalog
├── xml.dcl                        # XML declaration
└── generate.sh                    # Generation script
```

**Key DSSSL Patterns (See Detailed Examples Below):**
- String manipulation (case conversion, escaping)
- Node navigation and selection
- Iteration over XML elements
- File generation with flow objects
- Type mapping (FHIR → Rust, FHIR → PostgreSQL)
- Conditional generation

### Phase 4: Generation Script

```bash
#!/bin/bash
set -e

# Step 1: Convert FHIR JSONs to XML (if needed)
if [ "$1" == "--rebuild-model" ]; then
    cargo run --bin fhir-to-xml -- \
        --input fhir-r5/ \
        --output model/fhir-fire.xml
fi

# Step 2: Run OpenJade
export SGML_CATALOG_FILES="$(pwd)/catalog"
export SP_CHARSET_FIXED="YES"
export SP_ENCODING="XML"
openjade -t sgml -d codegen/map.dsl model/fhir-fire.xml

# Step 3: Format generated code
cargo fmt

# Step 4: Update sqlx metadata
DATABASE_URL=postgres://postgres:postgres@localhost:5432/fhir_dev \
    cargo sqlx prepare

echo "✅ Generation complete!"
```

### Generated Code Architecture

**For Each Resource, Generate:**

1. **Rust Model** (`src/models/{resource}.rs`)
   - Struct with FHIR fields
   - Serde serialization
   - Type conversions

2. **SQL Migration** (`migrations/{NNN}_create_{resource}.sql`)
   - Current table with search columns
   - History table
   - Indexes (GIN, BTREE)
   - Foreign key constraints

3. **Repository** (`src/repository/{resource}.rs`)
   - CRUD methods using sqlx
   - Search with dynamic query building
   - History operations
   - Helper methods (find_by_reference, etc.)

4. **Handler** (`src/api/handlers/{resource}.rs`)
   - Axum route handlers
   - Content negotiation (JSON/XML/HTML)
   - Bundle building
   - Pagination links

5. **Search Extractor** (`src/models/{resource}.rs`)
   - `extract_{resource}_search_params(&content)` function
   - Extracts values from JSON for indexed columns

### Benefits of This Approach

1. **Incremental adoption** - Generate one resource at a time
2. **Debug-friendly** - Easy to see what's generated and why
3. **Documentation built-in** - XML `<note>` elements explain deviations
4. **Future-proof** - When FHIR R6 comes out, adjust parser and re-generate
5. **Custom extensions** - Easy to add Fire-specific optimizations
6. **Version controlled** - Both model and generated code in git

## DSSSL Pattern Library

### Core String Manipulation

```scheme
; Case conversion
(define (downcase-string s)
  (let ((l (string-length s))) (let loop ((str "") (pos 0))
    (if (>= pos l) str
        (loop ($ str (string (char-downcase (string-ref s pos)))) (+ 1 pos))))))

(define (first-letter-upcase s)
    ($ (upcase-string (substring s 0 1)) (substring s 1 (string-length s))))

; String operations
(define $ string-append)  ; Shorthand

; Escape Rust keywords
(define (escape-rust-keyword identifier)
    ($ (case identifier
        (("type" "mod" "trait" "impl" "fn" "let" "mut" "ref" "match") "r#")
        (else ""))
       identifier))
```

### Navigation and Selection

```scheme
; Shorthand for attribute access
(define % attribute-string)

; Common accessors
(define (name-of node) (% "name" node))
(define (type-of node) (% "type" node))

; Tree navigation
(define (select-children name node)
    (select-elements (children node) name))
```

### Iteration Patterns

```scheme
; Generic iterators
(define (for nodelist func)
    (apply $ (map func (node-list->list nodelist))))

; Iterate over selected elements
(define (for-selected-children element func)
    (for (select-children element (current-node)) func))
```

### File Generation

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

### Type Mapping

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

### Generation Examples

**Generate Rust Struct:**
```scheme
(element resource
    (file ($ "src/models/" (downcase-string (name)) ".rs")
        ($
"use chrono::{DateTime, NaiveDate, Utc};
use serde::{Deserialize, Serialize};

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

**Generate SQL Tables:**
```scheme
(define (generate-tables resource)
    (let ((table-name (downcase-string (name-of resource)))
          (search-params (select-children "searchParam" resource)))
        ($
"CREATE TABLE " table-name " (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Search parameters
"
            (for search-params (lambda (param)
                ($ "    " (name-of param) " " (postgres-type (type-of param)) ",\n")))
"
    -- Indexes
    INDEX idx_" table-name "_last_updated (last_updated)
"
            (for search-params (lambda (param)
                ($ "    INDEX idx_" table-name "_" (name-of param) " (" (name-of param) "),\n")))
");\n")))
```

## Testing with pyrtest

**Fire uses [pyrtest](https://github.com/rschleitzer/pyrtest)** - a comprehensive Python-based black-box HTTP API test suite for validating FHIR R5 server implementations.

### Why pyrtest?

- **Separation of concerns** - Tests are decoupled from implementation
- **Language-agnostic** - Tests HTTP API, works with any FHIR server
- **Comprehensive** - Covers CRUD, search, bundles, history, pagination, conditional operations
- **Black-box testing** - Validates external behavior, not internal implementation
- **Reusable** - Same test suite validates multiple FHIR server implementations

### Current Test Status

**198/198 tests passing** ✅

Including:
- CRUD Operations
- Search with all parameter types
- Search modifiers (`:exact`, `:contains`, `:missing`)
- Search prefixes (gt, lt, ge, le)
- Search chaining (forward and reverse)
- `_include` and `_revinclude`
- Result parameters (`_count`, `_offset`, `_sort`, `_total`)
- Bundles (transaction and batch)
- History operations
- Pagination
- Error handling

### Running pyrtest

```bash
# Terminal 1 - Start Fire
cd ~/repos/fire
DATABASE_URL=postgres://postgres:postgres@localhost:5432/fhir_dev cargo run

# Terminal 2 - Run tests
cd ~/repos/pyrtest
export FHIR_BASE_URL=http://localhost:3000/fhir
pytest -v

# Run specific tests
pytest tests/test_patient_search.py -v
pytest tests/test_search_chaining.py -v
pytest tests/test_search_includes.py -v
```

### Development Workflow

1. **Implement feature in Fire** (Rust)
2. **Run pyrtest** to validate FHIR R5 compliance
3. **Fix issues** until tests pass
4. **Commit** when all tests green

This ensures Fire maintains FHIR R5 compliance throughout development.

## Notes for Claude Code

### Database Operations
- Always use `sqlx::query!` or `sqlx::query_as!` for type-safe queries
- Use transactions for multi-step database operations (create/update → history)
- Extract search parameters into dedicated columns for performance
- Current table queries never filter by version_id (always latest)
- History queries work with `{resource}_history` tables
- After schema changes, run `cargo sqlx prepare` to update query metadata

### FHIR Implementation
- FHIR search is complex - embrace it, don't oversimplify
- Search parameter extraction happens in `src/models/{resource}.rs`
- Repository handles database operations (`src/repository/{resource}.rs`)
- Handlers manage HTTP/REST concerns (`src/api/handlers/{resource}.rs`)
- Content negotiation supports JSON, XML, and HTML
- Bundle building uses efficient string concatenation (not serde for entire bundle)

### Code Style
- Performance matters - this is healthcare data at scale
- Type safety is critical - leverage Rust's type system
- Error handling returns proper FHIR OperationOutcome
- Use `tracing` for logging with structured fields
- **Test with pyrtest** - Run black-box HTTP tests to validate compliance

### Pagination
- Current offset-based strategy is acceptable for healthcare use cases
- Phantom reads are documented and understood trade-off
- Could add keyset pagination later for bulk exports if needed
- HAPI-style snapshots would add complexity without clear benefit for Fire's use cases

## Project Roadmap

### Phase 1: Foundation (Current) ✅
- [x] Patient, Observation, Practitioner resources
- [x] Full CRUD with history
- [x] Search with all parameter types
- [x] Search chaining and includes
- [x] 198/198 pyrtest tests passing

### Phase 2: Code Generation (Next)
- [ ] Build `fhir-to-xml` Rust tool
- [ ] Generate initial `model/fhir-fire.xml` from FHIR R5 JSONs
- [ ] Set up DSSSL generators
- [ ] Generate 5-10 common resources (Encounter, Condition, Medication, etc.)
- [ ] Validate with pyrtest
- [ ] Scale to all 140+ resources

### Phase 3: Production Features
- [ ] Authentication/Authorization (SMART on FHIR)
- [ ] Rate limiting
- [ ] Audit logging
- [ ] Subscription mechanism
- [ ] Bulk data export ($export operation)

### Phase 4: Optimization
- [ ] Performance profiling and optimization
- [ ] Caching strategy
- [ ] Connection pooling tuning
- [ ] Query optimization for common searches

## Open Questions

1. How to handle custom search parameters defined in profiles?
2. Subscription mechanism for real-time updates?
3. GraphQL interface in addition to REST?
4. Multi-tenancy strategy (if needed)?
5. Elasticsearch integration for advanced full-text search?

---

**This document is the source of truth for architectural decisions. Update it as the project evolves.**
