# CLAUDE.md - FHIR R5 Server in Rust

## Project Overview

Production-grade **FHIR R5 (5.0.0)** server in Rust focusing on:
- FHIR R5 compliance
- Performance and type safety
- Superior architecture (vs HAPI FHIR)
- Table-per-resource database design
- Separation of current and historical versions

## Core Architectural Decisions

### 1. Web Framework: Axum ✅
Chosen over Actix-Web for better developer ergonomics, cleaner syntax, direct Tokio integration. Healthcare APIs are I/O-bound, so Axum's DX outweighs marginal performance differences.

### 2. Database: PostgreSQL with JSON ✅
- JSON for FHIR resource storage
- ACID transactions (critical for bundles)
- GIN indexes for JSON queries
- `sqlx` with compile-time checked queries
- Custom query builder for FHIR search

### 3. Table-Per-Resource Architecture ✅
Each FHIR resource type gets **two tables**:
- **Current Table** - Latest version only, heavily indexed, fast searches
- **History Table** - All versions, lighter indexing, archivable

### 4. Search Parameter Extraction ✅
Resources stored as complete JSON + critical search parameters extracted to dedicated indexed columns.

### 5. Repository Pattern ✅
Each resource has: `create()`, `read()`, `read_version()`, `upsert()`, `update()`, `delete()`, `search()`, `history()`.

### 6. Pagination Strategy: Offset-Based ✅
Uses standard SQL `LIMIT` + `OFFSET`. Trade-offs:
- ✅ Simple, stateless, FHIR-compliant, no storage overhead
- ⚠️ Phantom reads possible, performance degrades with high offsets

Works well for healthcare (data changes slowly, most users don't paginate deeply). Alternative approaches (keyset, HAPI snapshots, Firely cursors) documented but not implemented.

## Current Status

### ✅ Completed (3 Resources)
- **Patient, Practitioner, Observation** with full CRUD, search, history
- String, token, date, reference search with modifiers (`:exact`, `:contains`, `:missing`)
- Search chaining (forward and reverse), `_include` and `_revinclude`
- Transaction bundles, capability statement, health checks
- Error handling with FHIR OperationOutcome
- Content negotiation (JSON, XML, HTML)
- **198/198 pyrtest tests passing** ✅

### 🚧 Next Phase: Code Generation

## Code Generation Strategy

### Philosophy: DTD-First with Curated Intermediate Model

Three-stage approach proven in production FHIR servers:

```
FHIR R5 JSONs → Curated XML Model → Generated Code
  (messy)       (clean/validated)     (correct)
                validates against DTD
```

**Benefits:**
1. DTD schema proven at scale in commercial FHIR server
2. Intermediate XML fixes FHIR spec inconsistencies
3. Version-controlled corrections with `<note>` documentation
4. DSSSL/OpenJade for generation - proven, XML-native

### Phase 0: DTD Schema (Completed ✅)

**File: `fhirspec.dtd`** - Defines XML model structure:
- `<resource>` with `active` attribute controls generation
- `<property>` handles choice types (`value[x]`) via `<variants>`
- `<search>` with types: token, reference, quantity, string, composite
- `<components>` for composite searches
- `<paths>`/`<parts>` for navigation through resource structure

### Phase 1: XML Model Generation

**Tool: `fhir-to-xml`** (Rust CLI in `src/bin/`) - Extracts from FHIR R5 JSONs, outputs curated XML.

### Phase 2: Manual Curation

**File: `fhir.xml`** (version controlled) - Fix spec inconsistencies, document deviations with `<note>`.

**Workflow:**
1. Run generator → fix compile errors
2. Edit `fhir.xml` (document fixes), edit generator if needed
3. Re-run generator
4. Commit XML model and generated code

**Note:** `onsgmls -s xml.dcl fhir.xml` validation is only for quick validation during `fhir-to-xml` development, not part of regular codegen workflow.

**Fixing Code Generators:**
When fixing a code generator:
1. The baseline file (e.g., manually crafted `migrations/001_initial_schema.sql`) is what we want to generate
2. Run `./fire.sh` - this overwrites the baseline file with generated code
3. Compare generated output against the baseline (what we want)
4. Adjust the generator file (`codegen/*.scm`) to match the baseline
5. Re-run `./fire.sh` and check if diffs are gone
6. Repeat until generated output matches the baseline exactly
7. Sometimes necessary to change the baseline file itself (reordering, bug fixes, etc.) during this process

**Current Status:**
- ✅ 0 DTD validation errors
- 3 active resources: Observation, Patient, Practitioner

### Phase 3: DSSSL Code Generation

**Directory Structure:**
```
fire/
├── fhir.xml, fhirspec.dtd, xml.dcl, catalog
├── codegen/
│   ├── map.dsl          # Main entry
│   ├── config.scm       # Generation switches
│   ├── utilities.scm    # String helpers, flow objects
│   ├── general.scm      # Cross-cutting functions
│   ├── rules.scm        # Element processing rules
│   ├── migration.scm    # ✅ SQL migrations
│   └── structs.scm      # ✅ Rust models
└── fire.sh              # Generation script
```

**Adding a New Generator:**
1. Declare entity in `map.dsl`: `<!ENTITY migration SYSTEM "migration.scm">`
2. Include entity: `&migration;`
3. Add rule in `rules.scm`: `(element resources (sosofo-append (migration) ...))`
4. Create generator file

**Order matters:** Declare → Include → Use

### Phase 4: Generation Script

**File: `fire.sh`**
```bash
#!/bin/bash
export SP_ENCODING=utf-8
openjade -t sgml -d codegen/map.dsl fhir.xml
```

**Usage:**
```bash
cargo run --bin fhir-to-xml              # Regenerate fhir.xml (if needed)
./fire.sh                                # Generate code
DATABASE_URL=... cargo sqlx prepare      # Update sqlx metadata
```

**Note:** Do NOT run `cargo fmt` on generated code - it makes comparison against codegen output difficult.

### Generated Code Architecture

1. **Single SQL Migration** (`migrations/001_initial_schema.sql`) - All active resources, current + history tables
2. **Rust Model** (per resource: `src/models/{resource}.rs`) - Structs, serde, conversions
3. **Repository** (per resource: `src/repository/{resource}.rs`) - CRUD, search, history
4. **Handler** (per resource: `src/api/handlers/{resource}.rs`) - Axum routes, content negotiation
5. **Search Extractor** (per resource: `src/models/{resource}.rs`) - Extract values from JSON

### Benefits
- Incremental adoption (one resource at a time)
- Debug-friendly (see what's generated and why)
- Documentation built-in (`<note>` elements)
- Future-proof (FHIR R6 = adjust parser, re-generate)
- Version controlled (model + generated code)

## DSSSL Pattern Library

### Learning Approach: Start with Dumb Template

1. Copy exact desired output file
2. Paste into string literal `($"...")`
3. Escape: `\` → `\\`, `"` → `\"`, `<` → `<""`
4. Verify generation produces identical output
5. Make dynamic incrementally

### Core Patterns

**Inverted String Pattern:**
```scheme
; Pattern: "text"variable"more-text" (no spaces around variables)
($"CREATE TABLE "table-name" (")
```

**String Manipulation:**
```scheme
(define $ string-append)
(define % attribute-string)
(define (name-of node) (% "name" node))
(define (downcase-string s) ...)  ; See codegen/utilities.scm
(define (escape-rust-keyword id) ...)
```

**Navigation:**
```scheme
(define (select-children name node)
    (select-elements (children node) name))
```

**Iteration:**
```scheme
(define (for nodelist func)
    (apply $ (map func (node-list->list nodelist))))
```

**File Generation:**
```scheme
(declare-flow-object-class formatting-instruction ...)
(declare-flow-object-class entity ...)
(define (file filename contents)
    (make entity system-id: filename
        (make formatting-instruction data: contents)))
```

**SGML Escaping in DSSSL:**
```scheme
; Escape < and & with "" (double quotes)
($ "Option<""String>")        ; Option<String>
($ "fn get(&""self)")         ; fn get(&self)
```

**Type Mapping:**
```scheme
(define (rust-type fhir-type)
    (case fhir-type
        (("string" "code" "uri") "String")
        (("boolean") "bool")
        (("integer") "i32")
        (("date") "chrono::NaiveDate")
        (("dateTime" "instant") "chrono::DateTime<chrono::Utc>")
        (else fhir-type)))

(define (postgres-type fhir-type)
    (case fhir-type
        (("string" "code" "uri") "TEXT")
        (("boolean") "BOOLEAN")
        (("date") "DATE")
        (("dateTime" "instant") "TIMESTAMPTZ")
        (else "JSONB")))
```

### Key Patterns from Fire

**Active Resource Filtering:**
```scheme
(define (active? resource)
    (or (true? "active" resource)
        (and complete-fhir? (not (string=? "Resource" (name-of resource))))))

(define (for-active-resources func)
    (for (node-list-filter active? (children (current-node))) func))
```

**Property Path Navigation:**
```scheme
(define (search-property search)
    ; Navigate: search → paths → path → parts → part → ref → property
    (let* ((paths-node (select-children "paths" search))
           (path-nodes ...)
           (part-ref ...))
      (if part-ref (element-with-id part-ref) #f)))

(define (search-is-collection? search)
    (let ((property (search-property search)))
      (if property (true? "iscollection" property) #f)))
```

**Boolean Checking:**
```scheme
(define (true? attribute node)
    (let ((attrstring (% attribute node)))
      (if attrstring (string=? "true" attrstring) #f)))
```

**Trailing Commas:**
```scheme
(define (has-output-following-siblings? search)
    (let loop ((sibling (ifollow search)))
      (cond ((node-list-empty? sibling) #f)
            ((string=? "composite" (% "type" sibling)) (loop (ifollow sibling)))
            (else #t))))

(define (trailing-comma search)
    (if (has-output-following-siblings? search) "," ""))
```

**Conditional Generation:**
```scheme
(case (% "type" search)
  (("token")
    (if (search-is-simple-code? search)
        ($"    "col" TEXT"(if is-collection "[]" "")",")
        (let ((suffix (if (search-is-identifier? search) "_value" "_code")))
          ($"    "col"_system TEXT"(if is-collection "[]" "")",
    "col suffix" TEXT"(if is-collection "[]" "")",")))))
```

**Variant Handling:**
```scheme
(let* ((property (search-property search))
       (has-variants (property-has-variants? property))
       (variants (if has-variants (property-variants property) (empty-node-list)))
       (variant-types (map (lambda (v) (% "type" v)) (node-list->list variants))))
  (if has-variants
      ($ (if (member "dateTime" variant-types) ...) ...)
      ($"    "col" DATE,")))
```

**Pattern Application Order:**
1. Dumb template with escaping
2. Make resource names dynamic (`for-active-resources`)
3. Make columns/fields dynamic (iterate search params)
4. Handle variants and collections
5. Add conditional logic
6. Implement trailing commas
7. Compose helper functions

**Helper Naming Conventions:**
- `search-X?` - Predicates for search parameters
- `property-X?` - Predicates for properties
- `X-of` - Extract children (e.g., `reference-searches-of`)
- `for-X` - Iteration helpers
- `has-X?` - Existence checks

**See `codegen/` directory for complete implementations.**

## Testing with pyrtest

**[pyrtest](https://github.com/rschleitzer/pyrtest)** - Python-based black-box HTTP API test suite.

**Current Status: 198/198 tests passing** ✅

Covers: CRUD, all search types, modifiers, prefixes, chaining, includes, bundles, history, pagination, error handling.

**Running:**
```bash
# Terminal 1 - Start Fire
DATABASE_URL=postgres://postgres:postgres@localhost:5432/fhir_dev cargo run

# Terminal 2 - Run tests
cd ~/repos/pyrtest
export FHIR_BASE_URL=http://localhost:3000/fhir
pytest -v
```

**Workflow:** Implement → Run pyrtest → Fix → Commit when green

## Notes for Claude Code

### Database Operations
- Use `sqlx::query!` or `sqlx::query_as!` for type-safe queries
- Use transactions for multi-step operations
- Current table queries never filter by version_id (always latest)
- After schema changes: `cargo sqlx prepare`

### FHIR Implementation
- Search parameter extraction: `src/models/{resource}.rs`
- Repository: `src/repository/{resource}.rs`
- Handlers: `src/api/handlers/{resource}.rs`
- Bundle building uses efficient string concatenation

### Code Style
- Performance matters - healthcare data at scale
- Leverage Rust's type system
- Return proper FHIR OperationOutcome for errors
- Use `tracing` for logging
- **Test with pyrtest**

## Project Roadmap

### Phase 1: Foundation ✅
- [x] Patient, Observation, Practitioner
- [x] Full CRUD, search, history, chaining
- [x] 198/198 pyrtest tests passing

### Phase 2: Code Generation ✅ (Current)
- [x] Build `fhir-to-xml` tool
- [x] Generate `fhir.xml` from FHIR R5 JSONs
- [x] Set up DSSSL generators (migration.scm, structs.scm)
- [x] Validate XML model (0 DTD errors)
- [x] Generate 3 resources
- [x] Validate with pyrtest
- [x] Document DSSSL patterns
- [ ] Build extractors/repositories/handlers generators
- [ ] Scale to more resources (after generators complete)

**Strategy: Depth-First**
- Start with 3 representative resources (Patient, Observation, Practitioner)
- Generate all layers for these 3 before scaling
- Keeps compilation manageable, easier debugging
- Observation covers nearly all FHIR search patterns
- Scale horizontally after patterns proven

### Phase 3: Production Features
- [ ] Authentication/Authorization (SMART on FHIR)
- [ ] Rate limiting, audit logging
- [ ] Subscription mechanism
- [ ] Bulk data export

### Phase 4: Optimization
- [ ] Performance profiling
- [ ] Caching strategy
- [ ] Connection pooling tuning
- [ ] Query optimization

## Open Questions

1. Custom search parameters in profiles?
2. Subscription mechanism for real-time updates?
3. GraphQL interface?
4. Multi-tenancy strategy?
5. Elasticsearch integration for full-text search?

---

**This document is the source of truth for architectural decisions. Update as project evolves.**
