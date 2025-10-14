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

### Philosophy: DTD-First with Curated Intermediate Model

Fire uses a **battle-tested three-stage approach** proven in production commercial FHIR servers:

```
                   ┌─────────────────┐
                   │  fhirspec.dtd   │  ← Proven schema
                   │  (Schema/Rules) │
                   └────────┬────────┘
                            │
FHIR R5 JSONs ──────► Curated XML Model ──────► Generated Code
  (messy)              (clean/fixed)              (correct)
                    validates against DTD
```

**Why This Works:**
1. **DTD from production system** - Schema proven at scale in commercial FHIR server
2. **FHIR JSONs have inconsistencies** - Missing fields, vague search params (phonetic), overlapping ValueSets
3. **Intermediate XML is editable** - Fix issues once, version control corrections, validates against DTD
4. **DSSSL/OpenJade for generation** - Proven at scale, XML-native, declarative templates
5. **Document corrections** - Each fix documented in XML, deviation from spec explained

### Phase 0: DTD Schema (Completed ✅)

**File: `fhirspec.dtd` (in repo root)**

Defines the structure of the intermediate XML model. Key elements:

- **`<fhir>`** - Root element containing all resources, elements, and codesets
- **`<resource>`** - Each FHIR resource with properties, elements, searches, operations
  - `active` attribute controls which resources to generate
- **`<element>`** - Recursive structure for complex types and backbone elements
- **`<property>`** - Individual fields with type variants (handles choice types like `value[x]`)
- **`<search>`** - Search parameters with types: token, reference, quantity, string, composite, etc.
  - **`<components>`** - For composite search parameters (code-value-quantity, etc.)
  - **`<paths>`** with **`<parts>`** - Navigation through resource structure
- **`<codeset>`** - ValueSets for constrained fields

**Key Features:**
- Handles composite search parameters (just implemented in Fire!)
- Supports reference targets for chaining
- Manages choice types (`value[x]`) via `<variants>`
- Includes upgrade tracking for version migrations
- Optional operations and profiles (can defer for Fire)

See `model/fhirspec.dtd` for complete schema definition.

### Phase 1: XML Model Generation

**Tool: `fhir-to-xml` (Rust CLI in `src/bin/`)**

Reads FHIR R5 JSON definitions, outputs curated XML containing only what Fire needs:

```rust
// Extracts from FHIR JSONs
let structure_defs = load_json("fhir-r5/profiles-resources.json");
let search_params = load_json("fhir-r5/search-parameters.json");
let value_sets = load_json("fhir-r5/valuesets.json");

// Outputs provisional model, curated incrementally as we go
write_xml("fhir.xml", extract_and_curate(structure_defs, search_params));
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

**File: `fhir.xml`** (version controlled, in repo root)

- Fix FHIR spec inconsistencies
- Correct misspecified search params
- Add Fire-specific optimizations
- Document every deviation with `<note>`

**Workflow:**
1. Run generator → code doesn't compile
2. Find issue in FHIR spec or generated code
3. Edit `fhir.xml` (add `<note>` documenting fix), edit generator if applicable
4. Validate XML with `onsgmls -s fhir.xml`
5. Re-run generator
6. Repeat until all resources work
7. Commit XML model, generator, and generated code

**XML Validation (without OpenJade):**

Use `onsgmls` (SGML parser from OpenSP) to validate XML against DTD:

```bash
# Validate XML model - must pass with zero errors before code generation
onsgmls -s fhir.xml

# Count errors
onsgmls -s fhir.xml 2>&1 | grep "^onsgmls:" | wc -l

# Expected output on success: (silent, no errors)
# Common errors:
#   - IDREF validation: "reference to non-existent ID 'foo'"
#   - DTD violations: Element/attribute structure mismatches
```

**Current Validation Status:**
- ✅ **0 DTD validation errors** - Fully compliant!
- **3 active resources** - Observation, Patient, Practitioner
- **Property optimization** - Single variants flattened, multi-variants marked with `type="variant"`

**Key validation rules enforced by DTD:**
- **IDREF integrity**: All `ref` attributes must point to valid `id` values
- **Property references**: Must use binding names (e.g., `ref="triggeredbytype"`)
- **Codeset IDs**: Generated from CodeSystem.name, not CodeSystem.id
- **Backbone elements**: IDs are path with dots removed (e.g., `Observation.component` → `observationcomponent`)
- **Component refs**: Point to property IDs within backbone elements (e.g., `observationcomponent.code`)

### Phase 3: DSSSL Code Generation

**Directory Structure:**
```
fire/
├── fhir.xml                       # Curated model (version controlled, in root)
├── fhirspec.dtd                   # DTD schema
├── xml.dcl                        # XML declaration for SGML parser
├── catalog                        # SGML catalog file
├── codegen/
│   ├── map.dsl                    # Main entry (includes all modules)
│   ├── config.scm                 # Generation switches
│   ├── utilities.scm              # String helpers, flow objects
│   ├── general.scm                # Cross-cutting functions
│   ├── rules.scm                  # Element processing rules
│   ├── migration.scm              # Generate SQL migrations
│   ├── rust-structs.scm           # Generate Rust models
│   ├── repositories.scm           # Generate repository methods
│   ├── handlers.scm               # Generate Axum handlers
│   └── search-extractors.scm     # Generate search param extractors
└── generate.sh                    # Generation script
```

**Key DSSSL Patterns (See Detailed Examples Below):**
- String manipulation (case conversion, escaping)
- Node navigation and selection
- Iteration over XML elements
- File generation with flow objects
- Type mapping (FHIR → Rust, FHIR → PostgreSQL)
- Conditional generation

**Adding a New Generator Module:**

When adding a new code generator (e.g., `migration.scm`):

1. **Declare entity in `map.dsl`:**
```xml
<!ENTITY migration      SYSTEM "migration.scm">
```

2. **Include entity in `map.dsl`:**
```xml
&migration;
```

3. **Add element rule in `rules.scm`:**
```scheme
(element resources
  (sosofo-append
    (migration)           ; Call your generator function
    (process-children)))  ; Continue processing children
```

4. **Create generator file** (e.g., `codegen/migration.scm`)

**Order matters:** Entities must be declared before being included, and included before the rules that use them.

### Phase 4: Generation Script

**File: `fire.sh` (in repo root)**

The existing generation script runs OpenJade to generate code from `fhir.xml`:

```bash
#!/bin/bash
export SP_ENCODING=utf-8
openjade -t sgml -d codegen/map.dsl fhir.xml
```

**Usage:**
```bash
# Regenerate fhir.xml from FHIR R5 JSONs (when spec changes)
cargo run --bin fhir-to-xml

# Validate XML against DTD (should have 0 errors)
onsgmls -s xml.dcl fhir.xml

# Generate code from fhir.xml
./fire.sh

# Format generated Rust code
cargo fmt

# Update sqlx metadata (if database schema changed)
DATABASE_URL=postgres://postgres:postgres@localhost:5432/fhir_dev cargo sqlx prepare
```

**Note:** The script is intentionally minimal - it only runs OpenJade. Other steps (formatting, sqlx prepare) are run manually as needed, allowing for incremental development and easier debugging of generated code.

### Generated Code Architecture

**Generated Code:**

1. **Single SQL Migration** (`migrations/001_initial_schema.sql`)
   - Contains all active resources in one file
   - For each resource: current table + history table
   - Search parameter columns (extracted from JSON)
   - Indexes (GIN for arrays, BTREE for singles)
   - All resources deployed together

2. **Rust Model** (per resource: `src/models/{resource}.rs`)
   - Struct with FHIR fields
   - Serde serialization
   - Type conversions

3. **Repository** (per resource: `src/repository/{resource}.rs`)
   - CRUD methods using sqlx
   - Search with dynamic query building
   - History operations
   - Helper methods (find_by_reference, etc.)

4. **Handler** (per resource: `src/api/handlers/{resource}.rs`)
   - Axum route handlers
   - Content negotiation (JSON/XML/HTML)
   - Bundle building
   - Pagination links

5. **Search Extractor** (per resource: `src/models/{resource}.rs`)
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

### Learning DSSSL: Start with a Dumb Template

When building a new DSSSL generator from scratch, start with a **dumb template** approach:

1. **Copy existing output** - Take the exact file you want to generate (e.g., `migrations/001_initial_schema.sql`)
2. **Paste into string literal** - Put it between `($"...")` in your DSSSL function
3. **Escape the content** - Apply three transformations:
   - Replace `\` with `\\` (escape backslashes)
   - Replace `"` with `\"` (escape double quotes)
   - Replace `<` with `<""` (escape angle brackets for OpenJade)
4. **Verify generation** - Run OpenJade and confirm it produces identical output
5. **Then make it dynamic** - Incrementally replace hardcoded parts with XML-driven logic

**Why this works:**
- Proves your file generation plumbing works
- Gives you a working baseline to diff against
- Makes debugging easier (you can see exactly what changed)
- Reduces cognitive load (syntax escaping separate from logic)

**Example workflow:**
```scheme
; Step 1: Dumb template (hardcoded SQL for all 3 resources)
(define (migration)
  (file "migrations/001_initial_schema.sql"
        ($"CREATE TABLE patient (...);")))

; Step 2: Keep only most feature-rich resource (Observation)
; Step 3: Make dynamic with resource iteration
; Step 4: Make columns dynamic from XML search parameters
```

#### Step 1: Choose the Most Feature-Rich Resource as Template

After creating the dumb template with hardcoded SQL, analyze which resource has the most diverse features to use as the template:

```bash
# Count search parameters per resource
Patient: 9 search parameters (family, given, identifier, birthdate, gender, active, etc.)
Observation: 18 search parameters (status, category, code, subject, value, effective, etc.)
Practitioner: 8 search parameters (family, given, identifier, telecom, active, etc.)

# Decision: Keep Observation (most feature-rich)
```

**Why keep the most feature-rich resource:**
- If template handles Observation (complex), it can handle simpler resources
- Tests all feature types: multiple arrays, dates, references, tokens
- Reduces risk of missing edge cases
- Observation has twice as many search params as the others

**Action**: Remove Patient and Practitioner sections from the dumb template, keeping only Observation.

```scheme
(define (migration)
  (file "migrations/001_initial_schema.sql"
        ($"-- Create observation current table
CREATE TABLE observation (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    status TEXT,
    category_system TEXT[],
    category_code TEXT[],
    code_system TEXT,
    code_code TEXT,
    subject_reference TEXT,
    -- ... all observation columns ...
);

-- Create indexes for current table
CREATE INDEX idx_observation_status ON observation (status);
CREATE INDEX idx_observation_category_code ON observation USING GIN (category_code);
-- ... all observation indexes ...

-- Create observation history table
CREATE TABLE observation_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    status TEXT,
    category_system TEXT[],
    -- ... all observation columns ...

    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_observation_history_id ON observation_history (id);
-- ... all history indexes ...
")))
```

#### Step 2: Make Table Names Dynamic and Iterate Over Resources

Replace all hardcoded "observation" strings with a `table-name` variable and wrap in resource iteration:

```scheme
(define (migration)
  (file "migrations/001_initial_schema.sql"
        (for-active-resources (lambda (resource)
          (let ((table-name (downcase-string (name-of resource))))
            ($"-- Create "table-name" current table
CREATE TABLE "table-name" (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
    status TEXT,
    category_system TEXT[],
    -- ... still hardcoded observation columns ...
);

-- Create indexes for current table
CREATE INDEX idx_"table-name"_status ON "table-name" (status);
CREATE INDEX idx_"table-name"_category_code ON "table-name" USING GIN (category_code);
-- ... all indexes now use dynamic table-name ...

-- Create "table-name" history table
CREATE TABLE "table-name"_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
    status TEXT,
    category_system TEXT[],
    -- ... still hardcoded observation columns ...

    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_"table-name"_history_id ON "table-name"_history (id);
-- ... all indexes now use dynamic table-name ...

"))))))
```

**Key points:**
- Use `for-active-resources` to iterate over resources with `active="true"`
- Use inline lambda to maintain template character
- Use inverted string pattern: `"text"table-name"more-text"` (no spaces around variables)
- All "observation" strings replaced with `table-name` variable

**Test:**
```bash
./fire.sh
grep -E '^-- Create [a-z]+ current table' migrations/001_initial_schema.sql
# Output: observation, patient, practitioner (all 3 resources generated!)
```

#### Step 3: Make Columns Dynamic from Search Parameters

Replace hardcoded column definitions with dynamic generation based on each resource's search parameters from XML:

```scheme
(define (migration)
  (file "migrations/001_initial_schema.sql"
        (for-active-resources (lambda (resource)
          (let ((table-name (downcase-string (name-of resource)))
                (searches (select-children "searches" resource)))
            ($"-- Create "table-name" current table
CREATE TABLE "table-name" (
    id TEXT PRIMARY KEY,
    version_id INTEGER NOT NULL DEFAULT 1,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    content JSONB NOT NULL,

    -- Extracted search parameters (indexed)
"(for-selected-children-of searches "search" (lambda (search)
    (let ((search-name (% "name" search))
          (search-type (% "type" search)))
      (if (not (string=? "_lastUpdated" search-name))
          (case search-type
            (("string")
              ($"    "(string-replace search-name "-" "_")"_name TEXT[],
"))
            (("token")
              ($"    "(string-replace search-name "-" "_")"_system TEXT[],
    "(string-replace search-name "-" "_")"_value TEXT[],
"))
            (("date")
              ($"    "(string-replace search-name "-" "_")" DATE,
"))
            (("reference")
              ($"    "(string-replace search-name "-" "_")"_reference TEXT[] DEFAULT '{}',
"))
            (else ""))
          ""))))
");

-- Create indexes for current table
CREATE INDEX idx_"table-name"_last_updated ON "table-name" (last_updated);

-- Create GIN index for JSONB content
CREATE INDEX idx_"table-name"_content ON "table-name" USING GIN (content);

-- Create "table-name" history table
CREATE TABLE "table-name"_history (
    id TEXT NOT NULL,
    version_id INTEGER NOT NULL,
    last_updated TIMESTAMPTZ NOT NULL,
    content JSONB NOT NULL,

    -- Same search parameters as current
"(for-selected-children-of searches "search" (lambda (search)
    (let ((search-name (% "name" search))
          (search-type (% "type" search)))
      (if (not (string=? "_lastUpdated" search-name))
          (case search-type
            (("string")
              ($"    "(string-replace search-name "-" "_")"_name TEXT[],
"))
            (("token")
              ($"    "(string-replace search-name "-" "_")"_system TEXT[],
    "(string-replace search-name "-" "_")"_value TEXT[],
"))
            (("date")
              ($"    "(string-replace search-name "-" "_")" DATE,
"))
            (("reference")
              ($"    "(string-replace search-name "-" "_")"_reference TEXT[] DEFAULT '{}',
"))
            (else ""))
          ""))))
"
    -- History metadata
    history_operation VARCHAR(10) NOT NULL,
    history_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (id, version_id)
);

-- Create indexes for history table
CREATE INDEX idx_"table-name"_history_id ON "table-name"_history (id);
CREATE INDEX idx_"table-name"_history_timestamp ON "table-name"_history (history_timestamp);
CREATE INDEX idx_"table-name"_history_last_updated ON "table-name"_history (last_updated);

-- Create GIN index for JSONB content in history
CREATE INDEX idx_"table-name"_history_content ON "table-name"_history USING GIN (content);

"))))))
```

**Search Type to Column Mapping:**

| Search Type | Column Pattern | Example |
|-------------|----------------|---------|
| `string` | `{name}_name TEXT[]` | `family` → `family_name TEXT[]` |
| `token` | `{name}_system TEXT[]`<br>`{name}_value TEXT[]` | `identifier` → `identifier_system TEXT[]`<br>`identifier_value TEXT[]` |
| `date` | `{name} DATE` | `birthdate` → `birthdate DATE` |
| `reference` | `{name}_reference TEXT[] DEFAULT '{}'` | `general-practitioner` → `general_practitioner_reference TEXT[] DEFAULT '{}'` |

**Key techniques:**
- **Nested lambdas** - Outer for resources, inner for search parameters
- **Conditional logic** - Skip `_lastUpdated` (handled by `last_updated` column)
- **String transformation** - `string-replace` converts hyphens to underscores
- **Case statement** - Pattern match on search type to generate appropriate columns
- **Inverted string pattern** - Maintained throughout with no spaces
- **Template character** - All logic inline, no extracted helper functions

**Test:**
```bash
./fire.sh
head -40 migrations/001_initial_schema.sql

# Output shows dynamic columns:
# Patient gets: active_system/value, family_name, given_name, birthdate, etc.
# Observation gets: code_system/value, status_system/value, date, etc.
# Each resource has columns matching its search parameters!
```

**Current State:**
- ✅ Columns generate dynamically from XML search parameters
- ✅ Each resource gets its own unique column set
- ✅ Type-based column generation works (string, token, date, reference)
- ❌ Column names don't perfectly match actual migration yet (refinement needed)
- ❌ Indexes still minimal (need dynamic generation per column)

**Next steps:**
- Refine column naming rules to match actual extractor implementations
- Generate indexes dynamically based on column types
- Handle special cases (composite searches, quantity searches, etc.)

### Inverted String Pattern

When dynamically building strings in DSSSL, maintain the **"inverted string" pattern** where variable insertions are directly adjacent to string literal boundaries with no spaces:

**Pattern Rule**: `"string-content"variable"more-content"`

The quotes "invert" or "flip" around the variable insertion point, creating a visual rhythm.

**Examples**:

```scheme
;; CORRECT - No spaces around variables
(let ((table-name "observation"))
  ($"CREATE TABLE "table-name" ("))

;; Output: CREATE TABLE observation (

;; WRONG - Spaces break the visual pattern
(let ((table-name "observation"))
  ($"CREATE TABLE " table-name " ("))

;; Still produces same output, but breaks the pattern
```

**Why this pattern matters**:
- **Visual consistency** - Easy to scan and identify variable insertion points
- **Clear boundaries** - Quotes mark exact transition between literal and dynamic content
- **Less ambiguity** - No confusion about where spaces come from (literal vs variable)
- **Proven at scale** - This pattern used in production DSSSL code generators

**More examples**:

```scheme
;; Index names - no spaces around variables
($"CREATE INDEX idx_"table-name"_"column-name" ON "table-name" ("column-name");")

;; Comments with variables
($"-- Create "resource-name" current table
")

;; Column definitions
($"    "column-name" "column-type",
")
```

**When spaces ARE needed**, they're part of the literal string on one side:

```scheme
;; Space AFTER variable (before next word)
($"pub struct "struct-name" {
")
;;                          ^space in literal

;; Space BEFORE variable (after previous word)
($"impl "trait-name" for "struct-name" {
")
;;     ^space in literal    ^space in literal
```

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

### Angular Bracket Escaping in DSSSL

When writing DSSSL code generators that output code containing angular brackets, you need to prevent OpenJade from interpreting them as SGML tags. This affects:
- Generic types (e.g., `Option<String>`, `Vec<T>`)
- Any other code containing `<` characters

**Problem**: OpenJade treats `<` as the start of an SGML tag, causing parsing errors.

**Solution: Double-Quote Escaping**

Within a `($...)` function, write the `<` character followed immediately by two double quotes:

```scheme
($ "Option<""String>")
```

The `<` is followed by `""` which breaks the sequence: the first `"` ends the current string, and the second `"` immediately begins a new string, preventing OpenJade from recognizing `<` as a tag opener.

**Examples**:
```scheme
;; Wrong - will cause OpenJade parsing errors:
($ "pub id: Option<String>")
($ "pub items: Vec<Item>")

;; Correct - escapes the < characters:
($ "pub id: Option<""String>")
($ "pub items: Vec<""Item>")
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

### Phase 2: Code Generation (Current)
- [x] Build `fhir-to-xml` Rust tool
- [x] Generate initial `model/fhir.xml` from FHIR R5 JSONs
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
