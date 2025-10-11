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

### 2. Database: PostgreSQL with JSONB ✅
- JSONB for flexible FHIR resource storage
- ACID transactions (critical for bundles)
- GIN indexes for JSON queries
- Using `sqlx` with compile-time checked queries
- Custom query builder for FHIR search

### 3. Table-Per-Resource Architecture ✅

**Each FHIR resource type gets TWO tables:**
- **Current Table** - Only latest version, heavily indexed, fast searches
- **History Table** - All versions, lighter indexing, archivable

**Benefits:** 22x faster reads, 5x faster searches, separate indexing strategies.

### 4. Search Parameter Extraction ✅
Resources stored as complete JSONB + critical search parameters extracted to dedicated indexed columns.

### 5. Repository Pattern ✅
Each resource has: `create()`, `read()`, `read_version()`, `update()`, `delete()`, `search()`, `history()`. All operations use sqlx transactions.

## Current Status

### ✅ Completed
- Project setup with Axum, sqlx, PostgreSQL
- Patient and Observation resources (current + history tables)
- Full CRUD operations
- Search with basic parameters
- History operations and version reading
- Transaction bundles (POST /fhir)
- Capability statement (GET /fhir/metadata)
- Health check endpoints
- Error handling, middleware (logging, request_id)
- FHIR R5 fields (triggeredBy, focus, bodyStructure)

### 🚧 Pending
- Code generation for 140+ FHIR resources
- Authentication/Authorization (SMART on FHIR)
- Rate limiting, audit logging
- Performance optimization, comprehensive tests

## FHIR Operations Reference

### Search Parameter Types
1. **String** - name, address (:contains, :exact)
2. **Token** - identifiers, codes (system|value)
3. **Date** - birthdate, dates (gt, lt, ge, le, eq)
4. **Reference** - patient, subject (Patient/123)
5. **Number** - quantities (with prefixes)
6. **Quantity** - measurements with units
7. **URI** - profile, urls

### Result Parameters
- `_count`, `_offset` - pagination
- `_sort` - sort order
- `_include`, `_revinclude` - referenced resources
- `_summary`, `_elements` - partial responses

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
        // Insert into history table with operation='CREATE'
        tx.commit().await?;
        Ok(resource)
    }

    pub async fn read(&self, id: &Uuid) -> Result<Option<" resource-name ">, FhirError> {
        // Read from current table where deleted = false
    }

    pub async fn update(&self, id: &Uuid, resource: " resource-name ") -> Result<" resource-name ", FhirError> {
        let mut tx = self.pool.begin().await?;
        // Get current version with FOR UPDATE lock
        // Update current table, increment version
        // Insert into history table with operation='UPDATE'
        tx.commit().await?;
        Ok(resource)
    }

    pub async fn delete(&self, id: &Uuid) -> Result<(), FhirError> {
        let mut tx = self.pool.begin().await?;
        // Mark deleted=true in current table
        // Insert into history table with operation='DELETE'
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

### Production-Proven File Structure

Use this proven modular structure for code generation:

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

# Parse FHIR StructureDefinitions to XML model
echo "Parsing FHIR StructureDefinitions..."
python3 scripts/parse_fhir_definitions.py \
    --input fhir-spec/resources \
    --output model/fhir-resources.xml

# Run OpenJade generator
echo "Generating code with OpenJade..."
openjade -t sgml -d codegen/map.dsl model/fhir-resources.xml

echo "Generation complete. Files created in src/, migrations/, etc."
```

**Windows (generate.bat):**
```batch
@echo off
setlocal

REM OpenJade paths
set JADEPATH=C:\Program Files\openjade-1.3.1
set PATH=%PATH%;%JADEPATH%\bin

REM Required environment variables
set SGML_CATALOG_FILES=%CD%\catalog
set SP_CHARSET_FIXED=YES
set SP_ENCODING=XML

REM Parse and generate
python scripts\parse_fhir_definitions.py ^
    --input fhir-spec\resources ^
    --output model\fhir-resources.xml

openjade -t sgml -d codegen\map.dsl model\fhir-resources.xml
```

**Environment Variable Reference:**
- **SGML_CATALOG_FILES** - Path to catalog file (required)
- **SP_CHARSET_FIXED** - Set to "YES" to fix character encoding issues
- **SP_ENCODING** - Set to "XML" for XML input processing
- **JADEPATH** - Path to OpenJade installation

#### Installation

**Linux:**
```bash
# Ubuntu/Debian
sudo apt-get install openjade

# Fedora/RHEL
sudo dnf install openjade
```

**macOS:**
```bash
# MacPorts
sudo port install openjade
```

**Windows:**
- Download precompiled binaries from OpenJade community sources
- Or compile from source: https://sourceforge.net/projects/openjade/

#### Usage Workflow

```bash
# 1. Make executable (Unix)
chmod +x generate.sh

# 2. Run generation
./generate.sh

# 3. Review generated files
git diff src/ migrations/

# 4. Run tests
cargo test

# 5. Format and commit
cargo fmt
git add src/ migrations/
git commit -m "Generate code for FHIR resources"
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

4. **Modular Generator Organization** - See file structure above

5. **Test Generation**
   - Generate for a few resources manually
   - Verify output matches expectations
   - Then generate all 140+ resources

6. **Handle FHIR Complexity**
   - Choice types (value[x]) → multiple fields
   - Cardinality (0..1 vs 0..*) → Option vs Vec
   - References → special handling for Resource/id format
   - Search parameters → extract to columns + build indexes

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

## Testing with pyrtest

**Fire uses [pyrtest](https://github.com/rschleitzer/pyrtest)** - a comprehensive Python-based black-box HTTP API test suite for validating FHIR R5 server implementations.

### Why pyrtest?

- **Separation of concerns** - Tests are decoupled from implementation
- **Language-agnostic** - Tests HTTP API, works with any FHIR server
- **Comprehensive** - Covers CRUD, search, bundles, history, pagination, conditional operations
- **Black-box testing** - Validates external behavior, not internal implementation
- **Reusable** - Same test suite can validate multiple FHIR server implementations

### Running pyrtest against Fire

1. **Start the Fire FHIR server**:
```bash
# Terminal 1 - Start Fire
cd ~/repos/fire
cargo run
```

2. **Run pyrtest in a separate terminal**:
```bash
# Terminal 2 - Run tests
cd ~/repos/pyrtest
export FHIR_BASE_URL=http://localhost:3000/fhir
pytest -v
```

3. **Run specific test categories**:
```bash
# Patient CRUD tests
pytest tests/test_patient_crud.py -v

# Search tests
pytest tests/test_patient_search.py tests/test_patient_search_advanced.py -v

# Bundle/transaction tests
pytest tests/test_bundles.py -v

# All tests matching a pattern
pytest -k "search" -v
```

### Test Coverage

pyrtest validates:
- ✅ **CRUD Operations** - Create, Read, Update, Delete
- ✅ **Search** - String, token, date, reference parameters
- ✅ **Search Modifiers** - `:exact`, `:contains`, `:missing`
- ✅ **Search Prefixes** - Date/number comparisons (gt, lt, ge, le)
- ✅ **Result Parameters** - `_count`, `_sort`, `_total`, `_include`, `_revinclude`
- ✅ **Bundles** - Transaction and batch bundles
- ✅ **History** - Resource versioning and history retrieval
- ✅ **Conditional Operations** - Conditional create, update, delete
- ✅ **Pagination** - Next/previous links, page navigation
- ✅ **Error Handling** - Validation and proper error responses

### Integration with CI/CD

Add pyrtest to your CI pipeline:

```yaml
# .github/workflows/test.yml
name: Fire FHIR Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:14
        env:
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: fhir_test
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    steps:
      - uses: actions/checkout@v3

      # Checkout pyrtest
      - name: Checkout pyrtest
        uses: actions/checkout@v3
        with:
          repository: rschleitzer/pyrtest
          path: pyrtest

      # Build and start Fire
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Build Fire
        run: cargo build --release
      - name: Start Fire in background
        run: |
          export DATABASE_URL=postgres://postgres:postgres@localhost/fhir_test
          cargo run --release &
          sleep 5  # Wait for server to start

      # Run pyrtest
      - uses: actions/setup-python@v4
        with:
          python-version: '3.11'
      - name: Install pyrtest dependencies
        run: |
          cd pyrtest
          pip install -r requirements.txt
      - name: Run pyrtest
        run: |
          cd pyrtest
          export FHIR_BASE_URL=http://localhost:3000/fhir
          pytest -v
```

### Development Workflow

1. **Implement feature in Fire** (Rust)
2. **Run pyrtest** to validate compliance
3. **Fix issues** until tests pass
4. **Commit** when all tests green

This ensures Fire maintains FHIR R5 compliance throughout development.

## Notes for Claude Code

- Always use `sqlx::query!` or `sqlx::query_as!` for type-safe queries
- Use transactions for multi-step database operations
- Extract search parameters into dedicated columns for performance
- Current table queries should never filter by version_id (always latest)
- History queries work with history table
- FHIR search is complex - embrace it, don't oversimplify
- Performance matters - this is healthcare data at scale
- Type safety is critical - leverage Rust's type system
- **Test with pyrtest** - Run black-box HTTP tests to validate FHIR compliance

## Open Questions (For Future Phases)

1. How to handle custom search parameters defined in profiles?
2. Subscription mechanism for real-time updates?
3. GraphQL interface in addition to REST?
4. Multi-tenancy strategy (if needed)?
5. Elasticsearch integration for advanced full-text search?

---

**This document is the source of truth for architectural decisions. Update it as the project evolves.**
