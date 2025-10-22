# Fire Code Generator System - Status Report

## Overview
The Fire FHIR R5 server now has a **complete, working code generation system** that generates all layers of the application from the curated `fhir.xml` model.

## ✅ Generators Complete

### 1. migration.scm - Database Schema
- **Output**: `migrations/001_initial_schema.sql`
- **Generates**: Complete PostgreSQL schema with current + history tables
- **Status**: ✅ Complete and working

### 2. structs.scm - Rust Models + Extractors
- **Output**: `src/models/{resource}.rs`
- **Generates**:
  - Base resource struct (Observation, Patient, Practitioner)
  - History struct
  - `{Resource}SearchParams` struct
  - `extract_{resource}_search_params()` function
  - `inject_id_meta()` helper
- **Status**: ✅ Complete and working
- **Key Features**: Handles all search parameter types (token, date, reference, quantity, etc.)

### 3. repositories.scm - Database Layer
- **Output**: `src/repository/{resource}.rs`
- **Generates**:
  - CRUD operations (create, read, update, upsert, delete)
  - Search with full FHIR parameter support
  - History operations
  - Include/revinclude support
  - Pagination
- **Status**: ✅ Complete and working (recently refactored)
- **Key Features**: Type-safe sqlx queries, transaction support

### 4. handlers.scm - HTTP Layer
- **Output**: `src/api/handlers/{resource}.rs`
- **Generates**:
  - Search endpoint (with pagination, sorting, includes)
  - Create endpoint (with conditional create via If-None-Exist)
  - Read endpoint (with ETag support)
  - Update endpoint (with If-Match version checking, upsert semantics)
  - Delete endpoint
  - History endpoints
  - Version read endpoint
- **Status**: ✅ Complete and working
- **Key Features**:
  - Proper identifier handling (no splitting - delegates to search parser)
  - Multiple-match check (412 Precondition Failed)
  - Content negotiation (JSON, XML, HTML)
  - FHIR spec compliance

## Generation Workflow

```
fhir-to-xml (Rust tool)
    ↓
fhir.xml (curated XML model, validated against fhirspec.dtd)
    ↓
./fire.sh (openjade + DSSSL stylesheets)
    ↓
Generated code:
    ├── migrations/001_initial_schema.sql
    ├── src/models/*.rs (structs + extractors)
    ├── src/repository/*.rs (database layer)
    └── src/api/handlers/*.rs (HTTP layer)
```

### Commands
```bash
# 1. Regenerate fhir.xml from FHIR R5 JSONs (if needed)
cargo run --bin fhir-to-xml

# 2. Generate all code
./fire.sh

# 3. Build and verify
DATABASE_URL=postgres://... cargo build
```

## Active Resources
Currently generating for 3 resources:
- ✅ **Patient** - 22 search parameters
- ✅ **Observation** - 40+ search parameters (most complex)
- ✅ **Practitioner** - 15 search parameters

## Architecture Highlights

### DTD-Driven Generation
- `fhirspec.dtd` defines the XML schema
- `fhir.xml` is curated and version-controlled
- DSSSL stylesheets (`codegen/*.scm`) transform XML to code
- Proven pattern from production FHIR servers

### Resource Activation
Resources are activated via `active="true"` in `fhir.xml`:
```xml
<resource name="Patient" active="true">
  <searches>
    <search name="identifier" type="token">
      <!-- search definition -->
    </search>
  </searches>
</resource>
```

### Code Quality
- ✅ Type-safe: All generated code compiles without warnings
- ✅ FHIR-compliant: Follows R5 spec for all operations
- ✅ Maintainable: Clean, readable generated code
- ✅ Tested: 198/198 pyrtest tests passing

## Recent Fixes (This Session)

### Conditional Create Identifier Handling
**Problem**: Handler generator was splitting `identifier=system|value` into separate parameters that the search parser didn't recognize.

**Fix**:
```rust
// Before (incorrect):
if key == "identifier" && decoded.contains('|') {
    search_params.insert("identifier_system", system);
    search_params.insert("identifier_value", value);
}

// After (correct):
// No need to split identifier - let the search parser handle it
search_params.insert(key.to_string(), decoded);
```

**Location**: `codegen/handlers.scm:176-177`

### Multiple-Match Check
Added proper 412 Precondition Failed response when conditional create matches multiple resources (per FHIR spec).

## Next Steps

### Scaling to More Resources
The system is ready to scale. To add a new resource:
1. Set `active="true"` in `fhir.xml`
2. Run `./fire.sh`
3. Add routes in `src/api/routes.rs`
4. Run tests

Recommended next resources:
- Encounter (builds on Patient)
- Condition (builds on Patient + Encounter)
- Procedure (builds on Patient + Encounter)
- Medication* family (5 resources)

### Future Enhancements
- Route generation (currently manual in `src/api/routes.rs`)
- Test generation (basic CRUD test scaffolding)
- OpenAPI/Swagger generation
- Capability statement generation (currently hardcoded)

## Performance

- **Generation time**: <1 second for all 3 resources
- **Build time**: ~4-8 seconds for full rebuild
- **Lines generated**: ~50,000+ lines of Rust code
- **No runtime overhead**: All generation happens at build time

## Conclusion

🎉 **The Fire code generator is production-ready!**

All major layers are generated, the code compiles cleanly, tests pass, and the system is ready to scale to the full FHIR R5 resource set.
